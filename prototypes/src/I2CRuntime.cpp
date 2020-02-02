#include <Arduino.h>
#include <vector>
#include <Wire.h>

#include "I2CRuntime.h"
#include "Scheduler.h"

uint16_t ReadDefinition::getNumBlockBytes() {
    return registerBlockLength * numBytesPerRegister;
}

I2CReadManager::I2CReadManager(
    ReadDefinition *readDefinition,
    Peripheral *peripheral,
    uint8_t *buffer,
    TwoWire *wire
)
: mDefinition(readDefinition)
, mPeripheral(peripheral)
, mScheduler()
, mInterReadScheduleId(0)
, mIntraReadScheduleId(0)
, mBuffer(buffer)
, mState(NOT_READING_BLOCK)
, mCursor(0)
, mWire(wire)
{
    mInterReadScheduleId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&I2CReadManager::startBlockRead, this)),
        readDefinition->readPeriod
    );

    mIntraReadScheduleId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&I2CReadManager::read, this)),
        REGISTER_REQ_DELAY_MILLI,
        false
    );
}

void I2CReadManager::startBlockRead() {
    mState = REQUESTING_SINGLE_READ;
    mScheduler.disableSchedule(mInterReadScheduleId);
    mScheduler.enableSchedule(mIntraReadScheduleId);
}

void I2CReadManager::finishBlockRead() {
    mScheduler.disableSchedule(mIntraReadScheduleId);
    mState = NOT_READING_BLOCK;

    mScheduler.kickSchedule(mInterReadScheduleId);
    mScheduler.enableSchedule(mInterReadScheduleId);
}

void I2CReadManager::read() {
    // Damn, I love a good state machine here and there.
    assert(mState != NOT_READING_BLOCK);

    if (mState == REQUESTING_SINGLE_READ) {
        this->requestReadAtCursor();
        mState = REQUESTED_SINGLE_READ;
    } else if (mState == REQUESTED_SINGLE_READ) {
        this->readAtCursor();
        this->advanceCursor();
    }
}

void I2CReadManager::advanceCursor() {
    if (mCursor == mDefinition->registerBlockLength - 1) {
        mCursor = 0;
        Serial.println("completed a block read");
        this->finishBlockRead();
        return;
    }

    mCursor++;
    mState = REQUESTING_SINGLE_READ;
}

void I2CReadManager::requestReadAtCursor() {
    assert(mState == REQUESTING_SINGLE_READ);

    Serial.printf("%d transmitting bytes to %x\n", millis(), mPeripheral->busAddress);
    mWire->beginTransmission(mPeripheral->busAddress);
    if (mDefinition->registerIdLength == RL16) {
        uint16_t regId = mDefinition->registerId + mCursor;
        mWire->write(regId >> 8);
        mWire->write(regId & 255);
    } else {
        mWire->write(mDefinition->registerId + mCursor);
    }
    mWire->endTransmission();
}

void I2CReadManager::readAtCursor() {
    assert(mState == REQUESTED_SINGLE_READ);

    mWire->requestFrom(
        mPeripheral->busAddress,
        mDefinition->numBytesPerRegister
    );
    Serial.printf("%d Requested %d from %x\n", millis(), mDefinition->numBytesPerRegister, mPeripheral->busAddress);
    Serial.println(mWire->available());

    // We don't typically want a loop like this in a non-blocking call but
    // I'm fairly certain this is okay since `TwoWire::read` is buffered.
    for (uint8_t i = 0; i < mDefinition->numBytesPerRegister; i++) {
        mBuffer[mCursor * mDefinition->numBytesPerRegister + i] = mWire->read();
    }
}

void I2CReadManager::loop() {
    mScheduler.loop();
}

bool I2CReadManager::isWriting() {
    return mState != NOT_READING_BLOCK;
}

I2CPeripheralManager::I2CPeripheralManager(Peripheral *peripheral, TwoWire *wire)
: mPeripheral(peripheral)
, mBuffer(NULL)
, mReadManagers()
, mWire(wire)
{
    mBuffer = I2CPeripheralManager::allocateBytes(mPeripheral);
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        mReadManagers.push_back(new I2CReadManager(
            peripheral->readDefinitions[i],
            peripheral,
            mBuffer[i],
            mWire
        ));
    }
}

I2CPeripheralManager::~I2CPeripheralManager() {
    I2CPeripheralManager::deallocateBytes(mPeripheral, mBuffer);
}

uint8_t ** I2CPeripheralManager::allocateBytes(Peripheral *peripheral) {
    uint8_t **bytes = new uint8_t * [peripheral->numReadDefinitions];
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        bytes[i] = new uint8_t[peripheral->readDefinitions[i]->getNumBlockBytes()];
    }
    return bytes;
}

void I2CPeripheralManager::deallocateBytes(Peripheral *peripheral, uint8_t **bytes) {
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        delete [] bytes[i];
    }
    delete [] bytes;
}

void I2CPeripheralManager::loop() {
    // We can break this up to use a cursor and call one loop per call if
    // needed.
    for (auto readManager : mReadManagers) {
        readManager->loop();
    }
}

uint8_t ** I2CPeripheralManager::getBuffer() {
    for (auto readManager : mReadManagers) {
        if (readManager->isWriting()) {
            return NULL;
        }
    }

    return mBuffer;
}

I2CRuntime::I2CRuntime(TwoWire *wire)
: mManagers()
, mWire(wire)
{}

std::size_t I2CRuntime::addPeripheral(Peripheral *peripheral) {
    mManagers.push_back(new I2CPeripheralManager(peripheral, mWire));
    return mManagers.size() - 1;
}

void I2CRuntime::loop() {
    // We can break this up to use a cursor and call one loop per call if
    // needed.
    for (auto manager : mManagers) {
        manager->loop();
    }
}

uint8_t ** I2CRuntime::getPeripheralBuffer(std::size_t peripheralId) {
    assert(peripheralId < mManagers.size());
    return mManagers[peripheralId]->getBuffer();
}

/*
// Not the way we actually want to do it, this is blocking non-loop code
// Just a proof of concept for the runtime struct
void prototypeOutputRead(
    TwoWire *wire,
    uint16_t busAddr,
    OutputRead *conf,
    uint8_t *bytes
) {
    std::vector<OutputRead *> outputs;
    outputs.push_back(conf);
    for (uint16_t i = 0; i < conf->manualAdvanceNum; i++) {
        wire->beginTransmission(busAddr);
        if (conf->registerIdLength == RL16) {
            uint16_t regId = conf->registerId + i;
            wire->write(regId >> 8);
            wire->write(regId & 255);
        } else {
            wire->write(conf->registerId + i);
        }
        wire->endTransmission();
        delay(REGISTER_REQ_DELAY_MILLI);
        wire->requestFrom(busAddr, conf->numBytesPerAdvance);

        for (uint8_t j = 0; j < conf->numBytesPerAdvance; j++) {
            bytes[i * conf->numBytesPerAdvance + j] = wire->read();
        }
    }
}

void prototype(TwoWire *wire, Peripheral *peripheral, uint8_t **outputs) {
    wire->beginTransmission(peripheral->busAddr);
    for (uint8_t i = 0; i < peripheral->numInitialWriteBytes; i++) {
        wire->write(peripheral->initialWrite[i]);
    }
    wire->endTransmission();
    delay(PERIPHERAL_CONF_DELAY_MILLI);

    for (uint8_t i = 0; i < peripheral->numOutputs; i++) {
        prototypeOutputRead(wire, peripheral->busAddr, peripheral->outputs + i, outputs[i]);
    }
}
*/