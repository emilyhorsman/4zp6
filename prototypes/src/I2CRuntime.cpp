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
        Serial.printf("%lu Completed a block read, disabling schedules\n", millis());
        // TODO
        /*mScheduler.disableSchedule(mInterReadScheduleId);
        mScheduler.disableSchedule(mIntraReadScheduleId);
        mState = NOT_READING_BLOCK;*/
        this->finishBlockRead();
        return;
    }

    mCursor++;
    mState = REQUESTING_SINGLE_READ;
}

void I2CReadManager::requestReadAtCursor() {
    assert(mState == REQUESTING_SINGLE_READ);

    Serial.printf("%lu Transmitting bytes to %x\n", millis(), mPeripheral->busAddress);
    mWire->beginTransmission(mPeripheral->busAddress);
    if (mDefinition->registerIdLength == RL16) {
        uint16_t regId = mDefinition->registerId + mCursor;
        Serial.printf("%lu %x %x %x\n", millis(), regId, regId >> 8, regId & 255);
        mWire->write(regId >> 8);
        mWire->write(regId & 255);
        Serial.printf("%lu Error after write? %d %s\n", millis(), mWire->lastError(), mWire->getErrorText(mWire->lastError()));
    } else {
        mWire->write(mDefinition->registerId + mCursor);
    }
    mWire->endTransmission();
    Serial.printf("%lu Error after endTrans? %d %s\n", millis(), mWire->lastError(), mWire->getErrorText(mWire->lastError()));
}

void I2CReadManager::readAtCursor() {
    assert(mState == REQUESTED_SINGLE_READ);

    mWire->requestFrom(
        mPeripheral->busAddress,
        mDefinition->numBytesPerRegister
    );
    Serial.printf("%lu Requested %d from %x, %d available\n", millis(), mDefinition->numBytesPerRegister, mPeripheral->busAddress, mWire->available());

    // We don't typically want a loop like this in a non-blocking call but
    // I'm fairly certain this is okay since `TwoWire::read` is buffered.
    for (uint8_t i = 0; i < mDefinition->numBytesPerRegister; i++) {
        mBuffer[mCursor * mDefinition->numBytesPerRegister + i] = mWire->read();
        Serial.printf("%lu Read byte %d: %x\n", millis(), mCursor * mDefinition->numBytesPerRegister + i, mBuffer[mCursor * mDefinition->numBytesPerRegister + i]);
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
