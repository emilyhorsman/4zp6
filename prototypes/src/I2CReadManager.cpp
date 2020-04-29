#include <Arduino.h>
#include <Wire.h>

#include "I2CReadManager.h"
#include "Scheduler.h"

I2CReadManager::I2CReadManager(
    ReadDefinition *readDefinition,
    Peripheral *peripheral,
    uint8_t *buffer,
    TwoWire *wire,
    std::shared_ptr<Func> onCompletedReadCallback
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
, mOnCompletedRead(onCompletedReadCallback)
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

/**
 * We've finished reading a from a contiguous block of register IDs and can
 * disable the intra-read schedule and switch back to the inter-read schedule.
 */
void I2CReadManager::finishBlockRead() {
    mScheduler.disableSchedule(mIntraReadScheduleId);
    mState = NOT_READING_BLOCK;

    mScheduler.kickSchedule(mInterReadScheduleId);
    mScheduler.enableSchedule(mInterReadScheduleId);
    (*mOnCompletedRead)();
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
#ifdef READ_MANAGER_DEBUG
        Serial.printf("%lu Completed a block read, disabling schedules\n", millis());
#endif
        this->finishBlockRead();
        return;
    }

    // A block of register IDs is always contiguous, so we simply increment the
    // cursor.
    mCursor++;
    mState = REQUESTING_SINGLE_READ;
}

void I2CReadManager::requestReadAtCursor() {
    assert(mState == REQUESTING_SINGLE_READ);

#ifdef READ_MANAGER_DEBUG
    Serial.printf("%lu Transmitting bytes to %x\n", millis(), mPeripheral->busAddress);
#endif
    mWire->beginTransmission(mPeripheral->busAddress);
    if (mDefinition->registerIdLength == RL16) {
        uint16_t regId = mDefinition->registerId + mCursor;
#ifdef READ_MANAGER_DEBUG
        Serial.printf("%lu %x %x %x\n", millis(), regId, regId >> 8, regId & 255);
#endif
        mWire->write(regId >> 8);
        mWire->write(regId & 255);
#ifdef READ_MANAGER_DEBUG
        Serial.printf("%lu Error after write? %d %s\n", millis(), mWire->lastError(), mWire->getErrorText(mWire->lastError()));
#endif
    } else {
        mWire->write(mDefinition->registerId + mCursor);
    }
    mWire->endTransmission();

#ifdef READ_MANAGER_DEBUG
    Serial.printf("%lu Error after endTrans? %d %s\n", millis(), mWire->lastError(), mWire->getErrorText(mWire->lastError()));
#endif
}

void I2CReadManager::readAtCursor() {
    assert(mState == REQUESTED_SINGLE_READ);

    mWire->requestFrom(
        mPeripheral->busAddress,
        mDefinition->numBytesPerRegister
    );
#ifdef READ_MANAGER_DEBUG
    Serial.printf("%lu Requested %d from %x, %d available\n", millis(), mDefinition->numBytesPerRegister, mPeripheral->busAddress, mWire->available());
#endif

    // We don't typically want a loop like this in a non-blocking call but
    // I'm fairly certain this is okay since `TwoWire::read` is buffered.
    for (uint8_t i = 0; i < mDefinition->numBytesPerRegister; i++) {
        mBuffer[mCursor * mDefinition->numBytesPerRegister + i] = mWire->read();
#ifdef READ_MANAGER_DEBUG
        Serial.printf("%lu Read byte %d: %x\n", millis(), mCursor * mDefinition->numBytesPerRegister + i, mBuffer[mCursor * mDefinition->numBytesPerRegister + i]);
#endif
    }
}

void I2CReadManager::loop() {
    mScheduler.loop();
}

bool I2CReadManager::isWriting() {
    return mState != NOT_READING_BLOCK;
}
