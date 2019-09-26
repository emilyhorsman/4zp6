#include <Arduino.h>
#include "I2CManager.h"

I2CManager::I2CManager(TwoWire *wire, uint32_t pollingInterval, uint32_t scanInterval)
: mAddressStatus()
, mLastPollTimeMilli(0)
, mPollingIntervalMilli(pollingInterval)
, mScanIntervalMilli(scanInterval)
, mCurPollingAddress(0)
, mWire(wire)
{}

void I2CManager::loop()
{
    if (this->shouldPoll()) {
        this->poll();
    }
}

bool I2CManager::shouldPoll()
{
    uint32_t time = millis();
    if (mCurPollingAddress > 127) {
        return (
            mLastPollTimeMilli == 0 ||
            time - mPollingIntervalMilli >= mLastPollTimeMilli
        );
    }

    return time - mScanIntervalMilli >= mLastPollTimeMilli;
}

void I2CManager::poll()
{
    if (mCurPollingAddress == 128) {
        mCurPollingAddress = 0;
        this->printReport(&Serial);
    }

    // Just send the START/END sequence to the address and see if it gives
    // anything back.
    mWire->beginTransmission(mCurPollingAddress);
    mWire->endTransmission();
    mWire->requestFrom(mCurPollingAddress, 1u);
    bool status = mWire->available() == 1;
    mAddressStatus[mCurPollingAddress] = status;
    if (status) {
        // Anything the device sent back won't be relevant and we want to avoid
        // reading it in the future.
        mWire->read();
    }

    mLastPollTimeMilli = millis();
    mCurPollingAddress++;
}

void I2CManager::printReport(Stream *stream)
{
    stream->printf("Connected Devices: %d\n", mAddressStatus.count());
}