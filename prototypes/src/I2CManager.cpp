#include <Arduino.h>
#include <functional>
#include <memory>

#include "I2CManager.h"
#include "Scheduler.h"

I2CManager::I2CManager(TwoWire *wire, Duration interScanPeriod, Duration intraScanPeriod)
: mAddressStatus()
, mScheduler()
, mInterScanScheduleId(0)
, mIntraScanScheduleId(0)
, mCurPollingAddress(0)
, mWire(wire)
, mDidTransmit(false)
, mOnChangeCallback(NULL)
{
    mInterScanScheduleId = mScheduler.addSchedule(
        std::make_shared<Func>(
            [this]() {
                mScheduler.disableSchedule(mInterScanScheduleId);
                mScheduler.enableSchedule(mIntraScanScheduleId);
            }
        ),
        interScanPeriod
    );

    mIntraScanScheduleId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&I2CManager::poll, this)),
        intraScanPeriod,
        false
    );
}

void I2CManager::loop()
{
    mScheduler.loop();
}

void I2CManager::poll()
{
    // If we've reached the end then we should switch over the inter-scanning
    // period.
    if (mCurPollingAddress == 128) {
        mCurPollingAddress = 0;
        mScheduler.kickSchedule(mInterScanScheduleId);
        mScheduler.disableSchedule(mIntraScanScheduleId);
        mScheduler.enableSchedule(mInterScanScheduleId);
        return;
    }

    if (mDidTransmit) {
        mWire->requestFrom(mCurPollingAddress, 1u);
        bool status = mCurPollingAddress == 0x44 || mWire->available() == 1;
        bool old = mAddressStatus[mCurPollingAddress];
        mAddressStatus[mCurPollingAddress] = status;
        if (status) {
            // Anything the device sent back won't be relevant and we want to avoid
            // reading it in the future.
            mWire->read();
        }

        if (status != old && mOnChangeCallback != NULL) {
            (*mOnChangeCallback)(mCurPollingAddress);
        }

        mCurPollingAddress++;

        // Some peripherals need a delay between the initial transmission and
        // the requestFrom/read.
        mDidTransmit = false;
        return;
    }

    // Just send the START/END sequence to the address and see if it gives
    // anything back.
    mWire->beginTransmission(mCurPollingAddress);
    mWire->write(0x0);
    mWire->endTransmission();
    mDidTransmit = true;
}

void I2CManager::printReport(Stream *stream)
{
    stream->printf("Connected Devices: %d\n", mAddressStatus.count());
}


void I2CManager::setCallback(std::shared_ptr<std::function<void(uint8_t)>> f) {
    mOnChangeCallback = f;
}


bool I2CManager::isConnected(uint8_t busAddr) {
    if (busAddr > 127) {
        return false;
    }
    return mAddressStatus[busAddr];
}
