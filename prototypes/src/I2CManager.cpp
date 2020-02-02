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
        this->printReport(&Serial);
        return;
    }

    if (mDidTransmit) {
        mWire->requestFrom(mCurPollingAddress, 1u);
        bool status = mWire->available() == 1;
        mAddressStatus[mCurPollingAddress] = status;
        if (status) {
            // Anything the device sent back won't be relevant and we want to avoid
            // reading it in the future.
            mWire->read();
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