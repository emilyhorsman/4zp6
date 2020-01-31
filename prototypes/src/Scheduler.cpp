#include <Arduino.h>
#include <functional>

#include "Scheduler.h"

Scheduler::Scheduler()
: mFuncs()
, mLastCallTimeMilli()
, mIntervalTimeMilli()
, mCursor(0)
{}

void Scheduler::loop() {
    if (mFuncs.size() == 0) {
        return;
    }

    if (this->timeHasElapsed()) {
        (*mFuncs[mCursor])();
    }

    mCursor = (mCursor + 1) % mFuncs.size();
}

bool Scheduler::timeHasElapsed() {
    uint32_t time = millis();
    return (
        mLastCallTimeMilli[mCursor] == 0 ||
        time - mIntervalTimeMilli[mCursor] >= mLastCallTimeMilli[mCursor]
    );
}

void Scheduler::addSchedule(
    std::function<void ()> *func,
    uint32_t interval
) {
    mFuncs.push_back(func);
    mLastCallTimeMilli.push_back(0);
    mIntervalTimeMilli.push_back(interval);
}