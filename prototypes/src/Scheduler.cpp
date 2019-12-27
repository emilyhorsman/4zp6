#include "Scheduler.h"

Scheduler::Scheduler()
: mFuncs()
, mLastCallTimeMilli()
, mIntervalTimeMilli()
, mCursor(0)
{}

void Scheduler::loop() {

}

std::size_t Scheduler::addSchedule(
    std::function<void ()> *func,
    uint32_t interval
) {
    mFuncs.push_back(func);
    mLastCallTimeMilli.push_back(0);
    mIntervalTimeMilli.push_back(interval);
    return mFuncs.size() - 1;
}

void Scheduler::removeSchedule(std::size_t id) {
    mFuncs.erase(mFuncs.begin() + id);
    mLastCallTimeMilli.erase(mLastCallTimeMilli.begin() + id);
    mIntervalTimeMilli.erase(mIntervalTimeMilli.begin() + id);
}