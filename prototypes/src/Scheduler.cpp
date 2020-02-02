#include <Arduino.h>
#include <functional>
#include <memory>
#include <vector>

#include "Scheduler.h"

void Schedule::callAndUpdate() {
    assert(isEnabled);
    previousCall = millis();
    (*f)();
}

ScheduleId Scheduler::addSchedule(
    std::shared_ptr<Func> f,
    Duration period,
    bool isEnabled
) {
    mSchedules.push_back(Schedule {
        f, period, HAS_NEVER_RAN_TIMESTAMP, isEnabled
    });

    // This is not ideal: the cursor could be half-way through the
    // container when we add a new schedule. But, `loop` is meant to be
    // executed in little time so we'll advance through everything quickly
    // anyway.
    mCursor = mSchedules.begin();

    return mSchedules.size() - 1;
}

void Scheduler::advanceCursor() {
    mCursor++;
    if (mCursor == mSchedules.end()) {
        mCursor = mSchedules.begin();
    }
}

bool Scheduler::canExecuteCursor() {
    if (mSchedules.empty() || !mCursor->isEnabled) {
        return false;
    }

    return (
        mCursor->previousCall == HAS_NEVER_RAN_TIMESTAMP ||
        millis() - mCursor->period >= mCursor->previousCall
    );
}

void Scheduler::disableSchedule(ScheduleId id) {
    if (id >= mSchedules.size()) {
        return;
    }

    mSchedules[id].isEnabled = false;
}

void Scheduler::enableSchedule(ScheduleId id) {
    if (id >= mSchedules.size()) {
        return;
    }

    mSchedules[id].isEnabled = true;
}

void Scheduler::kickSchedule(ScheduleId id) {
    if (id >= mSchedules.size()) {
        return;
    }

    mSchedules[id].previousCall = millis();
}

/**
 * This call should be non-blocking and consume as little CPU time as possible
 * per execution. `mCursor` is used to cycle through the schedules and check
 * one schedule for execution per call. 
 */
void Scheduler::loop() {
    if (this->canExecuteCursor()) {
        mCursor->callAndUpdate();
    }

    advanceCursor();
}