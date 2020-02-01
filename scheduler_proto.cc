#include <chrono>
#include <functional>
#include <iostream>
#include <memory>
#include <thread>
#include <vector>

#define HAS_NEVER_RAN_TIMESTAMP 0

// Redefine Milli to time_t on the Arduino-compatible code.
typedef unsigned long Milli;
// Some aliases for semantic richness/readability.
typedef Milli Duration;
typedef Milli Timestamp;

typedef std::function<void ()> Func;
typedef size_t ScheduleId;

// Mock the Arduino API. Can basically ignore this.
Milli millis() {
    // This is an abomination.
    return std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    ).count();
}

struct Schedule {
    std::shared_ptr<Func> f;
    Duration period;
    Timestamp previousCall;
    bool isEnabled;

    void callAndUpdate() {
        if (!isEnabled) {
            return;
        }

        previousCall = millis();
        (*f)();
    }
};

class Scheduler {
  private:
    std::vector<Schedule> mSchedules;
    std::vector<Schedule>::iterator mCursor;

  public:
    ScheduleId addSchedule(
        std::shared_ptr<Func> f,
        Duration period
    ) {
        mSchedules.push_back(Schedule {
            f, period, HAS_NEVER_RAN_TIMESTAMP, true
        });

        // This is not ideal: the cursor could be half-way through the
        // container when we add a new schedule. But, `loop` is meant to be
        // executed in little time so we'll advance through everything quickly
        // anyway.
        mCursor = mSchedules.begin();

        return mSchedules.size() - 1;
    }

    void advanceCursor() {
        mCursor++;
        if (mCursor == mSchedules.end()) {
            mCursor = mSchedules.begin();
        }
    }

    bool canExecuteCursor() {
        if (mSchedules.empty() || !mCursor->isEnabled) {
            return false;
        }

        return (
            mCursor->previousCall == HAS_NEVER_RAN_TIMESTAMP ||
            millis() - mCursor->period >= mCursor->previousCall
        );
    }

    void disableSchedule(ScheduleId id) {
        if (id >= mSchedules.size()) {
            return;
        }

        mSchedules[id].isEnabled = false;
    }

    void loop() {
        if (mSchedules.empty()) {
            return;
        }

        if (this->canExecuteCursor()) {
            mCursor->callAndUpdate();
        }

        advanceCursor();
    }
};

int main() {
    Scheduler scheduler;
    ScheduleId a = scheduler.addSchedule(
        std::make_shared<Func>(
            []() { std::cout << "Every second " << millis() << std::endl; }
        ),
        1000
    );

    scheduler.addSchedule(
        std::make_shared<Func>(
            []() { std::cout << "Every 2.5 seconds " << millis() << std::endl; }
        ),
        2500
    );

    scheduler.addSchedule(
        std::make_shared<Func>(
            [&a, &scheduler]() {
                std::cout << "Disabling " << a << std::endl;
                scheduler.disableSchedule(a++);
            }
        ),
        5010
    );


    // Mock the Arduino loop.
    while (true) {
        scheduler.loop();
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
}
