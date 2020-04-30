#ifndef SCHEDULER_H_
#define SCHEDULER_H_

#include <functional>
#include <memory>
#include <vector>

#define HAS_NEVER_RAN_TIMESTAMP 0

// Some aliases for semantic richness/readability.
typedef uint32_t Milli;
typedef Milli Duration;
typedef Milli Timestamp;

typedef std::function<void ()> Func;
typedef std::size_t ScheduleId;


/**
 * @brief Declarative configuration for a managed schedule.
 */
struct Schedule {
    std::shared_ptr<Func> f;
    /**
     * How many milliseconds between executions of `f`?
     */
    Duration period;
    /**
     * When was the previous call of `f`? If `f` has never been called then
     * this will be `HAS_NEVER_RAN_TIMESTAMP`.
     */
    Timestamp previousCall;
    bool isEnabled;

    void callAndUpdate();
};

/**
 * @brief Generic scheduler for non-blocking tasks given schedules for callbacks.
 */
class Scheduler {
    private:
        std::vector<Schedule> mSchedules;
        std::vector<Schedule>::iterator mCursor;

        void advanceCursor();
        bool canExecuteCursor();

    public:
        ScheduleId addSchedule(
            std::shared_ptr<Func>,
            Duration,
            bool isEnabled = true
        );
        void disableSchedule(ScheduleId);
        void enableSchedule(ScheduleId);
        /**
         * Update the schedule such that it will run after its period relative
         * to the call of `kickSchedule`. e.g., if `kickSchedule` is called at
         * t = 40 with a period of 500 then the schedule will be executed at
         * t = 540.
         */
        void kickSchedule(ScheduleId);
        void loop();
};

#endif
