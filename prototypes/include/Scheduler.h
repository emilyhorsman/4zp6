#ifndef SCHEDULER_H_
#define SCHEDULAR_H_

#include <functional>
#include <vector>

class Scheduler {
    private:
        std::vector<std::function<void ()> *> mFuncs;
        std::vector<uint32_t> mLastCallTimeMilli;
        std::vector<uint32_t> mIntervalTimeMilli;
        std::size_t mCursor;

        bool timeHasElapsed();

    public:
        Scheduler();
        void loop();
        void addSchedule(std::function<void ()> *, uint32_t);
};

#endif