#ifndef SCHEDULER_H_
#define SCHEDULAR_H_

#include <vector>

class Scheduler {
    private:
        std::vector<std::function<void ()> *> mFuncs;
        std::vector<uint32_t> mLastCallTimeMilli;
        std::vector<uint32_t> mIntervalTimeMilli;
        std::size_t mCursor;

    public:
        Scheduler();
        void loop();
        std::size_t addSchedule(std::function<void ()> *, uint32_t);
        void removeSchedule(std::size_t);
};

#endif