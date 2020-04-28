#ifndef I2CREAD_MANAGER_H_
#define I2CREAD_MANAGER_H_

#include <Wire.h>

#include "I2CPeripheral.h"
#include "Scheduler.h"

enum ReadManagerState {
    /**
     * We haven't started reading a block of register IDs from the peripheral.
     */
    NOT_READING_BLOCK,
    /**
     * We're going to inform the peripheral which register we want to perform a
     * read on.
     */
    REQUESTING_SINGLE_READ,
    /**
     * We're going to request the bytes and read them and then either advance
     * the cursor and go to REQUESTING_SINGLE_READ if the block isn't finished
     * or go to NOT_READING_BLOCK and reset the schedules if the block is
     * finished.
     */
    REQUESTED_SINGLE_READ,
};

class I2CReadManager {
    private:
        ReadDefinition *mDefinition;
        Peripheral *mPeripheral;
        Scheduler mScheduler;
        ScheduleId mInterReadScheduleId;
        ScheduleId mIntraReadScheduleId;
        /**
         * The buffer for the whole peripheral is a 2D array that manages bytes
         * per read definition. A single ReadManager instance only manages a
         * single ReaDefinition. This is the buffer for the single instance.
         */
        uint8_t * mBuffer;
        ReadManagerState mState;
        uint16_t mCursor;
        TwoWire * mWire;
        std::shared_ptr<Func> mOnCompletedRead;

        void startBlockRead();
        void finishBlockRead();
        void requestReadAtCursor();
        void readAtCursor();
        void advanceCursor();
        void read();

    public:
        I2CReadManager(ReadDefinition *, Peripheral *, uint8_t *, TwoWire *, std::shared_ptr<Func>);
        void loop();
        bool isWriting();
};

#endif
