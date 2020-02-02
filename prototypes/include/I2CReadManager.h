#ifndef I2CREAD_MANAGER_H_
#define I2CREAD_MANAGER_H_

#include <Wire.h>

#include "I2CPeripheral.h"
#include "Scheduler.h"

enum ReadManagerState {
    NOT_READING_BLOCK,
    REQUESTING_SINGLE_READ,
    REQUESTED_SINGLE_READ,
};

class I2CReadManager {
    private:
        ReadDefinition *mDefinition;
        Peripheral *mPeripheral;
        Scheduler mScheduler;
        ScheduleId mInterReadScheduleId;
        ScheduleId mIntraReadScheduleId;
        uint8_t * mBuffer;
        ReadManagerState mState;
        uint16_t mCursor;
        TwoWire * mWire;

        void startBlockRead();
        void finishBlockRead();
        void requestReadAtCursor();
        void readAtCursor();
        void advanceCursor();
        void read();

    public:
        I2CReadManager(ReadDefinition *, Peripheral *, uint8_t *, TwoWire *);
        void loop();
        bool isWriting();
};

#endif