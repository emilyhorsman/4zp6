#ifndef I2CRUNTIME_H_
#define I2CRUNTIME_H_

#include <vector>
#include <Wire.h>

#include "I2CPeripheral.h"
#include "I2CReadManager.h"
#include "Scheduler.h"

/**
 * Only intended to be used by an I2CRuntime instance
 */
class I2CPeripheralManager {
    private:
        Peripheral * mPeripheral;
        uint8_t ** mBuffer;
        std::vector<I2CReadManager *> mReadManagers;
        TwoWire * mWire;

        static uint8_t ** allocateBytes(Peripheral *);
        static void deallocateBytes(Peripheral *, uint8_t **);

    public:
        I2CPeripheralManager(Peripheral *peripheral, TwoWire *wire);
        ~I2CPeripheralManager();
        void loop();
        uint8_t ** getBuffer();
};

class I2CRuntime {
    private:
        std::vector<I2CPeripheralManager *> mManagers;
        TwoWire * mWire;

    public:
        I2CRuntime(TwoWire *);
        std::size_t addPeripheral(Peripheral *peripheral);
        uint8_t ** getPeripheralBuffer(std::size_t);
        void loop();
};

#endif