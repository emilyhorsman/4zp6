#ifndef I2CRUNTIME_H_
#define I2CRUNTIME_H_

#include <vector>
#include <Wire.h>

#include "I2CPeripheral.h"
#include "I2CReadManager.h"
#include "Scheduler.h"
#include "TelemetryProtocol.h"

class I2CRuntime;

/**
 * @brief Manages all the read managers and memory allocation for a single peripheral.
 *
 * Only intended to be used by an I2CRuntime instance.
 *
 * A peripheral manager will manage the read managers for each read definition
 * on a single peripheral.
 */
class I2CPeripheralManager {
    private:
        Peripheral * mPeripheral;
        uint8_t ** mBuffer;
        std::vector<I2CReadManager *> mReadManagers;
        TwoWire * mWire;
        I2CRuntime *mRuntime;

        static uint8_t ** allocateBytes(Peripheral *);
        static void deallocateBytes(Peripheral *, uint8_t **);

    public:
        I2CPeripheralManager(Peripheral *peripheral, TwoWire *wire, I2CRuntime *runtime);
        ~I2CPeripheralManager();
        void loop();
        uint8_t ** getBuffer();
        uint8_t getBusAddr();
};

/**
 * @brief Responsible for the primary event loop for all peripherals on the bus.
 */
class I2CRuntime {
    private:
        std::vector<I2CPeripheralManager *> mManagers;
        TwoWire * mWire;

    public:
        I2CRuntime(TwoWire *);
        std::size_t addPeripheral(Peripheral *peripheral);
        bool hasPeripheral(std::size_t);
        uint8_t ** getPeripheralBuffer(std::size_t);
        void loop();
        void setPayloadFunc(std::shared_ptr<PayloadFunc>);

        std::shared_ptr<PayloadFunc> mPayloadFunc;
};

#endif
