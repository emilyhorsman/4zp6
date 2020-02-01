#ifndef I2CRUNTIME_H_
#define I2CRUNTIME_H_

#include <vector>

#include "Scheduler.h"

#define REGISTER_REQ_DELAY_MILLI 20
#define PERIPHERAL_CONF_DELAY_MILLI 1000

enum RegisterLength {
    RL16,
    RL8,
};

struct ReadDefinition {
    /**
     * An arbitrary ID for external reference. This does not relate to the
     * hardware or any application logic in the I2CRuntime. This is used
     * externally to track read definitions.
     */
    uint16_t definitionId;

    /**
     * Some peripherals have 16-bit register IDs and some have 8-bit register
     * IDs.
     */
    RegisterLength registerIdLength;

    /**
     * Data is read on this output from a contiguous block of register IDs.
     * e.g., 0x80 to 0xFF. This is the first register ID of the block.
     */
    uint16_t registerId;

    /**
     * Data is read on this output from a contiguous block of register IDs.
     * e.g., 0x80 to 0xFF. This defines the number of register IDs in the block.
     * This is essentially how many times the loop will read bytes at a register
     * and then advance to the next register. For many peripherals this value is
     * simple 1 (i.e., there is no need to advance).
     */
    uint16_t registerBlockLength;

    /**
     * The number of bytes that will be read at each register ID in the
     * contiguous block. This means that the total number of bytes retrieved
     * from one ReadDefinition instance is:
     * 
     *     numBytesPerRegister * registerBlockLength
     */
    uint8_t numBytesPerRegister;

    /**
     * How many milliseconds between reading all bytes from the block of
     * registers?
     */
    Duration readPeriod;
};

typedef struct ReadDefinition ReadDefinition;

struct SetupWriteDefinition {
    uint8_t * bytes;
    uint8_t numBytes;
};

typedef struct SetupWriteDefinition SetupWriteDefinition;

struct Peripheral {
    /**
     * The address on the I2C bus. There is only one bus address per peripheral.
     * This is not tied to `ReadDefinition`s.
     */
    uint16_t busAddress;
    /**
     * Some peripherals require configuration in the form of bytes sent to the
     * peripheral at startup.
     */
    SetupWriteDefinition * setupWriteDefinition;

    /**
     * A Peripheral can define multiple things to read. e.g., a peripheral may
     * provide accelerometer and gyroscope data at non-contiguous register IDs.
     */
    uint8_t numReadDefinitions;
    ReadDefinition * readDefinitions;
};

typedef struct Peripheral Peripheral;

/**
 * Only intended to be used by an I2CRuntime instance
 */
class I2CPeripheralManager {
    private:
        Peripheral *mPeripheral;
        uint8_t **mBuffer;

    public:
        I2CPeripheralManager(Peripheral *peripheral);
        ~I2CPeripheralManager();
};

class I2CRuntime {
    private:
        std::vector<I2CPeripheralManager *> mManagers;

    public:
        uint8_t addPeripheral(Peripheral *peripheral);
        void removePeripheral(uint8_t peripheralId);
};

void prototypeOutputRead(
    TwoWire *wire,
    uint16_t busAddr,
    OutputRead *conf,
    uint8_t *bytes
);

void prototype(TwoWire *wire, Peripheral *peripheral, uint8_t **outputs);

uint8_t ** allocateOutputBytes(Peripheral *peripheral);
void deallocateOutputBytes(Peripheral *peripheral, uint8_t **bytes);

#endif