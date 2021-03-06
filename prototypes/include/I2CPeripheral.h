#ifndef I2CPERIPHERAL_H_
#define I2CPERIPHERAL_H_

#include "Scheduler.h"

/**
 * This value can be very important.
 *
 * Too low: Insufficient time between the register ID transmission and a request
 *          to read bytes. No data will be read.
 *
 * Too high: Performance will suffer for read definitions that have a long
 *           contiguous register block length. (e.g., something like the thermal
 *           camera that reads from 128 different registers.)
 */
#define REGISTER_REQ_DELAY_MILLI 15

enum RegisterLength {
    RL16,
    RL8,
};

/**
 * @brief Declarative configuration for reading a contiguous register block from a peripheral on the I2C bus.
 */
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
    uint8_t registerBlockLength;

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

    uint16_t getNumBlockBytes();
};

typedef struct ReadDefinition ReadDefinition;

struct SetupWriteDefinition {
    uint8_t * bytes;
    uint8_t numBytes;
};

typedef struct SetupWriteDefinition SetupWriteDefinition;

/**
 * @brief Declarative configuration for the behaviour of a peripheral on the I2C bus.
 */
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
    ReadDefinition ** readDefinitions;
};

typedef struct Peripheral Peripheral;

#endif
