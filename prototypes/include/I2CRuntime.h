#ifndef I2CRUNTIME_H_
#define I2CRUNTIME_H_

#include <vector>
#include <Wire.h>

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
};

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
};

class I2CRuntime {
    private:
        std::vector<I2CPeripheralManager *> mManagers;
        TwoWire * mWire;

    public:
        I2CRuntime(TwoWire *);

        std::size_t addPeripheral(Peripheral *peripheral);
};

/*
void prototypeOutputRead(
    TwoWire *wire,
    uint16_t busAddr,
    OutputRead *conf,
    uint8_t *bytes
);

void prototype(TwoWire *wire, Peripheral *peripheral, uint8_t **outputs);
*/

#endif