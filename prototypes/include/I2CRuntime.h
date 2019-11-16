#define REGISTER_REQ_DELAY_MILLI 20
#define PERIPHERAL_CONF_DELAY_MILLI 1000

enum RegisterLength {
    RL16,
    RL8,
};

struct OutputRead {
    // An arbitrary ID for external reference. This does not relate to the
    // hardware.
    uint16_t outputId;

    // The first register we are requesting data from.
    uint16_t registerId;

    RegisterLength registerIdLength;

    // The "no need" behaviour here is a value of 1. This is how many times
    // we will read and increment, starting at registerId.
    uint16_t manualAdvanceNum;

    // The number of bytes we will read per advance.
    uint8_t numBytesPerAdvance;

    // How often will we get data from the registers?
    uint16_t pollIntervalMilli;
};

typedef struct OutputRead OutputRead;

struct Peripheral {
    uint16_t busAddr;
    uint8_t numOutputs;
    OutputRead * outputs;
    uint8_t * initialWrite;
    uint8_t numInitialWriteBytes;
};

typedef struct Peripheral Peripheral;

void prototypeOutputRead(
    TwoWire *wire,
    uint16_t busAddr,
    OutputRead *conf,
    uint8_t *bytes
);

void prototype(TwoWire *wire, Peripheral *peripheral, uint8_t **outputs);

uint8_t ** allocateOutputBytes(Peripheral *peripheral);
void deallocateOutputBytes(Peripheral *peripheral, uint8_t **bytes);