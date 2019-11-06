#include "Arduino.h"

#define REGISTER_REQ_DELAY_MILLI 20
#define PERIPHERAL_CONF_DELAY_MILLI 1000

struct OutputRead {
    uint16_t outputId;
    uint16_t registerId;
    // The "no need" behaviour here is a value of 1.
    uint16_t manualAdvanceNum;
    uint8_t numBytesPerAdvance;
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

// Not the way we actually want to do it, this is blocking non-loop code
// Just a proof of concept for the runtime struct
void prototypeOutputRead(
    TwoWire *wire,
    uint16_t busAddr,
    OutputRead *conf
) {
    uint8_t *bytes = new uint8_t[conf->numBytesPerAdvance * conf->manualAdvanceNum];
    for (uint16_t i = 0; i < conf->manualAdvanceNum; i++) {
        wire->beginTransmission(busAddr);
        wire->write(conf->registerId + i);
        wire->endTransmission();
        delay(REGISTER_REQ_DELAY_MILLI);
        wire->requestFrom(busAddr, conf->numBytesPerAdvance);

        for (uint8_t j = 0; j < conf->numBytesPerAdvance; j++) {
            bytes[i * conf->numBytesPerAdvance + j] = wire->read();
        }
    }
}

void prototype(TwoWire *wire, Peripheral *peripheral) {
    wire->beginTransmission(peripheral->busAddr);
    for (uint8_t i = 0; i < numInitialWriteBytes; i++) {
        wire->write(peripheral->initialWrite[i]);
    }
    wire->endTransission();
    delay(PERIPHERAL_CONF_DELAY_MILLI);

    for (uint8_t i = 0; i < peripheral->numOutputs; i++) {
        prototypeOutputRead(wire, peripheral->busAddr, peripheral->outputs[i]);
    }
}
