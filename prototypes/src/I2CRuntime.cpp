#include <vector>
#include "Arduino.h"
#include "Wire.h"
#include "I2CRuntime.h"

uint16_t ReadDefinition::getNumBlockBytes() {
    return registerBlockLength * numBytesPerRegister;
}

I2CPeripheralManager::I2CPeripheralManager(Peripheral *peripheral)
: mPeripheral(peripheral)
, mBuffer(NULL)
, mIsWriting(false)
{
    mBuffer = I2CPeripheralManager::allocateBytes(mPeripheral);
}

I2CPeripheralManager::~I2CPeripheralManager() {
    I2CPeripheralManager::deallocateBytes(mPeripheral, mBuffer);
}

uint8_t ** I2CPeripheralManager::allocateBytes(Peripheral *peripheral) {
    uint8_t **bytes = new uint8_t * [peripheral->numReadDefinitions];
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        bytes[i] = new uint8_t[peripheral->readDefinitions[i]->getNumBlockBytes()];
    }
    return bytes;
}

void I2CPeripheralManager::deallocateBytes(Peripheral *peripheral, uint8_t **bytes) {
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        delete [] bytes[i];
    }
    delete [] bytes;
}

uint8_t I2CRuntime::addPeripheral(Peripheral *peripheral) {
    mManagers.push_back(new I2CPeripheralManager(peripheral));
    return mManagers.size() - 1;
}

/*
// Not the way we actually want to do it, this is blocking non-loop code
// Just a proof of concept for the runtime struct
void prototypeOutputRead(
    TwoWire *wire,
    uint16_t busAddr,
    OutputRead *conf,
    uint8_t *bytes
) {
    std::vector<OutputRead *> outputs;
    outputs.push_back(conf);
    for (uint16_t i = 0; i < conf->manualAdvanceNum; i++) {
        wire->beginTransmission(busAddr);
        if (conf->registerIdLength == RL16) {
            uint16_t regId = conf->registerId + i;
            wire->write(regId >> 8);
            wire->write(regId & 255);
        } else {
            wire->write(conf->registerId + i);
        }
        wire->endTransmission();
        delay(REGISTER_REQ_DELAY_MILLI);
        wire->requestFrom(busAddr, conf->numBytesPerAdvance);

        for (uint8_t j = 0; j < conf->numBytesPerAdvance; j++) {
            bytes[i * conf->numBytesPerAdvance + j] = wire->read();
        }
    }
}

void prototype(TwoWire *wire, Peripheral *peripheral, uint8_t **outputs) {
    wire->beginTransmission(peripheral->busAddr);
    for (uint8_t i = 0; i < peripheral->numInitialWriteBytes; i++) {
        wire->write(peripheral->initialWrite[i]);
    }
    wire->endTransmission();
    delay(PERIPHERAL_CONF_DELAY_MILLI);

    for (uint8_t i = 0; i < peripheral->numOutputs; i++) {
        prototypeOutputRead(wire, peripheral->busAddr, peripheral->outputs + i, outputs[i]);
    }
}
*/