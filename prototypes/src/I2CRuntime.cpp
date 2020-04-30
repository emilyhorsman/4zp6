#include <Arduino.h>
#include <vector>
#include <Wire.h>

#include "I2CRuntime.h"
#include "Scheduler.h"
#include "TelemetryProtocol.h"

uint16_t ReadDefinition::getNumBlockBytes() {
    return registerBlockLength * numBytesPerRegister;
}

I2CPeripheralManager::I2CPeripheralManager(Peripheral *peripheral, TwoWire *wire, I2CRuntime *runtime)
: mPeripheral(peripheral)
, mBuffer(NULL)
, mReadManagers()
, mWire(wire)
, mRuntime(runtime)
{
    mBuffer = I2CPeripheralManager::allocateBytes(mPeripheral);
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        ReadDefinition *def = peripheral->readDefinitions[i];
        mReadManagers.push_back(new I2CReadManager(
            def,
            peripheral,
            mBuffer[i],
            mWire,
            std::make_shared<Func>(
                [=]() {
                    (*mRuntime->mPayloadFunc) (
                        1,
                        mPeripheral->busAddress,
                        def,
                        mBuffer[i]
                    );
                }
            )
        ));
    }
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

void I2CPeripheralManager::loop() {
    // We can break this up to use a cursor and call one loop per call if
    // needed.
    for (auto readManager : mReadManagers) {
        readManager->loop();
    }
}

uint8_t ** I2CPeripheralManager::getBuffer() {
    for (auto readManager : mReadManagers) {
        if (readManager->isWriting()) {
            return NULL;
        }
    }

    return mBuffer;
}

uint8_t I2CPeripheralManager::getBusAddr() {
    return mPeripheral->busAddress;
}

I2CRuntime::I2CRuntime(TwoWire *wire)
: mManagers()
, mWire(wire)
, mPayloadFunc(NULL)
{}

std::size_t I2CRuntime::addPeripheral(Peripheral *peripheral) {
    I2CPeripheralManager *m = new I2CPeripheralManager(peripheral, mWire, this);
    for (std::size_t i = 0; i < mManagers.size(); i++) {
        if (mManagers[i]->getBusAddr() == peripheral->busAddress) {
            // TODO: free old peripheral manager
            mManagers[i] = m;
            Serial.printf("%lu Added peripheral %d\n", millis(), i);
            return i;
        }
    }

    mManagers.push_back(m);
    Serial.printf("%lu Added peripheral %d\n", millis(), mManagers.size() - 1);
    return mManagers.size() - 1;
}

bool I2CRuntime::hasPeripheral(std::size_t peripheralId) {
    return peripheralId < mManagers.size();
}

void I2CRuntime::loop() {
    // We can break this up to use a cursor and call one loop per call if
    // needed.
    for (auto manager : mManagers) {
        manager->loop();
    }
}

uint8_t ** I2CRuntime::getPeripheralBuffer(std::size_t peripheralId) {
    assert(this->hasPeripheral(peripheralId));
    return mManagers[peripheralId]->getBuffer();
}


void I2CRuntime::setPayloadFunc(std::shared_ptr<PayloadFunc> func) {
    mPayloadFunc = func;
}
