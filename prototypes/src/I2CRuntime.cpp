#include <Arduino.h>
#include <vector>
#include <Wire.h>

#include "I2CRuntime.h"
#include "Scheduler.h"

uint16_t ReadDefinition::getNumBlockBytes() {
    return registerBlockLength * numBytesPerRegister;
}

I2CPeripheralManager::I2CPeripheralManager(Peripheral *peripheral, TwoWire *wire)
: mPeripheral(peripheral)
, mBuffer(NULL)
, mReadManagers()
, mWire(wire)
{
    mBuffer = I2CPeripheralManager::allocateBytes(mPeripheral);
    for (uint8_t i = 0; i < peripheral->numReadDefinitions; i++) {
        ReadDefinition *def = peripheral->readDefinitions[i];
        Serial.printf("manager constructor: %i %p\n", i, def);
        mReadManagers.push_back(new I2CReadManager(
            def,
            peripheral,
            mBuffer[i],
            mWire,
            std::make_shared<Func>(
                [=]() {
                    Serial.printf("%lu completed read %d %p\n", millis(), i, def);
                    /*mRuntime->txPayload(
                        1,
                        mPeripheral->busAddress,
                        def,
                        mBuffer[i]
                    );*/
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

I2CRuntime::I2CRuntime(TwoWire *wire)
: mManagers()
, mWire(wire)
{}

std::size_t I2CRuntime::addPeripheral(Peripheral *peripheral) {
    mManagers.push_back(new I2CPeripheralManager(peripheral, mWire));
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
