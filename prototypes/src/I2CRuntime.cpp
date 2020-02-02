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
        mReadManagers.push_back(new I2CReadManager(
            peripheral->readDefinitions[i],
            peripheral,
            mBuffer[i],
            mWire
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
    return mManagers.size() - 1;
}

void I2CRuntime::loop() {
    // We can break this up to use a cursor and call one loop per call if
    // needed.
    for (auto manager : mManagers) {
        manager->loop();
    }
}

uint8_t ** I2CRuntime::getPeripheralBuffer(std::size_t peripheralId) {
    assert(peripheralId < mManagers.size());
    return mManagers[peripheralId]->getBuffer();
}
