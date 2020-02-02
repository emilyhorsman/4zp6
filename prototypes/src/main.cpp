#include <Arduino.h>
#include <functional>
#include <memory>
#include <Wire.h>

#include "I2CManager.h"

TwoWire *wire = &Wire;

/*
#include "I2CRuntime.h"
#include "Scheduler.h"


ReadDefinition sht31TempAndHumidity = {
    0,
    RL16,
    0x2400,
    1,
    6,
    2000,
};

ReadDefinition * sht31defs[1] = { &sht31TempAndHumidity };

Peripheral sht31 = {
    0x44,
    NULL,
    1,
    sht31defs,
};

ReadDefinition amg8833Image = {
    1,
    RL8,
    0x80,
    128,
    1,
    1500,
};

ReadDefinition * amg8833defs[1] = { &amg8833Image };

Peripheral amg8833 = {
    0x69,
    NULL,
    1,
    amg8833defs,
};


Scheduler scheduler;

I2CRuntime runtime(wire);
*/
I2CManager manager(wire);

void setup()
{
    Serial.begin(9600);
    while (!Serial) {
        delay(10);
    }
    Serial.println("Start");
    delay(10);

    //runtime.addPeripheral(&sht31);
    
    wire->begin();
    /*
    wire->beginTransmission(0x18);
    wire->write(0x20);
    wire->write(0b01110111);
    wire->endTransmission();
    */
    delay(1000);

    /*wire->beginTransmission(0x44);
    wire->write(0x30A2 >> 8);
    wire->write(0x30A2 & 0xFF);
    wire->endTransmission();
    delay(10);*/

}

void loop()
{
    //Serial.println("loop");
    manager.loop();
    /*wire->beginTransmission(0x44);
    wire->write(0x00);
    wire->write(0x06);
    wire->endTransmission();
    delay(10);
    wire->requestFrom(0x44, 1);
    Serial.println(wire->getErrorText(wire->lastError()));
    Serial.println(wire->available());
    delay(1000);*/
    //runtime.loop();

}