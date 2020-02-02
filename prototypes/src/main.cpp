#include <Arduino.h>
#include <functional>
#include <memory>
#include <Wire.h>

#include "I2CManager.h"
#include "I2CRuntime.h"
#include "Scheduler.h"

TwoWire *wire = &Wire;

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
I2CManager manager(wire);
uint8_t **shtBuffer = NULL;

void setup()
{
    Serial.begin(9600);
    while (!Serial) {
        delay(10);
    }
    Serial.println("Start");
    delay(10);

    runtime.addPeripheral(&sht31);
    scheduler.addSchedule(
        std::make_shared<Func>(
            []() {
                shtBuffer = runtime.getPeripheralBuffer(0);
                if (shtBuffer == NULL) {
                    return;
                }
                Serial.printf(
                    "%x %x %x %x %x %x\n",
                    shtBuffer[0][0],
                    shtBuffer[0][1],
                    shtBuffer[0][2],
                    shtBuffer[0][3],
                    shtBuffer[0][4],
                    shtBuffer[0][5]
                );

                uint16_t temp;
                temp = shtBuffer[0][0];
                temp <<= 8;
                temp |= shtBuffer[0][1];
                double t = temp;
                t *= 175;
                t /= 0xffff;
                t = -45 + t;
                Serial.printf("Temp: %f\n", t);
            }
        ),
        1000
    );
    
    wire->begin();
    delay(5000);
    Serial.println("starting");
    wire->beginTransmission(0x44);
    wire->write(0);
    //wire->write(0xA2);
    //wire->write(0x30);
    wire->endTransmission();
    delay(10);
    Serial.println(wire->getErrorText(wire->lastError()));
    wire->requestFrom(0x44, 1u);
    Serial.println(wire->available());
    Serial.println(wire->getErrorText(wire->lastError()));
    delay(10);
}

void loop()
{
    manager.loop();
    //runtime.loop();
    scheduler.loop();
}