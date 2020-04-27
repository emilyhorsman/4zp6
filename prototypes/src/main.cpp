#include <Arduino.h>
#include <functional>
#include <memory>
#include <WiFi.h>
#include <Wire.h>

#define ENABLE_PROVISIONING
#define ENABLE_MQTT

#include "I2CManager.h"
#include "I2CRuntime.h"
#include "MQTTManager.h"
#include "Scheduler.h"
#include "WiFiProvisioning.h"

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
#ifdef ENABLE_PROVISIONING
WiFiProvisioning provisioning;
#endif
#ifdef ENABLE_MQTT
MQTTManager mqttManager;
#endif

void setup()
{
    Serial.begin(9600);
    while (!Serial) {
        delay(10);
    }
    Serial.println("Start");
    delay(10);

#ifdef ENABLE_PERIPHERALS
    runtime.addPeripheral(&sht31);
    scheduler.addSchedule(
        std::make_shared<Func>(
            []() {
                shtBuffer = runtime.getPeripheralBuffer(0);
                if (shtBuffer == NULL) {
                    return;
                }

                // Sample application logic that a peripheral processor might
                // implement outside the microcontroller.
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
#endif

    wire->begin();
    delay(3000);

#ifdef ENABLE_PROVISIONING
    provisioning.setup();
#endif

    Serial.printf("%lu Setup completed\n", millis());
}

void loop()
{
#ifdef ENABLE_PERIPHERALS
    runtime.loop();
    scheduler.loop();
#endif

#ifdef ENABLE_PROVISIONING
    provisioning.loop();
#endif

#ifdef ENABLE_MQTT
    mqttManager.loop();
#endif
}
