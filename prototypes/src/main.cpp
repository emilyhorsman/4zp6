#include <Arduino.h>
#include <functional>
#include <memory>
#include <WiFi.h>
#include <Wire.h>

#define ENABLE_PROVISIONING
#define ENABLE_PERIPHERALS
#define ENABLE_MQTT

#include "I2CManager.h"
#include "I2CRuntime.h"
#include "MQTTManager.h"
#include "Scheduler.h"
#include "WiFiProvisioning.h"

TwoWire *wire = &Wire;

Scheduler scheduler;
I2CRuntime runtime(wire);
I2CManager manager(wire);
uint8_t **shtBuffer = NULL;
#ifdef ENABLE_PROVISIONING
WiFiProvisioning provisioning;
#endif
#ifdef ENABLE_MQTT
MQTTManager mqttManager(runtime);
#endif

void setup()
{
    Serial.begin(9600);
    while (!Serial) {
        delay(10);
    }
    Serial.println("Start");
    delay(10);

    wire->begin();
    delay(1000);

#ifdef ENABLE_PROVISIONING
    provisioning.setup();
#endif

#ifdef ENABLE_MQTT
    mqttManager.setup();
#endif

    manager.setCallback(std::make_shared<std::function<void (uint8_t)>>(
        [](uint8_t busAddr) {
            Serial.printf(
                "%lu %#x changed to %d\n",
                millis(),
                busAddr,
                manager.isConnected(busAddr)
            );
        }
    ));

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

    manager.loop();
}
