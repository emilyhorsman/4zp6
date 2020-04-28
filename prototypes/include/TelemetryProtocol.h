#ifndef TELEMETRY_PROTOCOL_H_
#define TELEMETRY_PROTOCOL_H_

#include <Arduino.h>
#include <Telemetry.pb.h>
#include <WiFi.h>

#include "I2CPeripheral.h"


class TelemetryProtocol {
    public:
        static size_t registration(uint8_t *buffer);
        static void provisioning(uint8_t *buffer, unsigned int size);
        static ReadDefinition * readDefinitionFromPB(Provisioning_ReadDef &msg);
};

#endif
