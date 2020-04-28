#ifndef TELEMETRY_PROTOCOL_H_
#define TELEMETRY_PROTOCOL_H_

#include <Arduino.h>
#include <Telemetry.pb.h>
#include <WiFi.h>

#include "I2CPeripheral.h"
#include "I2CRuntime.h"


class TelemetryProtocol {
    public:
        static size_t registration(uint8_t *buffer);
        static void provisioning(uint8_t *buffer, unsigned int size, I2CRuntime &runtime);
        static ReadDefinition * readDefinitionFromPB(Provisioning_ReadDef &msg);
        static size_t payload(uint32_t busId, uint16_t busAddress, ReadDefinition *def, uint8_t *payload, uint8_t *buffer);
};

#endif
