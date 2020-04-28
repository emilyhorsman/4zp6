#ifndef TELEMETRY_PROTOCOL_H_
#define TELEMETRY_PROTOCOL_H_

#include <Arduino.h>
#include <Telemetry.pb.h>
#include <WiFi.h>

#include "I2CPeripheral.h"


typedef std::function<void (uint32_t, uint16_t, ReadDefinition *, uint8_t *)> PayloadFunc;


class TelemetryProtocol {
    public:
        static size_t registration(uint8_t *buffer);
        static size_t payload(uint32_t busId, uint16_t busAddress, ReadDefinition *def, uint8_t *payload, uint8_t *buffer);
        static Peripheral * provisioning(uint8_t *buffer, unsigned int size);
        static ReadDefinition * readDefinitionFromPB(Provisioning_ReadDef &msg);
};

#endif
