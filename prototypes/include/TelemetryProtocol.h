#ifndef TELEMETRY_PROTOCOL_H_
#define TELEMETRY_PROTOCOL_H_

#include <Arduino.h>
#include <WiFi.h>

class TelemetryProtocol {
    public:
        static size_t registration(uint8_t *buffer);
};

#endif
