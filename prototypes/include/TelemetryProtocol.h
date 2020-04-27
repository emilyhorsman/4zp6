#ifndef TELEMETRY_PROTOCOL_H_
#define TELEMETRY_PROTOCOL_H_

#include <Arduino.h>

class TelemetryProtocol {
    public:
        static void registration(uint8_t *buffer, char *uuid);
};

#endif
