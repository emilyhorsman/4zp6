#include <WiFi.h>
#include <Telemetry.pb.h>
#include <pb_encode.h>

#include "TelemetryProtocol.h"

struct Sized {
    const uint8_t *buf;
    size_t size;
};

typedef struct Sized Sized;

bool encode_string(
    pb_ostream_t *stream,
    const pb_field_t *field,
    void * const *arg
) {
    if (!pb_encode_tag_for_field(stream, field)) {
        return false;
    }
    Sized *p = (Sized *) *arg;
    return pb_encode_string(stream, p->buf, p->size);
}

size_t TelemetryProtocol::registration(uint8_t *buffer) {
    Telemetry message = Telemetry_init_default;
    message.message = Telemetry_Message_REGISTRATION;
    message.registration.version = 1;

    uint8_t uuidBuf[6];
    WiFi.macAddress(uuidBuf);
    Sized uuidArg = { uuidBuf, 6 };
    message.registration.uuid.funcs.encode = encode_string;
    message.registration.uuid.arg = &uuidArg;

    uint32_t ipv4Buf = WiFi.localIP();
    Sized ipv4Arg = { (uint8_t *) &ipv4Buf, 4 };
    message.registration.ipv4.funcs.encode = encode_string;
    message.registration.ipv4.arg = &ipv4Arg;

    IPv6Address ipv6 = WiFi.localIPv6();
    Sized ipv6Arg = { (const uint8_t *) ipv6, 6 };
    message.registration.ipv6.funcs.encode = encode_string;
    message.registration.ipv6.arg = &ipv6Arg;

    pb_ostream_t stream = pb_ostream_from_buffer(buffer, 1024 /* TODO */);
    if (!pb_encode(&stream, Telemetry_fields, &message)) {
        Serial.printf("%lu Failed to encode registration message\n", millis());
        return 0;
    }

    return stream.bytes_written;
}