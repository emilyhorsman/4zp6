#include <Telemetry.pb.h>
#include <pb_encode.h>

#include "TelemetryProtocol.h"

bool encode_string(
    pb_ostream_t *stream,
    const pb_field_t *field,
    void * const *arg
) {
    if (!pb_encode_tag_for_field(stream, field)) {
        return false;
    }
    return pb_encode_string(stream, (uint8_t*) arg, strlen((char *) arg));
}

void TelemetryProtocol::registration(uint8_t *buffer, char *uuid) {
    Telemetry message = Telemetry_init_default;
    message.message = Telemetry_Message_REGISTRATION;
    message.registration.uuid.funcs.encode = encode_string;
    message.registration.uuid.arg = uuid;

    pb_ostream_t stream = pb_ostream_from_buffer(buffer, 1024 /* TODO */);
    if (!pb_encode(&stream, Telemetry_fields, &message)) {
        Serial.printf("%lu Failed to encode registration message\n", millis());
    }
}
