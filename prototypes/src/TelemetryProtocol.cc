#include <Telemetry.pb.h>
#include <WiFi.h>
#include <pb_decode.h>
#include <pb_encode.h>
#include <vector>

#include "I2CPeripheral.h"
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

void TelemetryProtocol::provisioning(
    uint8_t *buffer,
    unsigned int size,
    I2CRuntime &runtime
) {
    pb_istream_t stream = pb_istream_from_buffer(buffer, size);
    Telemetry message = Telemetry_init_default;

    std::vector<uint8_t> busAddrs;
    message.provisioning.busAddr.arg = &busAddrs;
    message.provisioning.busAddr.funcs.decode = [](
        pb_istream_t *stream,
        const pb_field_t *field,
        void **arg
    ) -> bool {
        // TODO: I2CPeripheral stores a single bus address because it's like data not definition.
        // TODO: Create multiple Peripheral instances that share ReadDefinitions
        // TODO: I think Peripheral::busAddress should just be a uint8_t
        uint8_t buf;
        while (stream->bytes_left) {
            if (!pb_read(stream, &buf, 1)) {
                return false;
            }
            ((std::vector<uint8_t> *) (*arg))->push_back(buf);
        }
        return true;
    };

    std::vector<ReadDefinition *> readDefs;
    message.provisioning.readDefinitions.arg = &readDefs;
    message.provisioning.readDefinitions.funcs.decode = [](
        pb_istream_t *stream,
        const pb_field_t *field,
        void **arg
    ) -> bool {
        Provisioning_ReadDef submessage;
        if (!pb_decode(stream, Provisioning_ReadDef_fields, &submessage)) {
            return false;
        }

        ((std::vector<ReadDefinition *> *) (*arg))->push_back(
            TelemetryProtocol::readDefinitionFromPB(submessage)
        );

        return true;
    };

    if (!pb_decode(&stream, Telemetry_fields, &message)) {
        Serial.printf("%lu Failed to decode message\n", millis());
        return;
    }

    if (message.message != Telemetry_Message_PROVISIONING) {
        return;
    }

    if (busAddrs.empty() || readDefs.empty()) {
        return;
    }
    // TODO: Manually free this later
    Peripheral *peripheral = (Peripheral *) malloc(sizeof(Peripheral));
    // TODO: Support multiple bus addresses
    peripheral->busAddress = busAddrs[0];
    peripheral->setupWriteDefinition = NULL;
    peripheral->numReadDefinitions = readDefs.size();
    peripheral->readDefinitions = (ReadDefinition **) malloc(sizeof(ReadDefinition *) * readDefs.size());
    for (size_t i = 0; i < readDefs.size(); i++) {
        peripheral->readDefinitions[i] = readDefs[i];
    }
    runtime.addPeripheral(peripheral);
}


ReadDefinition * TelemetryProtocol::readDefinitionFromPB(Provisioning_ReadDef &msg) {
    // TODO: Make sure these are freed
    ReadDefinition *def = (ReadDefinition *) malloc(sizeof(ReadDefinition));
    def->definitionId = (uint16_t) (0xffff & msg.definitionId);
    def->registerIdLength = (RegisterLength) msg.registerIdLength;
    def->registerId = (uint16_t) (0xffff & msg.registerId);
    def->registerBlockLength = (uint8_t) (0xff & msg.registerBlockLength);
    def->numBytesPerRegister = (uint8_t) (0xff & msg.numBytesPerRegister);
    def->readPeriod = msg.readPeriod;
    return def;
}


size_t TelemetryProtocol::payload(uint32_t busId, uint16_t busAddress, ReadDefinition *def, uint8_t *payload, uint8_t *buffer) {
    Serial.printf("payload %p\n", def);
    Serial.println("populating payload");
    Telemetry message = Telemetry_init_default;
    message.message = Telemetry_Message_PAYLOAD;
    message.payload.busId = busId;
    message.payload.busAddr = busAddress;
    Serial.println("i'm gonna do it i'm gonna dereference it");
    message.payload.definitionId = def->definitionId;
    Serial.println("dereferenced def");
    Sized data = { payload, def->getNumBlockBytes() };
    Serial.println("constucted Sized");
    message.payload.data.arg = &data;
    message.payload.data.funcs.encode = encode_string;

    pb_ostream_t stream = pb_ostream_from_buffer(buffer, 1024);
    if (!pb_encode(&stream, Telemetry_fields, &message)) {
        Serial.printf("%lu Failed to encode registration message\n", millis());
    }

    return stream.bytes_written;
}
