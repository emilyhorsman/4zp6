#include <Telemetry.pb.h>
#include <WiFi.h>
#include <pb_decode.h>
#include <pb_encode.h>
#include <vector>

#include "I2CPeripheral.h"
#include "TelemetryProtocol.h"

/**
 * @brief Buffer/size pair.
 */
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

bool encode_statuses(
    pb_ostream_t *stream,
    const pb_field_t *field,
    void * const *arg
) {
    std::vector<PeripheralStatus> *statuses = (std::vector<PeripheralStatus> *) *arg;

    for (auto status : *statuses) {
        if (!pb_encode_tag(stream, PB_WT_STRING, field->tag)) {
            return false;
        }
        Registration_Peripheral p = Registration_Peripheral_init_default;
        p.busId = (uint32_t) status.busId;
        p.busAddr = (uint32_t) status.busAddr;
        if (!pb_encode_submessage(stream, Registration_Peripheral_fields, &p)) {
            return false;
        }
    }

    return true;
}

size_t TelemetryProtocol::registration(std::vector<PeripheralStatus> *statuses, uint8_t *buffer) {
    Telemetry message = Telemetry_init_default;
    message.message = Telemetry_Message_REGISTRATION;
    message.registration.version = 1;

    uint8_t uuidBuf[6];
    WiFi.macAddress(uuidBuf);
    snprintf(
        message.registration.uuid,
        13,
        "%2x%2x%2x%2x%2x%2x",
        uuidBuf[0],
        uuidBuf[1],
        uuidBuf[2],
        uuidBuf[3],
        uuidBuf[4],
        uuidBuf[5]
    );

    IPAddress ipv4 = WiFi.localIP();
    snprintf(
        message.registration.ipv4,
        16,
        "%d.%d.%d.%d",
        ipv4[0], ipv4[1], ipv4[2], ipv4[3]
    );

    IPv6Address ipv6 = WiFi.localIPv6();
    snprintf(
        message.registration.ipv6,
        40,
        "%x%x:%x%x:%x%x:%x%x:%x%x:%x%x:%x%x:%x%x",
        ipv6[0], ipv6[1], ipv6[2], ipv6[3],
        ipv6[4], ipv6[5], ipv6[6], ipv6[7],
        ipv6[8], ipv6[9], ipv6[10], ipv6[11],
        ipv6[12], ipv6[13], ipv6[14], ipv6[15]
    );

    if (statuses != NULL) {
        message.registration.peripherals.arg = statuses;
        message.registration.peripherals.funcs.encode = encode_statuses;
    }

    pb_ostream_t stream = pb_ostream_from_buffer(buffer, 1024 /* TODO */);
    if (!pb_encode(&stream, Telemetry_fields, &message)) {
        Serial.printf("%lu Failed to encode registration message\n", millis());
        return 0;
    }

    return stream.bytes_written;
}

Peripheral * TelemetryProtocol::provisioning(
    uint8_t *buffer,
    unsigned int size
) {
    pb_istream_t stream = pb_istream_from_buffer(buffer, size);
    Telemetry message = Telemetry_init_default;

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
        return NULL;
    }

    if (message.message != Telemetry_Message_PROVISIONING) {
        return NULL;
    }

    if (readDefs.empty()) {
        return NULL;
    }
    // TODO: Manually free this later
    Peripheral *peripheral = (Peripheral *) malloc(sizeof(Peripheral));
    peripheral->busAddress = (uint16_t) (0xffff & message.provisioning.busAddr);
    peripheral->setupWriteDefinition = NULL;
    peripheral->numReadDefinitions = readDefs.size();
    peripheral->readDefinitions = (ReadDefinition **) malloc(sizeof(ReadDefinition *) * readDefs.size());
    for (size_t i = 0; i < readDefs.size(); i++) {
        peripheral->readDefinitions[i] = readDefs[i];
    }
    return peripheral;
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
    Telemetry message = Telemetry_init_default;
    message.message = Telemetry_Message_PAYLOAD;
    message.payload.busId = busId;
    message.payload.busAddr = busAddress;
    message.payload.definitionId = def->definitionId;
    Sized data = { payload, def->getNumBlockBytes() };
    message.payload.data.arg = &data;
    message.payload.data.funcs.encode = encode_string;

    pb_ostream_t stream = pb_ostream_from_buffer(buffer, 1024);
    if (!pb_encode(&stream, Telemetry_fields, &message)) {
        Serial.printf("%lu Failed to encode registration message\n", millis());
    }

    return stream.bytes_written;
}
