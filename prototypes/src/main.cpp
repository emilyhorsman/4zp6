#include "header.h"
#include <Wire.h>

// Telemetry protobuf
#include <pb_encode.h>
#include <pb_decode.h>
#include <Telemetry.pb.h>

//********** COMMON SETTINGS **********//

#define WIFI_SSID "RYAN"
#define WIFI_PASS "#RyanHome1!0"
#define MQTT_HOST "broker.hivemq.com"
#define MQTT_PORT 1883

//*************************************//

TwoWire *wire = &Wire;

int count = 0;

void setup()
{
    Serial.begin(9600);
    Serial.println();
    delay(10);

    pinMode(LED_BUILTIN, OUTPUT);

    // establish connectivity
    connect_wifi();
    mqtt.setServer(MQTT_HOST, MQTT_PORT);
    mqtt.setCallback(mqtt_rx);
    connect_mqtt();

    wire->begin();
    wire->setTimeout(5);
    wire->beginTransmission(0x18);
    wire->write(0x20);
    wire->write(0b01110111);
    wire->endTransmission();
    delay(1000);
}

void loop()
{
    if (WiFi.status() != WL_CONNECTED)
    {
        Serial.println(F("[Wifi] Connection lost"));
        connect_wifi();
    }
    if (!mqtt.connected())
    {
        Serial.println(F("[MQTT] Connection lost"));
        connect_mqtt();
    }
    mqtt.loop();

    /*
    char buff[6];
    wire->beginTransmission(0x44);
    wire->write(0x24);
    wire->write(0x0);
    wire->endTransmission();
    delay(30);
    wire->requestFrom(0x44, 6);
    wire->readBytes(buff, 6);
    uint16_t temp;
    temp = buff[0];
    temp <<= 8;
    temp |= buff[1];
    double t = temp;
    t *= 175;
    t /= 0xffff;
    t = -45 + t;
    Serial.printf("Temp: %f\n", t);
    */


    /*
    wire->beginTransmission(0x18);
    // Set the MSB in the 8 bit address to 1 so that it will auto increment.
    // Page 21/42 of https://cdn-shop.adafruit.com/datasheets/LIS3DH.pdf
    wire->write(0x28 | 128); 
    wire->endTransmission();
    delay(20);
    uint8_t buffb[6];
    wire->requestFrom(0x18, 6);
    uint16_t x, y, z;
    float xg, yg, zg;
    if (wire->readBytes(buffb, 6) == 6) {
        uint16_t x = buffb[0];
        x |= buffb[1] << 8;
        uint16_t y = buffb[2];
        y |= buffb[3] << 8;
        uint16_t z = buffb[4];
        z |= buffb[5] << 8;
        xg = (float) x / 16380;
        yg = (float) y / 16380;
        zg = (float) z / 16380;
        Serial.printf("[%d] 0x18 %f %f %f\n", count++, xg, yg, zg);
    }

    // Test multiple register reads, should match values.
    for (int i = 0; i < 6; i++) {
        wire->beginTransmission(0x18);
        wire->write(0x28 + i);
        wire->endTransmission();
        delay(20);
        wire->requestFrom(0x18, 1);
        buffb[i] = wire->read();
    }
    x = buffb[0];
    x |= buffb[1] << 8;
    y = buffb[2];
    y |= buffb[3] << 8;
    z = buffb[4];
    z |= buffb[5] << 8;
    xg = (float) x / 16380;
    yg = (float) y / 16380;
    zg = (float) z / 16380;
    Serial.printf("[%d] 0x18 %f %f %f\n", count, xg, yg, zg);*/

    /*
    uint8_t thermalBuf[128];
    for (int i = 0; i < 128; i++) {
        wire->beginTransmission(0x69);
        wire->write(0x80 + i);
        wire->endTransmission();
        wire->requestFrom(0x69, 1);
        thermalBuf[i] = wire->read();
    }
    float image[64];
    for (int i = 0; i < 64; i++) {
        uint16_t foo = thermalBuf[i * 2];
        foo |= thermalBuf[i * 2 + 1] << 8;
        foo &= 0b0000011111111111;
        image[i] = (float) foo / 4;
    }
    for (int x = 0; x < 8; x++) {
        for (int y = 0; y < 8; y++) {
            if (image[x * 8 + y] > t) {
                Serial.printf("1");
            } else {
                Serial.printf("0");
            }
        }
        Serial.println();
    }
    delay(1000);


    for (uint8_t i=0; i < 128; i++)
    {
        wire->requestFrom(i, 1);
        if (wire->available() == 1)
            Serial.printf("Found %x\n", i);
        else
            Serial.printf("Nothing on %x\n", i);
    }*/
    // delay(1000);
}

// Establish wifi connection. Called on setup and upon disconnection. Will block
// until connection is established.
void connect_wifi()
{
    Serial.print(F("[Wifi] Connecting to "));
    Serial.println(F(WIFI_SSID));
    WiFi.begin(WIFI_SSID, WIFI_PASS);

    while (WiFi.waitForConnectResult() != WL_CONNECTED)
    {
        Serial.println(F("[Wifi] Connection failed, retrying..."));
        delay(5000);
        WiFi.begin(WIFI_SSID, WIFI_PASS);
    }

    Serial.print(F("[Wifi] Connected\n[Wifi] IPv4: "));
    Serial.println(WiFi.localIP());
    Serial.print(F("[Wifi] IPv6: "));
    Serial.println(WiFi.localIPv6());
    Serial.print(F("[Wifi] MAC: "));
    Serial.println(WiFi.macAddress());

    uuid = normalize_mac(WiFi.macAddress());
}

// Establish MQTT connection. Called on setup and upon disconnection. Will block
// until connection is established.
void connect_mqtt()
{
    Serial.print(F("[MQTT] Connecting to "));
    Serial.print(F(MQTT_HOST));
    Serial.print(F(":"));
    Serial.println(MQTT_PORT);

    delay(1000);

    mqtt.connect(uuid.c_str());
    while (!mqtt.connected())
    {
        Serial.println(F("[MQTT] Connection failed, retrying..."));
        delay(5000);
        mqtt.connect(uuid.c_str());
    }

    // subscribe to incoming messages (broadcast and specific topic)
    mqtt.subscribe(MQTT_RX);
    mqtt.subscribe(String(MQTT_RX + uuid).c_str());

    // send hello message
    mqtt_tx_hello();
    Serial.println(F("[MQTT] Connected"));
}

// Incoming MQTT message handler. Called on incoming MQTT messages.
void mqtt_rx(char* topic, byte* payload, unsigned int msg_length)
{
    bool status;

    Telemetry msg = Telemetry_init_zero;
    pb_istream_t stream = pb_istream_from_buffer(payload, msg_length);
    status = pb_decode(&stream, Telemetry_fields, &msg);

    if (!status)
    {
        Serial.println(F("[MQTT] Error decoding RX message"));
    } else
    {
        switch (msg.type)
        {
            case Telemetry_Type_RX_HELLO:
                Serial.println(F("[MQTT] Received RX_HELLO message"));
                mqtt_tx_hello();
                break;

            case Telemetry_Type_RX_ACTION:
                Serial.println(F("[MQTT] Received RX_ACTION message"));
                if (msg.action.type == MsgAction_Action_DIGITAL_LOW)
                    digitalWrite(msg.action.pin, LOW);
                else
                    digitalWrite(msg.action.pin, HIGH);
                break;

            default:
                break;
        }
    }
}

// Send MQTT TX_HELLO message.
void mqtt_tx_hello()
{
    uint8_t buffer[Telemetry_size];
    size_t msg_length;
    bool status;

    Telemetry msg = Telemetry_init_zero;
    pb_ostream_t stream = pb_ostream_from_buffer(buffer, sizeof(buffer));

    // message fields
    msg.type = Telemetry_Type_TX_HELLO;
    strcpy(msg.hello.ip, String(WiFi.localIP().toString()).c_str());
    msg.hello.version = 1;

    status = pb_encode(&stream, Telemetry_fields, &msg);
    msg_length = stream.bytes_written;

    if (!status)
    {
        Serial.println(F("[MQTT] Error encoding TX_HELLO"));
    } else
    {
        mqtt.publish(String(MQTT_TX + uuid).c_str(), buffer, msg_length);
        Serial.println(F("[MQTT] Sent TX_HELLO message"));
    }
}
