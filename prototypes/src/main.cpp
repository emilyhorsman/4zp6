/*
 * Copyright (c) 2019 Emily Horsman, Tanner Ryan. All rights reserved.
 */
#include "headers.h"
#include "Wire.h"

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


    /*
    for (uint8_t i=0; i < 128; i++)
    {
        wire->requestFrom(i, 1);
        if (wire->available() == 1)
            Serial.printf("Found %x\n", i);
        else
            Serial.printf("Nothing on %x\n", i);
    }*/
    delay(1000);
}

void connect_wifi()
{
    Serial.print(F("[Wifi] Connecting to "));
    Serial.println(F(WIFI_SSID));
    WiFi.begin(WIFI_SSID, WIFI_PASS);

    while (WiFi.waitForConnectResult() != WL_CONNECTED)
    {
        Serial.println(F("[Wifi] Connection failed, retrying..."));
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

    // send hello here

    // subscribe to incoming messages here
    mqtt.subscribe("tanner");

    Serial.println(F("[MQTT] Connected"));
}

void mqtt_rx(char* topic, byte* payload, unsigned int msg_length)
{
    // incoming MQTT message handler
    Serial.printf("%s :: %s\n", topic, payload);
}
