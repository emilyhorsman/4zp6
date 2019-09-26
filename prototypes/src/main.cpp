#include <Arduino.h>
#include "Wire.h"

TwoWire *wire = &Wire;

int count = 0;

void setup()
{
    Serial.begin(9600);

    wire->begin();
    wire->setTimeout(5);
    delay(1000);
}

void loop()
{
    /*
        SHT31-D

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
        wire->write(0x0F);
        wire->endTransmission();
        delay(20);

        uint8_t buff[1];
        wire->requestFrom(0x18, 1);
        if (wire->readBytes(buff, 1) == 1) {
        Serial.printf("[%d] 0x18 ID: %x\n", count++, *buff);}
    */

    // wire->beginTransmission(0x44);
    //wire->write(0x0);
    // wire->endTransmission();
    delay(20);

    char buff[1];

    for (uint8_t i=0; i < 128; i++)
    {
        wire->requestFrom(i, 1);
        if (wire->available() == 1)
            Serial.printf("Found %x\n", i);
        else
            Serial.printf("Nothing on %x\n", i);
    }
    delay(10 * 1000);
}
