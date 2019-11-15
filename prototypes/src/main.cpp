#include "Arduino.h"
#include "Wire.h"
#include "I2CRuntime.h"

TwoWire *wire = &Wire;

OutputRead sht31TempAndHumidity = {
    0,
    0x2400,
    RL16,
    1,
    6,
    500,
};

Peripheral sht31 = {
    0x44,
    1,
    &sht31TempAndHumidity,
    NULL,
    0,
};

OutputRead amg8833Image = {
    1,
    0x80,
    RL8,
    128,
    1,
    1500,
};

Peripheral amg8833 = {
    0x69,
    1,
    &amg8833Image,
    NULL,
    0,
};

int count = 0;

void setup()
{
    Serial.begin(9600);
    Serial.println();
    delay(10);

    
    wire->begin();
    wire->setTimeout(5);
    /*
    wire->beginTransmission(0x18);
    wire->write(0x20);
    wire->write(0b01110111);
    wire->endTransmission();
    */
    delay(1000);

}

void loop()
{
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
    Serial.printf("Temp : %f\n", t);
    */

   prototype(wire, &amg8833);


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