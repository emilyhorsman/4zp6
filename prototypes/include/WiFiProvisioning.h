#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <WiFi.h>

#define PROVISIONING_PORT

class WiFiProvisioning {
    private:
        WiFiServer mServer;

    public:
        WiFiProvisioning();
        void setup();
        void loop();
};

#endif