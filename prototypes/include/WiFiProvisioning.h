#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <WiFi.h>

#define PROVISIONING_PORT

class WiFiProvisioning {
    private:
        WiFiServer mServer;
        WiFiClient mClient;
        bool mHasConnectedClient;

        void stopClient();

    public:
        WiFiProvisioning();
        void setup();
        void loop();
};

#endif