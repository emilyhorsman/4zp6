#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <string>
#include <WiFi.h>

#define PROVISIONING_PORT

class WiFiProvisioning {
    private:
        WiFiServer mServer;
        WiFiClient mClient;
        bool mHasConnectedClient;
        std::string mRequestBuffer;

        void stopClient();
        void controller();
        void viewGet();
        void viewPost();
        bool isPostRequestComplete();

    public:
        WiFiProvisioning();
        void setup();
        void loop();
};

#endif