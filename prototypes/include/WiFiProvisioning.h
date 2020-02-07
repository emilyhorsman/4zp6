#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <Preferences.h>
#include <WiFi.h>
#include <string>

#define PROVISIONING_PORT

class WiFiProvisioning {
    private:
        WiFiServer mServer;
        WiFiClient mClient;
        bool mHasConnectedClient;
        std::string mRequestBuffer;
        Preferences mPreferences;

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
