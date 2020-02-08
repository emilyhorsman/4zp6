#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <Preferences.h>
#include <WiFi.h>
#include <string>

#define PROVISIONING_PORT

void replace(std::string &haystack, std::string needle, const char * replacement);

class WiFiProvisioning {
    private:
        WiFiServer mServer;
        WiFiClient mClient;
        bool mHasConnectedClient;
        std::string mRequestBuffer;
        Preferences mPreferences;
        size_t mRequestStart;

        void stopClient();
        void controller();
        std::string getContent();
        void viewGet();
        void viewPost();
        bool isPostRequestComplete();

    public:
        WiFiProvisioning();
        void setup();
        void loop();
};

#endif
