#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <Preferences.h>
#include <WiFi.h>
#include <string>

#include "Scheduler.h"

void replace(std::string &haystack, std::string needle, const char * replacement);

class WiFiProvisioning {
    private:
        WiFiServer mServer;
        WiFiClient mClient;
        bool mHasConnectedClient;
        std::string mRequestBuffer;
        Preferences mPreferences;
        size_t mRequestStart;
        bool mIsNetworked;
        uint8_t mConnectionAttempts;
        Scheduler mScheduler;
        ScheduleId mScheduleTickId;

        void stopClient();
        void controller();
        std::string getContent();
        void viewGet();
        void viewPost();
        bool isPostRequestComplete();
        /**
         * Returns false if SSID and password are not set.
         */
        bool tryConnectionFromPreferences();
        void broadcastAP();
        void unnetworkedLoop();
        void tick();

    public:
        WiFiProvisioning();
        void setup();
        void loop();
};

#endif
