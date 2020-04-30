#ifndef WIFI_PROVISIONING_H_
#define WIFI_PROVISIONING_H_

#include <Preferences.h>
#include <WiFi.h>
#include <string>

#include "Scheduler.h"

#define PROVISIONING_AP_SSID "TelemetryMicrocontroller"
#define PROVISIONING_AP_PASS "4zp6capstone"
#define PROVISIONING_CONNECTION_ATTEMPT_LIMIT 10
#define PREFERENCES_NAMESPACE "telemetry"
#define WEB_SERVER_TIMEOUT 1000

void replace(std::string &haystack, std::string needle, const char * replacement);

/**
 * @brief Manages the web application for provisioning connectivity information.
 *
 * Responsiblities:
 *
 *     - Connecting to an SSID if one has been stored in Flash memory
 *       (Preferences).
 *     - Broadcasting an AP if a network cannot be successfully connected to.
 *     - Serving a webpage that allows configuration of WiFi and MQTT
 *       parameters.
 *     - Writing Preferences to Flash memory.
 */
class WiFiProvisioning {
    private:
        WiFiServer mServer;
        WiFiClient mClient;
        bool mHasConnectedClient;
        std::string mRequestBuffer;
        Preferences mPreferences;
        /**
         * Allow us to timeout a HTTP request.
         */
        size_t mRequestStart;
        /**
         * Are we connected to a network or broadcasting an AP?
         */
        bool mIsNetworked;
        uint8_t mConnectionAttempts;
        Scheduler mScheduler;
        ScheduleId mScheduleTickId;

        /**
         * Returns false if SSID and password are not set.
         */
        bool tryConnectionFromPreferences();
        void broadcastAP();
        /**
         * Check to see if we're connected yet. If we exceed the connection
         * attempts then we should bail and just broadcast an AP.
         */
        void tick();

        /**
         * [Web Server]
         */
        void stopClient();
        /**
         * [Web Server] Parse an incoming request and dispatch to either the
         * GET or POST view if we've received a complete request.
         */
        void controller();
        /**
         * [Web Server] Get the form page and populate default values from
         * Flash memory.
         */
        std::string getContent();
        /**
         * [Web Server]
         */
        void viewGet();
        /**
         * [Web Server]
         */
        void viewPost();
        /**
         * [Web Server] Determine if we've received Content-Length amount of
         * payload.
         */
        bool isPostRequestComplete();

    public:
        WiFiProvisioning();
        void setup();
        void loop();
};

#endif
