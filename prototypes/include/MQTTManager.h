#ifndef MQTT_MANAGER_H_
#define MQTT_MANAGER_H_

#include <Preferences.h>
#include <PubSubClient.h>
#include <WiFi.h>

#include "Scheduler.h"

class MQTTManager {
    private:
        Preferences mPreferences;
        WiFiClient mWiFiClient;
        PubSubClient mPubSub;
        Scheduler mScheduler;
        ScheduleId mScheduleTickId;
        std::string mUUID;

        void attemptConnection();
        void tick();

    public:
        MQTTManager();
        void setup();
        void loop();
};

#endif
