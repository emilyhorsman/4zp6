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
        char mUUID[13];
        char mTXUUID[16];
        char mRXUUID[16];

        void attemptConnection();
        void tick();
        void txRegistration();
        bool publish(std::string payload);

    public:
        MQTTManager();
        void setup();
        void loop();
};

#endif
