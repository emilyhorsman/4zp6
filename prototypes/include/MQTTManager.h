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
        bool mIsSubscribed;

        void attemptConnection();
        void tick();
        void txRegistration();
        bool publish(uint8_t *payload, unsigned int len);
        bool publish(char *payload);
        bool publish(std::string payload);
        void subscribe();
        void onPayload(char * topic, uint8_t * payload, unsigned int size);

    public:
        MQTTManager();
        void setup();
        void loop();
};

#endif
