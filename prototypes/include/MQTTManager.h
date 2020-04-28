#ifndef MQTT_MANAGER_H_
#define MQTT_MANAGER_H_

#include <Preferences.h>
#include <PubSubClient.h>
#include <WiFi.h>

#include "I2CRuntime.h"
#include "Scheduler.h"

class MQTTManager {
    private:
        I2CRuntime &mRuntime;
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
        void txPayload(uint32_t busId, uint16_t busAddress, ReadDefinition *def, uint8_t *payload);
        bool publish(uint8_t *payload, unsigned int len);
        bool publish(char *payload);
        bool publish(std::string payload);
        void subscribe();
        void onPayload(char * topic, uint8_t * payload, unsigned int size);

    public:
        MQTTManager(I2CRuntime &runtime);
        void setup();
        void loop();
};

#endif
