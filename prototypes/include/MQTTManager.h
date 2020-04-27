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

    public:
        MQTTManager();
        void loop();
};

#endif
