#include <Arduino.h>

#include "MQTTManager.h"
#include "WiFiProvisioning.h"

MQTTManager::MQTTManager()
: mPreferences()
, mWiFiClient()
, mPubSub(mWiFiClient)
, mScheduler()
, mScheduleTickId()
{
    mScheduleTickId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&MQTTManager::tick, this)),
        1000,
        true
    );
}


void MQTTManager::setup() {
    mPreferences.begin(PREFERENCES_NAMESPACE, false);
}


void MQTTManager::loop() {
    mScheduler.loop();
    mPubSub.loop();
}


void MQTTManager::tick() {
    if (!mPubSub.connected()) {
        this->attemptConnection();
    }
}


void MQTTManager::attemptConnection() {
    String host = mPreferences.getString("mqtt_host");
    String user = mPreferences.getString("mqtt_user");
    String pass = mPreferences.getString("mqtt_password");
    uint32_t port = mPreferences.getUInt("mqtt_port", 0);
    if (host.isEmpty() || user.isEmpty() || pass.isEmpty() || !port) {
        Serial.printf("%lu Incomplete MQTT connection details in preferences.\n", millis());
        return;
    }

    Serial.printf("%lu Attempting connection\n", millis());
    mPubSub.setServer(host.c_str(), port);
    Serial.printf("%lu Connection status: %d\n", millis(), mPubSub.connect("test", user.c_str(), pass.c_str()));
}
