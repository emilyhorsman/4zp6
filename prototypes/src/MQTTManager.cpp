#include <Arduino.h>

#include "MQTTManager.h"
#include "WiFiProvisioning.h"

MQTTManager::MQTTManager()
: mPreferences()
, mWiFiClient()
, mPubSub(mWiFiClient)
, mScheduler()
, mScheduleTickId()
{}


void MQTTManager::setup() {
    mPreferences.begin(PREFERENCES_NAMESPACE, false);
}


void MQTTManager::loop() {
    mPubSub.loop();
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
}
