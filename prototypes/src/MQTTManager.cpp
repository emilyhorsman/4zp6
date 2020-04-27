#include <Arduino.h>
#include <WiFI.h>

#include "MQTTManager.h"
#include "WiFiProvisioning.h"

MQTTManager::MQTTManager()
: mPreferences()
, mWiFiClient()
, mPubSub(mWiFiClient)
, mScheduler()
, mScheduleTickId()
, mUUID()
{
    mScheduleTickId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&MQTTManager::tick, this)),
        1000,
        true
    );
}


void MQTTManager::setup() {
    mPreferences.begin(PREFERENCES_NAMESPACE, false);

    byte macAddr[6];
    char buf[3];
    WiFi.macAddress(macAddr);
    mUUID.clear();
    for (uint8_t i = 0; i < 6; i++) {
        snprintf(buf, 3, "%x", macAddr[i]);
        mUUID += buf[0];
        mUUID += buf[1];
    }
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
    bool status = mPubSub.connect(mUUID.c_str(), user.c_str(), pass.c_str());
    if (status) {
        this->txRegistration();
    }
}


void MQTTManager::txRegistration() {
    Serial.printf("%lu Sending registration\n", millis());
    Serial.println(this->publish("hello"));
}


bool MQTTManager::publish(std::string payload) {
    char txUUID[16];
    snprintf(txUUID, 16, "tx/%s", mUUID.c_str());
    return mPubSub.publish(txUUID, payload.c_str());
}
