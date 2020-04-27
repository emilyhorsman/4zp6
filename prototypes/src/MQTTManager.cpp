#include <Arduino.h>
#include <WiFI.h>

#include "MQTTManager.h"
#include "TelemetryProtocol.h"
#include "WiFiProvisioning.h"

MQTTManager::MQTTManager()
: mPreferences()
, mWiFiClient()
, mPubSub(mWiFiClient)
, mScheduler()
, mScheduleTickId()
, mUUID()
, mTXUUID()
, mRXUUID()
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
    WiFi.macAddress(macAddr);
    for (uint8_t i = 0; i < 6; i++) {
        snprintf(mUUID + i * 2, 3, "%x", macAddr[i]);
    }
    snprintf(mUUID, 13, "%x%x%x%x%x%x", macAddr[0], macAddr[1], macAddr[2], macAddr[3], macAddr[4], macAddr[5]);
    snprintf(mTXUUID, 16, "tx/%s", mUUID);
    snprintf(mRXUUID, 16, "rx/%s", mUUID);
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
    bool status = mPubSub.connect(mUUID, user.c_str(), pass.c_str());
    if (status) {
        this->txRegistration();
    }
}


void MQTTManager::txRegistration() {
    Serial.printf("%lu Sending registration\n", millis());
    Serial.println(this->publish("hello"));
}


bool MQTTManager::publish(std::string payload) {
    return mPubSub.publish(mTXUUID, payload.c_str());
}
