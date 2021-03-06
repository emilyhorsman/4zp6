#include <Arduino.h>
#include <WiFI.h>

#include "I2CRuntime.h"
#include "MQTTManager.h"
#include "TelemetryProtocol.h"
#include "WiFiProvisioning.h"

MQTTManager::MQTTManager(I2CRuntime &runtime)
: mRuntime(runtime)
, mPreferences()
, mWiFiClient()
, mPubSub(mWiFiClient)
, mScheduler()
, mScheduleTickId()
, mUUID()
, mTXUUID()
, mRXUUID()
, mIsSubscribed(false)
{
    mScheduleTickId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&MQTTManager::tick, this)),
        1000,
        true
    );

    mRuntime.setPayloadFunc(std::make_shared<PayloadFunc>(
        std::bind(
            &MQTTManager::txPayload,
            this,
            std::placeholders::_1,
            std::placeholders::_2,
            std::placeholders::_3,
            std::placeholders::_4
        )
    ));
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

    mPubSub.setCallback(std::bind(
        &MQTTManager::onPayload,
        this,
        std::placeholders::_1,
        std::placeholders::_2,
        std::placeholders::_3
    ));
}


void MQTTManager::loop() {
    mScheduler.loop();
    mPubSub.loop();
}


void MQTTManager::tick() {
    if (mPubSub.state() != MQTT_CONNECTED) {
        this->attemptConnection();
    } else if (!mIsSubscribed && mPubSub.state() == MQTT_CONNECTED) {
        this->subscribe();
    }
}


void MQTTManager::subscribe() {
    mIsSubscribed = mPubSub.subscribe(mRXUUID);
    mPubSub.subscribe("broadcast");
    if (!mIsSubscribed) {
        Serial.printf("%lu Unable to subscribe to %s topic: %d\n", millis(), mRXUUID, mPubSub.state());
    }
}


void MQTTManager::onPayload(char * topic, uint8_t * payload, unsigned int size) {
    Serial.printf("%lu MQTT payload from topic: %s\n", millis(), topic);
    Peripheral *p = TelemetryProtocol::provisioning(payload, size);
    if (p != NULL) {
        mRuntime.addPeripheral(p);
    }
}


void MQTTManager::attemptConnection() {
    if (!WiFi.isConnected()) {
        return;
    }
    String host = mPreferences.getString("mqtt_host");
    String user = mPreferences.getString("mqtt_user");
    String pass = mPreferences.getString("mqtt_password");
    uint32_t port = mPreferences.getUInt("mqtt_port", 0);
    if (host.isEmpty() || user.isEmpty() || pass.isEmpty() || !port) {
        Serial.printf("%lu Incomplete MQTT connection details in preferences.\n", millis());
        return;
    }

    Serial.printf("%lu Attempting MQTT connection\n", millis());
    mPubSub.setServer(host.c_str(), port);
    bool status = mPubSub.connect(mUUID, user.c_str(), pass.c_str());
    if (status) {
        this->txRegistration(NULL);
        this->subscribe();
    }
}


void MQTTManager::txRegistration(std::vector<PeripheralStatus> *statuses) {
    if (mPubSub.state() != MQTT_CONNECTED) {
        return;
    }

    Serial.printf("%lu Sending registration %d\n", millis(), statuses == NULL ? 0 : statuses->size());
    uint8_t buffer[1024];
    size_t len = TelemetryProtocol::registration(statuses, buffer);
    if (len) {
        this->publish(buffer, len);
    }
}


void MQTTManager::txPayload(uint32_t busId, uint16_t busAddress, ReadDefinition *def, uint8_t *payload) {
    Serial.printf("%lu Sending payload\n", millis());
    uint8_t buffer[1024];
    size_t len = TelemetryProtocol::payload(busId, busAddress, def, payload, buffer);
    if (len) {
        this->publish(buffer, len);
    }
}


bool MQTTManager::publish(uint8_t *payload, unsigned int len) {
    return mPubSub.publish(mTXUUID, payload, len);
}


bool MQTTManager::publish(char *payload) {
    return mPubSub.publish(mTXUUID, payload);
}


bool MQTTManager::publish(std::string payload) {
    return this->publish(payload.c_str());
}
