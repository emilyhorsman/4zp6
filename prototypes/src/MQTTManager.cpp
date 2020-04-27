#include "MQTTManager.h"

MQTTManager::MQTTManager()
: mPreferences()
, mWiFiClient()
, mPubSub(mWiFiClient)
, mScheduler()
, mScheduleTickId()
{}


void MQTTManager::loop() {
    mPubSub.loop();
}
