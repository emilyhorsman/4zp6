#include <Arduino.h>
#include <Preferences.h>
#include <WiFi.h>
#include <WiFiAP.h>
#include <functional>
#include <map>
#include <memory>
#include <string>

#include "WiFiProvisioning.h"

WiFiProvisioning::WiFiProvisioning()
: mServer()
, mClient()
, mHasConnectedClient(false)
, mRequestBuffer()
, mPreferences()
, mRequestStart(0)
, mIsNetworked(false)
, mConnectionAttempts(0)
, mScheduler()
, mScheduleTickId(0)
{
    mScheduleTickId = mScheduler.addSchedule(
        std::make_shared<Func>(std::bind(&WiFiProvisioning::tick, this)),
        1000,
        false
    );
}

void WiFiProvisioning::tick() {
    if (WiFi.isConnected()) {
        mScheduler.disableSchedule(mScheduleTickId);
        mIsNetworked = true;
        Serial.printf("%lu Connected as %s\n", millis(), WiFi.localIP().toString().c_str());
    }

    if (++mConnectionAttempts == 10) {
        Serial.printf("%lu Connection attempts exceeded.\n", millis());
        mScheduler.disableSchedule(mScheduleTickId);
        WiFi.disconnect();
        this->broadcastAP();
    }
}

bool WiFiProvisioning::tryConnectionFromPreferences() {
    String ssid = mPreferences.getString("ssid");
    String pass = mPreferences.getString("password");
    if (ssid.isEmpty() || pass.isEmpty()) {
        Serial.printf("%lu No SSID and password in preferences.\n", millis());
        return false;
    }

    Serial.printf("%lu Attempting connection to %s\n", millis(), ssid.c_str());
    WiFi.begin(ssid.c_str(), pass.c_str());
    mScheduler.enableSchedule(mScheduleTickId);
    return true;
}

void WiFiProvisioning::broadcastAP() {
    WiFi.softAP("TelemetryMicrocontroller", "4zp6capstone");
    WiFi.softAPenableIpV6();
    mIsNetworked = true;
    Serial.printf(
        "%lu Access Point: %s %s\n",
        millis(),
        WiFi.softAPIP().toString().c_str(),
        WiFi.softAPIPv6().toString().c_str()
    );
}

void WiFiProvisioning::setup() {
    mPreferences.begin("telemetry", false);

    if (!this->tryConnectionFromPreferences()) {
        this->broadcastAP();
    }

    mServer.begin();
}

void WiFiProvisioning::stopClient() {
    Serial.printf("%lu Closing client connection\n", millis());
    mClient.stop();
    mHasConnectedClient = false;
    mRequestBuffer.clear();
}

void WiFiProvisioning::loop() {
    mScheduler.loop();

    if (!mIsNetworked) {
        return;
    }

    if (!mHasConnectedClient) {
        // Why does WiFiServer::available not return a pointer?? Its internal
        // members that are expensive are pointers at least.
        mClient = mServer.available();
        if (!mClient) {
            return;
        }
        Serial.printf("%lu Client connected: %s\n", millis(), mClient.remoteIP().toString().c_str());
        mHasConnectedClient = true;
        mRequestStart = millis();
        mRequestBuffer.clear();
    }

    if (!mClient.connected() || mRequestStart + 1000 < millis()) {
        this->stopClient();
        Serial.printf("%lu Timed out.\n", millis());
        return;
    }

    if (mClient.available() == 0) {
        return;
    }

    mRequestBuffer += mClient.read();
    this->controller();
}

bool WiFiProvisioning::isPostRequestComplete() {
    std::size_t delimIndex = mRequestBuffer.find("\r\n\r\n");
    if (delimIndex == std::string::npos) {
        return false;
    }

    std::size_t contentLengthIndex = mRequestBuffer.find("Content-Length:");
    if (contentLengthIndex == std::string::npos) {
        return false;
    }

    std::string value = "";
    for (int i = contentLengthIndex + 16; i < delimIndex; i++) {
        if (mRequestBuffer[i] == '\r') {
            break;
        }

        value += mRequestBuffer[i];
    }

    return mRequestBuffer.size() - (delimIndex + 4) >= atoi(value.c_str());

}

void WiFiProvisioning::controller() {
    if (mRequestBuffer.find("GET / HTTP/1.1\r\n", 0, 16) == 0) {
        this->viewGet();
        this->stopClient();
        return;
    }

    if (
        mRequestBuffer.find("POST / HTTP/1.1\r\n", 0, 17) == 0 &&
        this->isPostRequestComplete()
    ) {
        this->viewPost();
        this->stopClient();
        return;
    }
}

void replace(std::string &haystack, std::string needle, const char * replacement) {
    std::size_t index = haystack.find(needle);
    if (index == std::string::npos) {
        return;
    }
    haystack.replace(index, needle.size(), replacement);
}

std::string WiFiProvisioning::getContent() {
#include "index.html._cc"
    replace(file, std::string("{ssid}"), mPreferences.getString("ssid").c_str());
    replace(file, std::string("{password}"), mPreferences.getString("password").c_str());
    replace(file, std::string("{mqtt_host}"), mPreferences.getString("mqtt_host").c_str());
    replace(file, std::string("{mqtt_user}"), mPreferences.getString("mqtt_user").c_str());
    replace(file, std::string("{mqtt_password}"), mPreferences.getString("mqtt_password").c_str());
    char tmp[6];
    snprintf(tmp, 6, "%d", mPreferences.getUInt("mqtt_port"));
    replace(file, std::string("{mqtt_port}"), tmp);

    return file;
}

void WiFiProvisioning::viewGet() {
    mClient.println("HTTP/1.1 200 OK");
    mClient.println("Content-Type: text/html");
    mClient.println();
    mClient.println(this->getContent().c_str());
    mClient.println();
}

void WiFiProvisioning::viewPost() {
    std::size_t delimIndex = mRequestBuffer.find("\r\n\r\n");
    if (delimIndex == std::string::npos) {
        Serial.printf("%lu not found, should never have gotten here\n", millis());
        return;
    }
    std::map<std::string, std::string> payload;
    std::string key = "";
    bool isValue = false;
    for (uint32_t i = delimIndex + 4; i < mRequestBuffer.size(); i++) {
        char c = mRequestBuffer[i];
        if (isValue && c == '&') {
            key = "";
            isValue = false;
        } else if (isValue) {
            payload[key] += c;
        } else if (!isValue && c == '=') {
            isValue = true;
            payload[key] = "";
        } else {
            key += c;
        }
    }

    mPreferences.putString("ssid", payload["ssid"].c_str());
    mPreferences.putString("password", payload["password"].c_str());
    mPreferences.putString("mqtt_host", payload["host"].c_str());
    mPreferences.putString("mqtt_user", payload["mqtt_user"].c_str());
    mPreferences.putString("mqtt_password", payload["mqtt_password"].c_str());
    mPreferences.putUInt("mqtt_port", (uint32_t) atoi(payload["port"].c_str()));

    mClient.println("HTTP/1.1 200 OK");
    mClient.println("Content-Type: text/html");
    mClient.println();
    mClient.println("Success");
    mClient.println();
}
