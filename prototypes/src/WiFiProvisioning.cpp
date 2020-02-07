#include <Arduino.h>
#include <Preferences.h>
#include <WiFi.h>
#include <WiFiAP.h>
#include <map>
#include <string>

#include "WiFiProvisioning.h"

WiFiProvisioning::WiFiProvisioning()
: mServer(PROVISIONING_PORT)
, mClient()
, mHasConnectedClient(false)
, mRequestBuffer()
, mPreferences()
{}

void WiFiProvisioning::setup() {
    mPreferences.begin("telemetry", false);
    Serial.println(mPreferences.getString("ssid").c_str());
    Serial.println(mPreferences.getString("password").c_str());
    Serial.println(mPreferences.getString("mqtt_host").c_str());
    Serial.println(mPreferences.getUInt("mqtt_port"));

    WiFi.softAP("TelemetryMicrocontroller", "4zp6capstone");
    WiFi.softAPenableIpV6();
    Serial.printf(
        "%lu Access Point: %s %s\n",
        millis(),
        WiFi.softAPIP().toString().c_str(),
        WiFi.softAPIPv6().toString().c_str()
    );

    mServer.begin();
}

void WiFiProvisioning::stopClient() {
    Serial.printf("%lu Closing client connection\n", millis());
    mClient.stop();
    mHasConnectedClient = false;
    mRequestBuffer.clear();
}

void WiFiProvisioning::loop() {
    if (!mHasConnectedClient) {
        // Why does WiFiServer::available not return a pointer?? Its internal
        // members that are expensive are pointers at least.
        mClient = mServer.available();
        if (!mClient) {
            return;
        }
        Serial.printf("%lu Client connected: %s\n", millis(), mClient.remoteIP().toString().c_str());
        mHasConnectedClient = true;
        mRequestBuffer.clear();
    }

    if (!mClient.connected()) {
        this->stopClient();
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

    return mRequestBuffer.size() - (delimIndex + 4) == atoi(value.c_str());

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
    }
}

void WiFiProvisioning::viewGet() {
    mClient.println("HTTP/1.1 200 OK");
    mClient.println("Content-Type: text/html");
    mClient.println();
    mClient.println(
#include "index.html._cc"
    );
    mClient.println();
}

void WiFiProvisioning::viewPost() {
    std::size_t delimIndex = mRequestBuffer.find("\r\n\r\n");
    std::map<std::string, std::string> payload;
    std::string key = "";
    bool isValue = false;
    for (uint8_t i = delimIndex + 4; i < mRequestBuffer.size(); i++) {
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
    mPreferences.putUInt("mqtt_port", (uint32_t) atoi(payload["port"].c_str()));

    mClient.println("HTTP/1.1 200 OK");
    mClient.println("Content-Type: text/html");
    mClient.println();
    mClient.println("Success");
    mClient.println();
}
