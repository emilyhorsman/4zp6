#include <Arduino.h>
#include <WiFi.h>
#include <WiFiAP.h>

#include "WiFiProvisioning.h"

WiFiProvisioning::WiFiProvisioning()
: mServer(PROVISIONING_PORT)
, mClient()
, mHasConnectedClient(false)
{}

void WiFiProvisioning::setup() {
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
    }

    if (!mClient.connected()) {
        this->stopClient();
        return;
    }

    if (!mClient.available()) {
        return;
    }

    char c = mClient.read();
    Serial.printf("%c", c);
    if (c == '\n') {
        mClient.println("HTTP/1.1 200 OK");
        mClient.println("Content-Type: text/html");
        mClient.println();
        mClient.println("hello");
        mClient.println();
        this->stopClient();
    }
}