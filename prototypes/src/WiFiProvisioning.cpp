#include <Arduino.h>
#include <string>
#include <WiFi.h>
#include <WiFiAP.h>

#include "WiFiProvisioning.h"

WiFiProvisioning::WiFiProvisioning()
: mServer(PROVISIONING_PORT)
, mClient()
, mHasConnectedClient(false)
, mRequestBuffer()
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

    if (!mClient.available()) {
        return;
    }

    mRequestBuffer += mClient.read();
    this->controller();
}

void WiFiProvisioning::controller() {
    if (mRequestBuffer.find("GET / HTTP/1.1\r\n", 0, 16) == 0) {
        this->viewGet();
        this->stopClient();
    }

    if (mRequestBuffer.find("POST / HTTP/1.1", 0, 17) == 0) {
        this->viewPost();
        return;
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
}