#include <Arduino.h>
#include <WiFi.h>
#include <WiFiAP.h>

#include "WiFiProvisioning.h"

WiFiProvisioning::WiFiProvisioning()
: mServer(PROVISIONING_PORT)
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

void WiFiProvisioning::loop() {
    WiFiClient client = mServer.available();
    if (!client) {
        return;
    }

    Serial.printf("%lu Client connected: %s\n", millis(), client.localIP().toString().c_str());

    while (client.connected()) {
        if (!client.available()) {
            continue;
        }
        char c = client.read();
        if (c != '\n') {
            continue;
        }

        client.println("HTTP/1.1 200 OK");
        client.println("Content-Type: text/html");
        client.println();
        client.println("hello");
        client.println();
        break;
    }

    client.stop();
}