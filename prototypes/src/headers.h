#ifndef CLIENT_HEADERS
#define CLIENT_HEADERS

#include <Arduino.h>
#include <WiFi.h>
#include <PubSubClient.h>

// Unique device identifier
String uuid;

// MQTT client
WiFiClient esp32Wifi;
PubSubClient mqtt(esp32Wifi);

/* Connectivity */
void connect_wifi();
void connect_mqtt();
void mqtt_rx(char* topic, byte* payload, unsigned int msg_length);

/* Helpers */
String normalize_mac(String str);

#endif
