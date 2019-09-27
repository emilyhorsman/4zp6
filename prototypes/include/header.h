#ifndef CLIENT_HEADER
#define CLIENT_HEADER

#include <Arduino.h>
#include <WiFi.h>
#include <PubSubClient.h>

// MQTT receive and transmit routes
#define MQTT_RX "4zp6_rx/"
#define MQTT_TX "4zp6_tx/"

// Unique device identifier
String uuid;

// MQTT client
WiFiClient esp32Wifi;
PubSubClient mqtt(esp32Wifi);

/* Connectivity + MQTT */
void connect_wifi();
void connect_mqtt();
void mqtt_tx_hello();
void mqtt_tx_data();
void mqtt_rx(char* topic, byte* payload, unsigned int msg_length);

/* Helpers */
String normalize_mac(String str);

#endif
