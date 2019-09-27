package mqtt

import (
	"crypto/rand"
	"fmt"
	"log"
	"net/url"
	"strings"
	"time"

	telemetry "backend/telemetry"

	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/golang/protobuf/proto"
)

// Client is the MQTT interface.
type Client struct {
	options  *mqtt.ClientOptions // MQTT client options
	internal mqtt.Client         // MQTT internal client
	ch       chan Message        // MQTT message output channel
	tx       string              // MQTT subscribe topic
}

// Message represents a Telemetry message.
type Message struct {
	Mac     string              // Mac is the client MAC address
	Payload telemetry.Telemetry // Payload is the cleint
}

// Init takes a hostanme and a topic. It initializes a new Client, returning a
// MQTT Client and output channel, or an error. Incoming messages received on
// the provided topic will be sent to the output channel.
func Init(host, tx string) (*Client, chan Message, error) {
	var c Client
	c.tx = tx

	// parse server host
	uri, err := url.Parse(host)
	if err != nil {
		return nil, nil, err
	}

	// generate random identifier for client ID
	uuid := make([]byte, 8)
	rand.Read(uuid)

	// MQTT client options
	c.options = &mqtt.ClientOptions{
		Servers:              []*url.URL{uri},
		ClientID:             fmt.Sprintf("%x", uuid),
		Username:             "",
		Password:             "",
		CleanSession:         true,
		Order:                true,
		WillEnabled:          false,
		WillTopic:            "",
		WillPayload:          nil,
		WillQos:              0,
		WillRetained:         false,
		ProtocolVersion:      0,
		KeepAlive:            15,               // how often to ping server
		PingTimeout:          10 * time.Second, // timeout for ping response
		ConnectTimeout:       5 * time.Second,  // timeout for initial connection
		MaxReconnectInterval: 5 * time.Second,  // delay between reconnects
		AutoReconnect:        true,
		Store:                nil,
		DefaultPublishHandler: func(internal mqtt.Client, raw mqtt.Message) {
			triggerMessage(raw, c.ch) // trigged on incoming messages
		},
		OnConnect: func(internal mqtt.Client) {
			triggerConnect(internal, c.tx)
		}, // triggered on connection
		OnConnectionLost:    triggerDisconnect, // trigger on disconnection
		WriteTimeout:        0,
		MessageChannelDepth: 100, // inflight messages during reconnect
		ResumeSubs:          false,
		HTTPHeaders:         make(map[string][]string),
	}

	// initialize output channel (buffer of 512 messages)
	c.ch = make(chan Message, 512)

	// import options
	c.internal = mqtt.NewClient(c.options)
	return &c, c.ch, nil
}

// Connect will estabish the connection to the MQTT. It may return a connection
// error.
func (c *Client) Connect() error {
	// connect to MQTT server
	token := c.internal.Connect()
	if token.Wait() && token.Error() != nil {
		return token.Error()
	}
	return nil
}

// Close will close the connection to the MQTT server.
func (c *Client) Close() {
	// notify the broker, wait 1 second, then close connection
	c.internal.Disconnect(1000)
	log.Println("[MQTT] Connection closed")
}

// Publish will send the payload with the provided topic string to the broker.
// It may return an error if the payload cannot be sent.
func (c *Client) Publish(topic string, payload []byte) error {
	token := c.internal.Publish(topic, 0, false, payload)
	if token.Wait() && token.Error() != nil {
		return token.Error()
	}
	return nil
}

// triggerConnect is called on MQTT connection. It will subscribe the internal
// MQTT client to the topic provided at initialization.
func triggerConnect(internal mqtt.Client, topic string) {
	// subscribe to sensor MQTT_TX topic
	token := internal.Subscribe(topic, 0, nil)
	if token.Wait() && token.Error() != nil {
		log.Println("[MQTT] Error:", token.Error())
		return
	}
	log.Println("[MQTT] Connection established")
}

// triggerDisconnect is called on MQTT disconnect.
func triggerDisconnect(c mqtt.Client, err error) {
	log.Println("[MQTT] Connection lost, retrying...")
}

// triggerMessage is called on incoming messages. It will attempt to convert a
// raw MQTT message into a Telemetry struct. If successful, the message will be
// emitted on the output channel.
func triggerMessage(raw mqtt.Message, out chan Message) {
	// attempt to decode the payload
	msg := &telemetry.Telemetry{}
	err := proto.Unmarshal(raw.Payload(), msg)
	if err != nil {
		log.Println("[MQTT] Incoming message not valid")
		return
	}
	// decode incoming topic (e.g. "4zp6_tx/MAC")
	parts := strings.Split(raw.Topic(), "/")
	if len(parts) != 2 || parts[1] == "" {
		log.Println("[MQTT] Incoming message not valid")
		return
	}
	// assemble message + send to output channel
	out <- Message{
		Mac:     parts[1], // mac address from topic
		Payload: *msg,     // payload
	}
}
