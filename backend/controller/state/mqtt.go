package state

import (
	"net/url"
	"time"

	mqtt "github.com/eclipse/paho.mqtt.golang"
	log "github.com/sirupsen/logrus"
)

// MQTT is the MQTT client.
type MQTT struct {
	topics      []string            // Topics to subscribe
	isConnected bool                // isConnected indicate if MQTT connection is currently established
	options     *mqtt.ClientOptions // options is internal client options
	client      mqtt.Client         // client is internal client
	ch          chan MQTTMessage    // ch is the message output channel, exposed by Init()
	log         *log.Logger         // log is state structured event logger
}

// MQTTMessage is a message sent over MQTT.
type MQTTMessage struct {
	Topic   string // Topic is the topic the message was published on
	Payload []byte // Payload is the message payload
}

// Init will attempt to initialize the MQTT client. It
// requires the MQTT broker's host, username, and password.
// It will return the output channel or an error.
func (m *MQTT) Init(log *log.Logger, host, user, pass string) (chan MQTTMessage, error) {
	// can not directly access state.Log
	m.log = log

	uri, err := url.Parse(host)
	if err != nil {
		return nil, err
	}
	// MQTT client options
	m.options = &mqtt.ClientOptions{
		Servers:              []*url.URL{uri},
		ClientID:             "",
		Username:             user,
		Password:             pass,
		CleanSession:         true,
		Order:                true,
		WillEnabled:          false,
		WillTopic:            "",
		WillPayload:          nil,
		WillQos:              0,
		WillRetained:         false,
		ProtocolVersion:      0,
		KeepAlive:            15,               // how often to ping broker
		PingTimeout:          10 * time.Second, // timeout for ping response
		ConnectTimeout:       5 * time.Second,  // timeout for initial connection
		MaxReconnectInterval: 5 * time.Second,  // delay between reconnects
		AutoReconnect:        true,
		Store:                nil,
		DefaultPublishHandler: func(client mqtt.Client, raw mqtt.Message) {
			triggerMessage(m, raw) // trigged on incoming message
		},
		OnConnect: func(client mqtt.Client) {
			triggerConnect(m, client) // triggered on connection
		},
		OnConnectionLost: func(client mqtt.Client, err error) {
			triggerDisconnect(m) // trigger on disconnection
		},
		WriteTimeout:        0,
		MessageChannelDepth: 100, // inflight messages during reconnect
		ResumeSubs:          false,
		HTTPHeaders:         make(map[string][]string),
	}
	// initialize output channel (buffer of 128 messages)
	m.ch = make(chan MQTTMessage, 128)
	// import options
	m.client = mqtt.NewClient(m.options)
	return m.ch, nil
}

// GetTopics returns the list of currently subscribed
// topics.
func (m *MQTT) GetTopics() []string {
	return m.topics
}

// UpdateTopics repleaces the subscribe topics list with the
// one provided. It will notify the MQTT broker of the
// changes. It may return an error if the transaction fails
// to complete.
func (m *MQTT) UpdateTopics(topics []string) error {
	m.topics = topics
	// only notify if connected
	if m.isConnected {
		return m.notifyTopics()
	}
	return nil
}

// notifyTopics will notify the MQTT broker of the topics to
// subscribe to. It will return an error if the topics fail
// to bind.
func (m *MQTT) notifyTopics() error {
	// unsubscribe existing topics, subscribe new
	if len(m.topics) > 0 {
		m.client.Unsubscribe(m.topics...)
	}
	for _, topic := range m.topics {
		token := m.client.Subscribe(topic, 0, nil)
		if token.Wait() && token.Error() != nil {
			return token.Error()
		}
		m.log.Infof("[mqtt] subscribed to topic '%s'", topic)
	}
	return nil
}

// Connect will attempt to establish a connection with the
// MQTT broker. It will return an error if the connection
// fails to establish.
func (m *MQTT) Connect() error {
	m.log.Info("[mqtt] connecting to broker")
	token := m.client.Connect()
	if token.Wait() && token.Error() != nil {
		return token.Error()
	}
	return nil
}

// Close will close the connection to the MQTT broker.
func (m *MQTT) Close() {
	// wait 1 second and close connection
	m.client.Disconnect(1000)
	m.isConnected = false
	m.log.Info("[mqtt] connection to broker closed")
}

// Publish will publish a payload on the provided topic. It
// will return an error if the payload fails to publish.
func (m *MQTT) Publish(topic string, payload []byte) error {
	token := m.client.Publish(topic, 0, false, payload)
	if token.Wait() && token.Error() != nil {
		return token.Error()
	}
	return nil
}

// triggerConnect is called on MQTT connection. It updates
// the internal connection status and triggers this client
// to notify the broker of all topics.
func triggerConnect(m *MQTT, client mqtt.Client) {
	m.isConnected = true
	m.log.Info("[mqtt] connected to broker, subscribing to topics ...")
	err := m.notifyTopics()
	if err != nil {
		m.log.Error("[mqtt] failed to subscribe to topics ", err)
	}
}

// triggerDisconnect is called on MQTT disconnect.
func triggerDisconnect(m *MQTT) {
	m.log.Error("[mqtt] connection to broker lost, retrying ...")
}

// triggerMessage is called on incoming MQTT message. It
// will emit an MQTTMessage on the out channel.
func triggerMessage(m *MQTT, raw mqtt.Message) {
	m.ch <- MQTTMessage{
		Topic:   raw.Topic(),
		Payload: raw.Payload(),
	}
}
