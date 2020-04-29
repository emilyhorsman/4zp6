package event

import (
	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

// Start will start the main event engine. This is responsible for performing
// networked I/O with AMQP, MQTT, PostgreSQL. Also responsible for publishing
// onto Websockets output channel in state.
func Start(s *state.State) error {
	// subscribe to microcontroller MQTT topics
	topics := []string{"tx/#"}
	err := s.MQTT.UpdateTopics(topics)
	if err != nil {
		s.Log.Fatal(err)
	}

	// subscribe to AMQP routes (route tx. messages to sensorData queue)
	routes := map[string][]string{
		"engineConfig": {"global.config"},
		"engineData":   {"data.#"},
	}
	err = s.AMQP[0].UpdateRouting(routes)
	if err != nil {
		s.Log.Fatal(err)
	}

	// consume from MQTT + AMQP
	go consumeMQTT(s)
	go consumeAMQP(s)

	return nil
}
