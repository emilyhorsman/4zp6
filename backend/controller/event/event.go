package event

import (
	"time"

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

	// notify peripheral processors to send provisioning config every 3 mintues
	go func(s *state.State) {
		delay := 3 * time.Minute
		for {
			s.Log.Println("[amqp] requesting processors to broadcast provisioning config, requesting again in", delay)
			err := s.AMQP[1].Publish("global.req", nil)
			if err != nil {
				s.Log.Errorln(err)
			}
			time.Sleep(delay)
		}
	}(s)

	// consume from MQTT + AMQP
	go consumeMQTT(s)
	go consumeAMQP(s)

	return nil
}
