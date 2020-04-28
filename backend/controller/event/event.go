package event

import (
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

// Start will start the main event engine. This is responsible for performing
// networked I/O with AMQP, MQTT, PostgreSQL, and Websockets.
func Start(s *state.State) error {
	/*
	 * everything below will be soon removed.
	 */

	/*
	 * test MQTT connection
	 */
	topics := []string{"test/#"}
	err := s.MQTT.UpdateTopics(topics)
	if err != nil {
		s.Log.Fatal(err)
	}
	// consume from MQTT
	go func() {
		for msg := range s.MQTTCh {
			s.Log.Printf("mqtt [%s] %s\n", msg.Topic, msg.Payload)
		}
	}()
	// publish to MQTT
	go func() {
		for {
			time.Sleep(2 * time.Second)
			t := time.Now().UTC().Format(time.RFC3339)
			err := s.MQTT.Publish("test/a", []byte(t))
			if err != nil {
				s.Log.Error(err)
			}
		}
	}()

	/*
	 * test AMQP connection
	 */
	routes := map[string][]string{
		// messages published with routing keys "test1" or "test2" will be sent
		// to queue testA
		"testA": {"test1", "test2"},
		"testB": {"test3", "test4"},
	}
	err = s.AMQP[0].UpdateRouting(routes)
	if err != nil {
		s.Log.Fatal(err)
	}
	// consume from AMQP
	go func() {
		for msg := range s.AMQPCh[0] {
			s.Log.Printf("amqp [%s] %s\n", msg.RoutingKey, msg.Payload)
		}
	}()
	// publish to AMQP
	go func() {
		for {
			time.Sleep(2 * time.Second)
			t := time.Now().UTC().Format(time.RFC3339)
			err := s.AMQP[1].Publish("test1", []byte(t))
			if err != nil {
				s.Log.Error(err)
			}
			err = s.AMQP[1].Publish("test3", []byte(t))
			if err != nil {
				s.Log.Error(err)
			}
		}
	}()
	return nil
}
