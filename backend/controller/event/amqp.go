package event

import "github.com/emilyhorsman/4zp6/backend/controller/state"

// consuemAMQP consumes the AMQP output channel found in state. It is
// responsible for processing incoming AMQP messages.
func consumeAMQP(s *state.State) {
	for {
		msg := <-s.AMQPCh[0]
		s.Log.Printf("amqp [%s] %s\n", msg.RoutingKey, msg.Payload)
	}

	/*
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
	*/
}
