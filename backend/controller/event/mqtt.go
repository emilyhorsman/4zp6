package event

import (
	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
	"google.golang.org/protobuf/proto"
)

// consuemMQTT consumes the MQTT output channel found in state. It is
// responsible for processing incoming MQTT messages.
func consumeMQTT(s *state.State) {
	for {
		// attempt to unmarshal incoming message
		wire := <-s.MQTTCh
		var msg telemetry.Telemetry
		err := proto.Unmarshal(wire.Payload, &msg)
		// break current iteration if not valid
		if err != nil {
			s.Log.Errorln("[mqtt] error RX from", wire.Topic, err)
			continue
		}
		// route message on telemetry type
		switch msg.Message {
		case telemetry.Telemetry_REGISTRATION:
			s.Log.Println("[mqtt] RX_Registration on", wire.Topic)
			rxRegistration(s, &msg, &wire)
		case telemetry.Telemetry_PAYLOAD:
			s.Log.Println("[mqtt] RX_Payload on", wire.Topic)
			rxPayload(s, &msg, &wire)
		default:
			s.Log.Println("[mqtt] RX unsupported message type", msg.Message, "on", wire.Topic)
		}
	}

	/*
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
	*/
}

// rxRegistration is called when receiving a registration frame. It will update
// the "registration" and "peripheral" tables with data found in the
// registration message. It then sends all available provisioning profiles back
// to the microconroller.
func rxRegistration(s *state.State, msg *telemetry.Telemetry, wire *state.MQTTMessage) {
	// update "registration" and "peripheral" tables
	err := db.UpsertRegistration(s, msg)
	if err != nil {
		s.Log.Error(err)
	}
	if len(msg.Registration.Peripherals) == 0 {
		s.Log.Info("[mqtt] no peripherals are connected to microcontroller")
	}

	reg := msg.Registration
	txRoute := "rx/" + string(reg.Uuid)

	// for each peripheral sent, send a provisioning profile (if exists)
	for _, p := range reg.Peripherals {
		// query profile by bus address
		profile, found, err := db.QueryProvisioning(s, p.BusAddr)
		if err != nil {
			s.Log.Error(err)
		}
		if !found {
			// no point in sending blank profile
			s.Log.Warnln("[mqtt] no provisioning proviles available", string(reg.Uuid), "busAddr", p.BusAddr)
			continue
		}
		// attach provisioning profile to frame
		frame := &telemetry.Telemetry{
			Message:      telemetry.Telemetry_PROVISIONING,
			Provisioning: profile,
		}
		// generate binary frame payload
		binary, err := proto.Marshal(frame)
		if err != nil {
			s.Log.Error(err)
		}
		// publish payload (send it back to sender)
		err = s.MQTT.Publish(txRoute, binary)
		if err != nil {
			s.Log.Error(err)
		}
		s.Log.Println("[mqtt] TX_Provisioning", string(reg.Uuid), "busAddr", p.BusAddr)
	}
}

// rxPayload is called when receiving a payload frame.
func rxPayload(s *state.State, msg *telemetry.Telemetry, wire *state.MQTTMessage) {
	s.Log.Printf("[mqtt] %+v\n", msg.Payload)
}
