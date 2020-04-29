package event

import (
	"encoding/json"
	"strconv"
	"strings"

	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
	"google.golang.org/protobuf/proto"
)

var (
	// map of registered microcontroller UUIDs
	registered map[string]bool
)

// consuemMQTT consumes the MQTT output channel found in state. It is
// responsible for processing incoming MQTT messages.
func consumeMQTT(s *state.State) {
	// get map of registered microcontrollers
	reg, err := db.GetRegistered(s)
	if err != nil {
		s.Log.Fatal(err)
	}
	registered = reg

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
			s.Log.Printf("[mqtt] RX unsupported message type %s on %s", msg.Message, wire.Topic)
		}
	}
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

	// update registration map entry
	registered[string(reg.Uuid)] = true

	// for each peripheral sent, send a provisioning profile (if exists)
	for _, p := range reg.Peripherals {
		// query profile by bus address
		profile, found, err := db.QueryProvisioning(s, p.BusAddr)
		if err != nil {
			s.Log.Error(err)
		}
		if !found {
			// no point in sending blank profile
			s.Log.Warnf("[mqtt] no provisioning profiles available %s, busAddr 0x%x", string(reg.Uuid), p.BusAddr)
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
		s.Log.Printf("[mqtt] TX_Provisioning %s, busAddr 0x%x", string(reg.Uuid), p.BusAddr)
	}
}

// rxPayload is called when receiving a payload frame. If the microcontroller is
// not yet registered, it will send a TX_Request to the microcontroller
// requesting a registration frame. If the device is registered, it will forward
// the raw data to AMQP for processing.
func rxPayload(s *state.State, msg *telemetry.Telemetry, wire *state.MQTTMessage) {
	// get UUID
	parts := strings.Split(wire.Topic, "/")
	if len(parts) != 2 {
		s.Log.Errorln("[mqtt] invalid routing key", wire.Topic)
	}
	uuid := parts[1]

	// request registraiton if device not registered
	_, ok := registered[uuid]
	if !ok {
		s.Log.Warnln("[mqtt]", uuid, "has not registered, sending TX_Request for Registration")
		// send request registration frame
		frame := &telemetry.Telemetry{
			Message: telemetry.Telemetry_REQUEST,
			Request: &telemetry.Request{
				Action: telemetry.Request_REQUEST_REGISTRATION,
			},
		}
		// generate binary frame payload
		binary, err := proto.Marshal(frame)
		if err != nil {
			s.Log.Error(err)
		}
		// publish payload (send it back to sender)
		err = s.MQTT.Publish("rx/"+uuid, binary)
		if err != nil {
			s.Log.Error(err)
		}
		return
	}

	// conert message payload to JSON
	buff, err := json.Marshal(msg.Payload)
	if err != nil {
		s.Log.Error(err)
	}

	s.Log.Println("[tanner]", string(buff))

	// routing key: controller.addr.uuid
	route := "controller." + strconv.Itoa(int(msg.Payload.BusAddr)) + "." + uuid

	// send JSON data over AMQP
	err = s.AMQP[1].Publish(route, buff)
	if err != nil {
		s.Log.Errorln("[amqp] error publishing raw data", err)
	}
	s.Log.Println("[amqp] TX_Payload raw on", route)
}
