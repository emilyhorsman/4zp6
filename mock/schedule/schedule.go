package schedule

import (
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
	"github.com/gogo/protobuf/proto"
	"github.com/sirupsen/logrus"
)

var (
	l           = logrus.New()    // structured logger
	provisioned = make(chan bool) // channel indicating provision message has been received
	macAddress  = "aabbccddee"    // mac address of mock microcontroller
	m           *state.MQTT       // mqtt client
)

// Start a mock microcontroller loop.
func Start() {
	// establish mqtt connection
	client, out, err := mqtt(l)
	if err != nil {
		panic(err)
	}
	m = client
	defer m.Close()

	// MQTT connection on ESP32 blocks when initializing
	time.Sleep(1 * time.Second)

	// subscribe to MQTT topic (receive on MAC address)
	err = m.UpdateTopics([]string{
		"rx/" + macAddress,
	})
	if err != nil {
		panic(err)
	}

	// listen for MQTT messages
	go func(msgs chan state.MQTTMessage) {
		for msg := range msgs {
			rxMessage(msg)
		}
	}(out)

	// send registration frame
	txRegistration()

	// wait to receive provisioning frame before starting scheduling loop
	l.Println("waiting for provisioning frame")
	<-provisioned

	for {
		// poll sensors and send payload frame(s), sleep and repeat
		txPayload()
		time.Sleep(3 * time.Second)
	}
}

// Called when receiving incoming message.
func rxMessage(msg state.MQTTMessage) {
	// attempt to decode binary to telemetry instance
	var incoming telemetry.Telemetry
	err := proto.Unmarshal(msg.Payload, &incoming)
	if err != nil {
		l.Errorln("failed to unmarshal incoming message", err)
		return
	}

	// if provisioning message, we can begin main execution
	if incoming.Message == telemetry.Telemetry_PROVISIONING {
		l.Println("microcontroller successfully provisioned")
		provisioned <- true
		return
	}

	// if request message, perform requested action
	if incoming.Message == telemetry.Telemetry_REQUEST {
		switch incoming.Request.Action {
		case telemetry.Request_REBOOT:
			l.Println("[request] reboot")
		case telemetry.Request_FORCE_READS:
			l.Println("[request] force reads")
			txPayload()
		case telemetry.Request_FORCE_SCAN:
			l.Println("[request] force scan")
			txRegistration()
		case telemetry.Request_REQUEST_REGISTRATION:
			l.Println("[request] registration")
			txRegistration()
		case telemetry.Request_CLEAR_PREFERENCES:
			l.Println("[request] clear preferences")
		}
	}
}

// Transmit Registration frame.
func txRegistration() {
	// assemble peripherals
	peripherals := []*telemetry.Registration_Peripheral{
		&telemetry.Registration_Peripheral{
			BusId:           0x1,
			BusAddr:         0x44,
			GeneralCallResp: []byte("call resp"),
		},
		&telemetry.Registration_Peripheral{
			BusId:           0x1,
			BusAddr:         0x20,
			GeneralCallResp: []byte("call resp 2"),
		},
	}
	// assemble registration
	registration := &telemetry.Registration{
		Version:     1,
		Uuid:        []byte(macAddress),
		Ipv4:        []byte("127.0.0.1"),
		Ipv6:        []byte("::1"),
		Peripherals: peripherals,
	}
	// assemble frame
	frame := &telemetry.Telemetry{
		Message:      telemetry.Telemetry_REGISTRATION,
		Registration: registration,
	}
	// encode registration frame + publish
	buff, err := proto.Marshal(frame)
	if err != nil {
		panic(err)
	}
	err = m.Publish("tx/"+macAddress, buff)
	if err != nil {
		panic(err)
	}
	l.Println("registration frame sent")
}

// Transmit Payload frame.
func txPayload() {
	// assemble payload
	payload := &telemetry.Payload{
		BusId:        0x1,
		BusAddr:      0x44,
		DefinitionId: 0x1,
		Data:         []byte("raw data"),
	}
	// assemble frame
	frame := &telemetry.Telemetry{
		Message: telemetry.Telemetry_PAYLOAD,
		Payload: payload,
	}
	// encode payload frame + publish
	buff, err := proto.Marshal(frame)
	if err != nil {
		panic(err)
	}
	err = m.Publish("tx/"+macAddress, buff)
	if err != nil {
		panic(err)
	}
	l.Println("payload frame sent")
}
