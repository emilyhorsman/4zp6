package main

import (
	mqtt "backend/mqtt"
	telemetry "backend/telemetry"
	"log"
	"time"

	"github.com/golang/protobuf/proto"
)

func main() {
	// initalize client (subscribe to "4zp6_tx/#" messages)
	m, out, err := mqtt.Init("tcp://broker.hivemq.com:1883", "4zp6_tx/#")
	if err != nil {
		// please don't actually do this
		panic(err)
	}
	// establish connection
	err = m.Connect()
	if err != nil {
		// please don't actually do this
		panic(err)
	}

	// test loops
	go highLowLoop(m)
	go helloLoop(m)

	// consume messages from devices (blocking)
	for msg := range out {
		log.Printf("[%s] %+v\n", msg.Mac, msg.Payload)
	}
}

// helloLoop sends a RX_HELLO message to all devices every 15 seconds.
func helloLoop(m *mqtt.Client) {
	for {
		msg := &telemetry.Telemetry{
			Type: telemetry.Telemetry_RX_HELLO,
		}
		out, err := proto.Marshal(msg)
		if err != nil {
			panic(err)
		}
		log.Println("Sending RX_HELLO")
		err = m.Publish("4zp6_rx/", out)
		if err != nil {
			panic(err)
		}
		time.Sleep(15 * time.Second)
	}
}

// highLowLoop sends a RX_ACTION every 5 seconds to notify all devices to
// alternate between DIGITAL_HIGH and DIGITAL_LOW on pin 13.
func highLowLoop(m *mqtt.Client) {
	for {
		// notfiy all devices to digitalWrite pin 13 to high
		msg := &telemetry.Telemetry{
			Type: telemetry.Telemetry_RX_ACTION,
			Action: &telemetry.MsgAction{
				Type: telemetry.MsgAction_DIGITAL_HIGH,
				Pin:  13,
			},
		}
		out, err := proto.Marshal(msg)
		if err != nil {
			panic(err)
		}
		log.Println("Sending DIGITAL_HIGH on 13")
		err = m.Publish("4zp6_rx/", out)
		if err != nil {
			panic(err)
		}
		time.Sleep(5 * time.Second)

		// notfiy all devices to digitalWrite pin 13 to low
		msg = &telemetry.Telemetry{
			Type: telemetry.Telemetry_RX_ACTION,
			Action: &telemetry.MsgAction{
				Type: telemetry.MsgAction_DIGITAL_LOW,
				Pin:  13,
			},
		}
		out, err = proto.Marshal(msg)
		if err != nil {
			panic(err)
		}
		log.Println("Sending DIGITAL_LOW on 13")
		err = m.Publish("4zp6_rx/", out)
		if err != nil {
			panic(err)
		}
		time.Sleep(5 * time.Second)
	}
}
