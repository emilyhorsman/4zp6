package main

import (
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
)

var (
	// device is a simulated microcontroller for testing backend.
	device = Device{
		Firmware: 1,
		UUID:     "aabbccddee",
		IPv4:     "127.0.0.1",
		IPv6:     "::1",
		Peripherals: []Peripheral{
			{
				BusID:    0x1,
				BusAddr:  0x44,
				CallResp: []byte{0x1, 0x2, 0x3},
			},
		},
	}

	// broker is the simulated microcontroller configuration (MQTT broker).
	broker     = "tcp://127.0.0.1:1883"
	brokerUser = "4zp6"
	brokerPass = "i0tmqtt"

	// txRoute is publishing route
	txRoute = "tx/" + device.UUID

	// rxRoute is subscribing route
	rxRoute = "rx/" + device.UUID

	// indicator if microcontroller is provisioned (always false on boot)
	provisioned = false

	// schedule interval is how often the schedule runs
	scheduleInterval = 5 * time.Second

	log = logrus.New()
)

// Device simulates a microcontroller.
type Device struct {
	Firmware    int          // Firmware version
	UUID        string       // UUID is MAC address
	IPv4        string       // IPv4 address
	IPv6        string       // IPv6 address
	Peripherals []Peripheral // Peripherals is list of emulated peripherals
}

// Peripheral simulates a connected peripheral.
type Peripheral struct {
	BusID    int    // BusID is I2C bus identifier
	BusAddr  int    // BusAddr is I2C bus address
	CallResp []byte // CallResponse is I2C general call response
}

// main simulates a microcontroller.
func main() {
	// connect to MQTT
	var mqtt state.MQTT
	incoming, err := mqtt.Init(log, broker, brokerUser, brokerPass)
	if err != nil {
		log.Panic(err)
	}
	err = mqtt.Connect()
	if err != nil {
		log.Panic(err)
	}

	// subscribe to UUID topic for receiving messages
	mqtt.UpdateTopics([]string{
		rxRoute,
	})

	// listen for incoming messages
	go func(log *logrus.Logger, incoming chan state.MQTTMessage) {
		log.Println("listening for RX")
		for {
			// attempt to unmarshal incoming message
			wire := <-incoming
			var msg telemetry.Telemetry
			err := proto.Unmarshal(wire.Payload, &msg)
			// break current iteration if not valid
			if err != nil {
				log.Errorln("error RX from", wire.Topic, err)
				continue
			}
			// route message on telemetry type
			switch msg.Message {
			case telemetry.Telemetry_PROVISIONING:
				log.Println("RX_Provisioning on", wire.Topic)
				rxProvisioning(wire, msg)
			case telemetry.Telemetry_REQUEST:
				log.Println("RX_Request on", wire.Topic)
				rxRequest(wire, msg)
			default:
				log.Println("RX unsupported message type", msg.Message, "on", wire.Topic)
			}
		}
	}(log, incoming)

	// send registration message
	time.Sleep(500 * time.Millisecond)
	err = txRegistration(mqtt)
	if err != nil {
		log.Panic(err)
	}

	// start runtime schedule
	log.Println("starting schedule, running every", scheduleInterval)
	for {
		// only run schedule if device is provisioned
		if provisioned {
			log.Println("device provisioned, running schedule")
			schedule(mqtt)
		} else {
			log.Println("device not provisioned, sleeping for", scheduleInterval)
		}
		time.Sleep(scheduleInterval)
	}
}

// schedule is called when the time-driven schedule is to run.
func schedule(mqtt state.MQTT) {
	// generate data to send (current time)
	t := time.Now().UTC().Format(time.RFC3339)

	// attach data to payload
	payload := telemetry.Payload{
		BusId:        0x1,
		BusAddr:      0x44,
		DefinitionId: 0x0,
		Data:         []byte(t),
	}
	// attach payload to frame
	frame := &telemetry.Telemetry{
		Message: telemetry.Telemetry_PAYLOAD,
		Payload: &payload,
	}
	// generate binary frame payload
	binary, err := proto.Marshal(frame)
	if err != nil {
		log.Panic(err)
	}
	// publish payload
	err = mqtt.Publish(txRoute, binary)
	if err != nil {
		log.Panic(err)
	}
	log.Printf("sent TX_Payload %+v\n", frame)
}

// rxProvisioning is called when receiving a provisioning frame. It provisions
// the state of the microcontroller.
func rxProvisioning(wire state.MQTTMessage, msg telemetry.Telemetry) {
	// device is now provisioned
	provisioned = true
	log.Printf("%+v\n", msg.Provisioning)
}

// rxRequest is called when receiving a request frame. It is for performing one
// off requests of the microcontroller.
func rxRequest(wire state.MQTTMessage, msg telemetry.Telemetry) {
	log.Printf("%+v\n", msg.Request)
}

// txRegistration sends a registration frame to the backend. If the frame cannot
// be generated or sent, an error will be returned.
func txRegistration(mqtt state.MQTT) error {
	log.Println("generating TX_Registration")

	// iterate over all device peripherals, convert to telemetry peripheral
	var peripherals []*telemetry.Registration_Peripheral
	for _, p := range device.Peripherals {
		// generate peripheral, append to list
		peripheral := &telemetry.Registration_Peripheral{
			BusId:           uint32(p.BusID),
			BusAddr:         uint32(p.BusAddr),
			GeneralCallResp: p.CallResp,
		}
		peripherals = append(peripherals, peripheral)
	}
	// attach peripherals to registration
	registration := telemetry.Registration{
		Version:     uint32(device.Firmware),
		Uuid:        []byte(device.UUID),
		Ipv4:        []byte(device.IPv4),
		Ipv6:        []byte(device.IPv6),
		Peripherals: peripherals,
	}
	// attach registration to frame
	frame := &telemetry.Telemetry{
		Message:      telemetry.Telemetry_REGISTRATION,
		Registration: &registration,
	}
	// generate binary frame payload
	binary, err := proto.Marshal(frame)
	if err != nil {
		return err
	}
	// publish payload
	err = mqtt.Publish(txRoute, binary)
	if err != nil {
		return err
	}
	log.Println("sent TX_Registration")
	return nil
}
