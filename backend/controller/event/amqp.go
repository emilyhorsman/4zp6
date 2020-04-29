package event

import (
	"encoding/json"
	"strconv"
	"strings"
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
	"google.golang.org/protobuf/proto"
)

// consumeAMQP consumes the AMQP output channel found in state. It is
// responsible for processing incoming AMQP messages.
func consumeAMQP(s *state.State) {
	for {
		msg := <-s.AMQPCh[0]
		t := time.Now().UTC()

		s.Log.Println("[amqp] incoming message", msg.RoutingKey, string(msg.Payload))

		// parse routing key
		parts := strings.Split(msg.RoutingKey, ".")

		// if routing key begins with "data.", index the processed data
		if parts[0] == "data" {
			// parse parts
			addr := parts[1]
			uuid := parts[2]

			// convert string to integer
			addrVal, err := strconv.Atoi(addr)
			if err != nil {
				s.Log.Errorln(err)
			}
			// index the data
			err = db.IndexData(s, uuid, addrVal, t, msg.Payload)
			if err != nil {
				s.Log.Errorln("[amqp] error indexing processed data", err)
			}

			// convert payload into raw interface
			var buff interface{}
			err = json.Unmarshal(msg.Payload, &buff)
			if err != nil {
				s.Log.Errorln(err)
			}
			// encode data into a websocket JSON frame
			ws := state.WebsocketFrame{
				UUID:      uuid,
				BusAddr:   addrVal,
				Timestamp: t.Format(time.RFC3339),
				Data:      buff,
			}
			// generate the JSON, publish on Websocket channel
			frame, err := json.Marshal(ws)
			if err != nil {
				s.Log.Errorln(err)
			}
			s.Data <- frame
			s.Log.Printf("[amqp] indexed and published on websocket, uuid=%s, busAddr=0x%x, ts=%s, data=%s", uuid, addrVal, t, string(msg.Payload))
			continue
		}

		// if routing key is "global.config", add the data into database
		if parts[0] == "global" && parts[1] == "config" {
			// parse provided JSON configuration
			var config db.JSONConfig
			err := json.Unmarshal(msg.Payload, &config)
			if err != nil {
				s.Log.Errorln(err)
			}
			err = db.UpsertConfig(s, config)
			if err != nil {
				s.Log.Errorln(err)
			}
			// query profile by bus address
			profile, _, err := db.QueryProvisioning(s, uint32(config.BusAddr))
			if err != nil {
				s.Log.Error(err)
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
			// publish payload (broadcast to all controllers)
			err = s.MQTT.Publish("broadcast", binary)
			if err != nil {
				s.Log.Error(err)
			}
			s.Log.Printf("[mqtt] TX_Provisioning broadcast busAddr 0x%x", config.BusAddr)
			continue
		}
	}
}
