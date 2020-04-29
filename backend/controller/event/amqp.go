package event

import (
	"encoding/json"
	"strconv"
	"strings"
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

// consumeAMQP consumes the AMQP output channel found in state. It is
// responsible for processing incoming AMQP messages.
func consumeAMQP(s *state.State) {
	// testing only
	a := db.JSONConfig{
		BusAddr: 68,
		Name:    "SHT31",
		ReadDefinitions: []db.ReadDefinition{
			{
				DefinitionID:        1,
				RegisterIDLength:    16,
				RegisterID:          9216,
				RegisterBlockLength: 1,
				NumBytesPerRegister: 6,
				ReadPeriod:          500,
			},
		},
	}
	buff, _ := json.Marshal(a)
	_ = s.AMQP[1].Publish("global.config", buff)
	// testing only

	for {
		msg := <-s.AMQPCh[0]
		t := time.Now().UTC()

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
			continue
		}

		// testing only (controller receiving raw data)
		if parts[0] == "controller" {
			// parse parts
			addr := parts[1]
			uuid := parts[2]
			data := `{"key1":1,"key2":"A","key3":true}`
			err := s.AMQP[1].Publish("data."+addr+"."+uuid, []byte(data))
			if err != nil {
				s.Log.Errorln(err)
			}
			continue
		}
		// testing only

		// testing only (controller requested to send capabilities)
		if parts[0] == "global" && parts[1] == "req" {
			buff, _ := json.Marshal(a)
			_ = s.AMQP[1].Publish("global.config", buff)
			continue
		}
		// testing only
	}
}
