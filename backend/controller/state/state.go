package state

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	"github.com/joho/godotenv"
	_ "github.com/lib/pq" // postgres SQL driver
	log "github.com/sirupsen/logrus"
)

// State is the global state for the controller.
type State struct {
	Log      *log.Logger         // Log is a structured event logger
	IsDocker bool                // IsDocker indicates if running inside Docker
	Config   *Config             // Config contains global configuration
	MQTT     *MQTT               // MQTT is the MQTT client
	MQTTCh   chan MQTTMessage    // MQTTCh is the MQTT output channel
	AMQP     [2]*AMQP            // AMQP is the AMQP client (two for duplex)
	AMQPCh   [2]chan AMQPMessage // AMQPCh is the AMQP output channel
	SQL      *sql.DB             // SQL is the SQL client
	Data     chan []byte         // Data channel is populated by AMQP and consumed by websockets
}

// WebsocketFrame is a frame emitted on the websocket connection.
type WebsocketFrame struct {
	UUID      string      `json:"uuid"`      // UUID is the UUID of the microcontroller
	BusAddr   int         `json:"busAddr"`   // BusAddr is the bus address where data was collected
	Timestamp string      `json:"timestamp"` // Timestamp is time which data was received by peripheral controller
	Data      interface{} `json:"data"`      // Data is the JSON payload from peripheral controller
}

// Provision generates a global state for the controller. It
// will return an error if the state fails to initialize.
func Provision() (*State, error) {
	var s State

	s.Log = log.New()
	s.Log.Info("[master] initializing controller")

	s.Log.Info("[master] initializing config in state")
	err := s.config()
	if err != nil {
		return &s, err
	}

	s.Log.Info("[master] initializing MQTT in state")
	err = s.mqtt()
	if err != nil {
		return &s, err
	}

	s.Log.Info("[master] initializing AMQP in state")
	err = s.amqp()
	if err != nil {
		return &s, err
	}

	s.Log.Info("[master] initializing SQL in state")
	err = s.sql()
	if err != nil {
		return &s, err
	}

	s.Log.Info("[master] initializing Data in state")
	s.Data = make(chan []byte)

	s.Log.Info("[master] controller initialized, no errors")
	return &s, nil
}

// config attempts to populate the Config field in State.
func (s *State) config() error {
	s.IsDocker = os.Getenv("HOSTNAME") != ""

	// .env need to be manually loaded outside of Docker
	if !s.IsDocker {
		s.Log.Info("[master] controller not connected to Docker network, loading config files manually")
		configPath, err := filepath.Abs("../config/config.env")
		if err != nil {
			return err
		}
		secretPath, err := filepath.Abs("../config/secret.env")
		if err != nil {
			return err
		}
		err = godotenv.Load(configPath, secretPath)
		if err != nil {
			return err
		}
	} else {
		s.Log.Info("[master] controller connected to Docker network, autoloading config")
	}

	// generate configuration from loaded environment
	s.Config = &Config{}
	return s.Config.load()
}

// mqtt attempts to populate the MQTT and MQTTCh fields in
// State.
func (s *State) mqtt() error {
	s.MQTT = &MQTT{}

	// use vlan inside Docker, localhost when outside
	mqttHost := "tcp://vernemq:1883"
	if !s.IsDocker {
		s.Log.Info("[master] controller not connected to Docker network, using loopback interface for MQTT")
		mqttHost = "tcp://127.0.0.1:1883"
	}
	outChan, err := s.MQTT.Init(s.Log, mqttHost, s.Config.MQTTUser, s.Config.MQTTPass)
	if err != nil {
		return err
	}
	// set output channel + connect to broker
	s.MQTTCh = outChan
	err = s.MQTT.Connect()
	if err != nil {
		return err
	}
	return nil
}

// amqp attempts to populate the AMQP and AMQPCh fields in
// State.
func (s *State) amqp() error {
	s.AMQP[0] = &AMQP{}
	s.AMQP[1] = &AMQP{}

	// use vlan inside Docker, localhost when outside
	amqpHost := "rabbitmq"
	if !s.IsDocker {
		s.Log.Info("[master] controller not connected to Docker network, using loopback interface for AMQP")
		amqpHost = "127.0.0.1"
	}
	amqpConn := fmt.Sprintf("amqp://%s:%s@%s:5672/",
		s.Config.AMQPUser, s.Config.AMQPPass, amqpHost)

	// initialize all AMQP clients, set output channels
	for i, client := range s.AMQP {
		outChan, err := client.Init(s.Log, "amqp"+strconv.Itoa(i), amqpConn)
		if err != nil {
			return err
		}
		s.AMQPCh[i] = outChan
	}

	return nil
}

// sql attempts to populate the SQL field in State.
func (s *State) sql() error {
	// use vlan inside Docker, localhost when outside
	sqlHost := "postgres"
	if !s.IsDocker {
		s.Log.Info("[master] controller not connected to Docker network, using loopback interface for AMQP")
		sqlHost = "127.0.0.1"
	}
	sqlConn := fmt.Sprintf("host=%s user=%s password=%s sslmode=disable",
		sqlHost, s.Config.SQLUser, s.Config.SQLPass)

	// open connection with database
	db, err := sql.Open("postgres", sqlConn)
	if err != nil {
		return err
	}
	s.SQL = db

	// test transaction to test connectivity
	return db.Ping()
}
