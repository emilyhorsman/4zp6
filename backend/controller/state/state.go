package state

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	"github.com/joho/godotenv"
	log "github.com/sirupsen/logrus"
)

// State is the global state for the controller.
type State struct {
	Log      *log.Logger // Log is a structured event logger
	IsDocker bool        // IsDocker indicates if running inside Docker
	Config   *Config     // Config contains global configuration
	MQTT     *MQTT
	MQTTCh   chan MQTTMessage
	AMQP     [2]*AMQP
	AMQPCh   [2]chan AMQPMessage
	SQL      bool
}

// Provision generates a global state for the controller. It
// will return an error if the state fails to initialize.
func Provision() (*State, error) {
	var s State

	s.Log = log.New()
	s.Log.Info("initializing controller")

	s.Log.Info("initializing config in state")
	err := s.config()
	if err != nil {
		return &s, err
	}

	s.Log.Info("initializing MQTT in state")
	err = s.mqtt()
	if err != nil {
		return &s, err
	}

	s.Log.Info("initializing AMQP in state")
	err = s.amqp()
	if err != nil {
		return &s, err
	}

	s.Log.Info("initializing SQL in state")
	err = s.postgres()
	if err != nil {
		return &s, err
	}

	s.Log.Info("controller initialized, no errors")
	return &s, nil
}

// config attempts to populate the Config field in State.
func (s *State) config() error {
	s.IsDocker = os.Getenv("HOSTNAME") != ""

	// .env need to be manually loaded outside of Docker
	if !s.IsDocker {
		s.Log.Info("controller not connected to Docker network, loading config files manually")
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
		s.Log.Info("controller connected to Docker network, autoloading config")
	}

	// generate configuration from loaded environment
	s.Config = &Config{}
	return s.Config.load()
}

// mqtt attempts to populate the MQTT field in State.
func (s *State) mqtt() error {
	s.MQTT = &MQTT{}

	// use vlan inside Docker, localhost when outside
	mqttHost := "tcp://vernemq:1883"
	if !s.IsDocker {
		s.Log.Info("controller not connected to Docker network, using loopback interface for MQTT")
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

func (s *State) amqp() error {
	s.AMQP[0] = &AMQP{}
	s.AMQP[1] = &AMQP{}

	// use vlan inside Docker, localhost when outside
	amqpHost := "rabbitmq"
	if !s.IsDocker {
		s.Log.Info("controller not connected to Docker network, using loopback interface for AMQP")
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

func (s *State) postgres() error {
	return nil
}
