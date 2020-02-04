package state

import (
	"errors"
	"os"
	"strings"
)

// Config is the environment variable configuration for the
// controller.
type Config struct {
	AMQPUser string // AMQPUser is AMQP username
	AMQPPass string // AMQPPass is AMQP password
	MQTTUser string // MQTTUser is MQTT username
	MQTTPass string // MQTTPass is MQTT password
	SQLUser  string // SQLUser is SQL username
	SQLPass  string // SQLPass is SQL password
}

// load will attempt to load the required environment
// variables into the Config struct. An error will be
// returned if a required variable is not defined.
func (c *Config) load() error {
	// AMQP parameters
	c.AMQPUser = os.Getenv("RABBITMQ_DEFAULT_USER")
	if c.AMQPUser == "" {
		return errors.New("env RABBITMQ_DEFAULT_USER not defined")
	}
	c.AMQPPass = os.Getenv("RABBITMQ_DEFAULT_PASS")
	if c.AMQPPass == "" {
		return errors.New("env RABBITMQ_DEFAULT_PASS not defined")
	}

	// MQTT parameters
	for _, key := range os.Environ() {
		if strings.Contains(key, "DOCKER_VERNEMQ_USER_") {
			parts := strings.Split(key, "_")
			if len(parts) < 4 {
				return errors.New("env DOCKER_VERNEMQ_USER_ not in correct format")
			}
			userPass := strings.Split(parts[3], "=")
			if len(parts) < 2 {
				return errors.New("env DOCKER_VERNEMQ_USER_ not in correct format")
			}
			c.MQTTUser = userPass[0]
			c.MQTTPass = userPass[1]
		}
	}
	if c.MQTTUser == "" || c.MQTTPass == "" {
		return errors.New("env DOCKER_VERNEMQ_USER_ not defined")
	}

	// SQL parameters
	c.SQLUser = os.Getenv("POSTGRES_USER")
	if c.SQLUser == "" {
		return errors.New("env POSTGRES_USER not defined")
	}
	c.SQLPass = os.Getenv("POSTGRES_PASSWORD")
	if c.SQLPass == "" {
		return errors.New("env POSTGRES_PASSWORD not defined")
	}

	return nil
}
