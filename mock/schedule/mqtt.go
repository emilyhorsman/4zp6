package schedule

import (
	"errors"
	"os"
	"strings"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
	"github.com/sirupsen/logrus"
)

func mqtt(l *logrus.Logger) (*state.MQTT, chan state.MQTTMessage, error) {
	m := &state.MQTT{}

	// get MQTT credentials from env
	mqttUser := ""
	mqttPass := ""
	for _, key := range os.Environ() {
		if strings.Contains(key, "DOCKER_VERNEMQ_USER_") {
			parts := strings.Split(key, "_")
			if len(parts) < 4 {
				return m, nil, errors.New("bad env")
			}
			userPass := strings.Split(parts[3], "=")
			if len(parts) < 2 {
				return m, nil, errors.New("bad env")
			}
			mqttUser = userPass[0]
			mqttPass = userPass[1]
		}
	}

	// initialize MQTT client + connect
	out, err := m.Init(l, "tcp://telemetry.0xt.ca:1883", mqttUser, mqttPass)
	if err != nil {
		return m, nil, err
	}
	err = m.Connect()
	if err != nil {
		return m, nil, err
	}
	return m, out, nil
}
