// BSD 2-Clause License
//
// Copyright (c) 2020 Emily Horsman, Tanner Ryan. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package main

import (
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

func main() {
	// initialize main state
	s, err := state.Provision()
	if err != nil {
		s.Log.Fatal(err)
	}

	/*
	 * everything below will be soon removed.
	 */

	/*
	 * test MQTT connection
	 */
	topics := []string{"test/#"}
	err = s.MQTT.UpdateTopics(topics)
	if err != nil {
		s.Log.Fatal(err)
	}
	// consume from MQTT
	go func() {
		for msg := range s.MQTTCh {
			s.Log.Printf("mqtt [%s] %s\n", msg.Topic, msg.Payload)
		}
	}()
	// publish to MQTT
	go func() {
		for {
			time.Sleep(2 * time.Second)
			t := time.Now().UTC().Format(time.RFC3339)
			err := s.MQTT.Publish("test/a", []byte(t))
			if err != nil {
				s.Log.Error(err)
			}
		}
	}()

	/*
	 * test AMQP connection
	 */
	routes := map[string][]string{
		// messages published with routing keys "test1" or
		// "test2" will be sent to queue testA
		"testA": []string{"test1", "test2"},
		"testB": []string{"test3", "test4"},
	}
	err = s.AMQP[0].UpdateRouting(routes)
	if err != nil {
		s.Log.Fatal(err)
	}
	// consume from AMQP
	go func() {
		for msg := range s.AMQPCh[0] {
			s.Log.Printf("amqp [%s] %s\n", msg.RoutingKey, msg.Payload)
		}
	}()
	// publish to AMQP
	for {
		time.Sleep(2 * time.Second)
		t := time.Now().UTC().Format(time.RFC3339)
		err := s.AMQP[1].Publish("test1", []byte(t))
		if err != nil {
			s.Log.Error(err)
		}
		err = s.AMQP[1].Publish("test3", []byte(t))
		if err != nil {
			s.Log.Error(err)
		}
	}
}
