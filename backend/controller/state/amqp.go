package state

import (
	"strings"
	"time"

	log "github.com/sirupsen/logrus"
	"github.com/streadway/amqp"
)

// AMQP is the AMQP client.
type AMQP struct {
	routing     map[string][]string // routing is a map binding queues to a list of routing keys
	isConnected bool                // isConnected indicate if AMQP connection is currently established
	channel     *amqp.Channel       // channel is the internal AMQP channel
	id          string              // id is the client identifier
	host        string              // host is the formatted AMQP host string
	exchange    string              // exchange is common AMQP exchange
	ch          chan AMQPMessage    // ch is the message output channel, exposed by Init()
	log         *log.Logger         // log is state structured event logger
}

// AMQPMessage is a message sent over AMQP.
type AMQPMessage struct {
	RoutingKey string
	Payload    []byte
}

// Init will attempt to initialize the AMQP client. It
// requires the AMQP broker's ID string and formatted AMQP
// host string. It will return the output channel or an
// error.
func (a *AMQP) Init(log *log.Logger, id, host string) (chan AMQPMessage, error) {
	// can not directly access state.Log
	a.log = log

	a.id = id
	a.host = host
	a.exchange = "engine"

	// initialize output channel (buffer of 128 messages)
	a.ch = make(chan AMQPMessage, 128)

	// call internal function to perform actual
	// initialization
	return a.ch, a.init()
}

// init is the internal initalization function. It is called
// by Init() and recover(). It establishes the connection to
// the AMQP broker without redeclaring the output channel.
// It will return an error if the connection with the AMQP
// broker fails to establish.
func (a *AMQP) init() error {
	// establish connection + channel with broker
	a.log.Infof("[%s] connecting to broker", a.id)
	conn, err := amqp.Dial(a.host)
	if err != nil {
		return err
	}
	ch, err := conn.Channel()
	if err != nil {
		return err
	}
	a.channel = ch

	// spawn goroutine to recover failed connections
	go func(a *AMQP, conn *amqp.Connection) {
		// channel triggers on closure
		<-conn.NotifyClose(make(chan *amqp.Error))
		a.isConnected = false
		a.log.Errorf("[%s] connection to broker lost, retrying ...", a.id)
		a.recover(conn)
	}(a, conn)

	a.isConnected = true
	a.log.Infof("[%s] connected to broker, notifying routes ...", a.id)
	return a.notifyRouting()
}

// GetRouting returns the list queues and the list of
// routing queues binded to each queue.
func (a *AMQP) GetRouting() map[string][]string {
	return a.routing
}

// UpdateRouting replace the AMQP routing with the one
// provided. It will notify the AMQP broker of the changes.
// It may return an error if the transaction fails to
// complete.
func (a *AMQP) UpdateRouting(routing map[string][]string) error {
	// unbind previous
	if a.isConnected {
		for queue, routingKeys := range a.routing {
			for _, routingKey := range routingKeys {
				a.log.Infof("[%s] unrouting routing key '%s' from queue '%s' on exchange '%s'",
					a.id, routingKey, queue, a.exchange)
				err := a.channel.QueueUnbind(queue, routingKey, a.exchange, nil)
				if err != nil {
					return err
				}
			}
		}
	}
	a.routing = routing
	// only notify if connected
	if a.isConnected {
		return a.notifyRouting()
	}
	return nil
}

// notifyRouting will notify the AMQP broker of the routing
// key queue binding. It will return an error if the routing
// keys fail to bind.
func (a *AMQP) notifyRouting() error {
	// declare exchange
	err := a.channel.ExchangeDeclare(
		a.exchange, // name
		"topic",    // type
		true,       // durable
		false,      // auto deleted
		false,      // internal
		false,      // no-wait
		nil,        // arguments
	)
	if err != nil {
		return err
	}

	// iterate over each queue
	for queue, routingKeys := range a.routing {
		// declare queue
		q, err := a.channel.QueueDeclare(
			queue, // name
			false, // durable
			false, // delete when unused
			false, // exclusive
			false, // no wait
			nil,   // arguments
		)
		if err != nil {
			return err
		}
		// iterate over all corresponding routing keys, bind
		// each key to queue above
		for _, key := range routingKeys {
			err = a.channel.QueueBind(
				q.Name,     // name
				key,        // routing key
				a.exchange, // exchange
				false,      // no wait
				nil,        // arguments
			)
			if err != nil {
				return err
			}
		}
		// generate output channel generate channel to
		// consume from
		ch, err := a.channel.Consume(
			q.Name, // name
			"",     // consumer tag
			true,   // auto ack message
			false,  // exclusive
			false,  // no local
			false,  // no wait
			nil,    // arguments
		)
		if err != nil {
			return err
		}

		// consume RabbitMQ and send deliveries to output
		// channel (fan in)
		go func(input <-chan amqp.Delivery, out chan AMQPMessage) {
			for d := range input {
				out <- AMQPMessage{
					RoutingKey: d.RoutingKey,
					Payload:    d.Body,
				}
			}
		}(ch, a.ch)

		routes := strings.Join(routingKeys, ",")
		a.log.Infof("[%s] routing (%s) to queue '%s' on exchange '%s'",
			a.id, routes, queue, a.exchange)
	}

	// no error
	return nil
}

// Close will close the channel to the AMQP broker. It will
// return an error if the channel fails to gracefully close.
func (a *AMQP) Close() error {
	return a.channel.Close()
}

// Publish accept a routing key and a payload. It will
// attempt to publish the payload with the provided routing
// key. It will return an error if the payload fails to
// publish.
func (a *AMQP) Publish(routingKey string, payload []byte) error {
	return a.channel.Publish(
		a.exchange, // exchange
		routingKey, // routing key
		false,      // mandatory
		false,      // immediate
		amqp.Publishing{
			Body: payload,
		},
	)
}

// recover is called when the AMQP loses its connection. It
// will recursively call init() until the connection is
// re-established.
func (a *AMQP) recover(conn *amqp.Connection) {
	// prevent memory leaks
	if conn != nil {
		conn.Close()
	}
	// recursive call until connection established
	err := a.init()
	if err != nil {
		a.log.Warnf("[%s] failed to reprovision connection, retrying ...", a.id)
		time.Sleep(10 * time.Second)
		a.recover(conn)
	}
}
