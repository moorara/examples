package mq

import (
	"fmt"
	"log"

	"github.com/streadway/amqp"
)

const (
	SensorsQueue            = "Sensors"
	SensorDiscoveryExchange = "SensorDiscovery"
	PersistMessagesQueue    = "PersistMessages"

	WebappDiscoveryQueue  = "WebappDiscovery"
	WebappSourceExchange  = "WebappSources"
	WebappMessageExchange = "WebappMessages"
)

func handleError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
		panic(fmt.Sprintf("%s: %s", msg, err))
	}
}

func GetChannel(url string) (*amqp.Connection, *amqp.Channel) {
	conn, err := amqp.Dial(url)
	handleError(err, "Failed to establish connection to message broker")

	ch, err := conn.Channel()
	handleError(err, "Failed to get channel for connection")

	return conn, ch
}

func GetQueue(ch *amqp.Channel, name string, autoDelete bool) *amqp.Queue {
	q, err := ch.QueueDeclare(
		name,       //name string,
		false,      //durable bool,
		autoDelete, //autoDelete bool,
		false,      //exclusive bool,
		false,      //noWait bool,
		nil)        //args amqp.Table)

	handleError(err, "Failed to declare queue "+name)

	return &q
}
