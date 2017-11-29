package main

import (
	"bytes"
	"encoding/gob"
	"log"

	"github.com/moorara/toys/go/rabbitmq-postgres/config"
	"github.com/moorara/toys/go/rabbitmq-postgres/data"
	"github.com/moorara/toys/go/rabbitmq-postgres/dt"
	"github.com/moorara/toys/go/rabbitmq-postgres/mq"
)

func main() {
	conn, ch := mq.GetChannel(config.RabbitMQURL)
	defer conn.Close()
	defer ch.Close()

	msgs, err := ch.Consume(
		mq.PersistMessagesQueue, //queue string,
		"",    //consumer string,
		false, //autoAck bool,
		false, //exclusive bool,
		false, //noLocal bool,
		false, //noWait bool,
		nil)   //args amqp.Table)

	if err != nil {
		log.Fatalln("Failed to get access to messages")
	}

	for msg := range msgs {
		sm := new(dt.SensorMessage)
		buf := bytes.NewReader(msg.Body)
		dec := gob.NewDecoder(buf)
		dec.Decode(sm)

		err := data.SaveSensorValue(sm)

		if err != nil {
			log.Printf("Failed to save value from sensor %v. Error: %s", sm.Name, err.Error())
		} else {
			msg.Ack(false)
			log.Printf("Sensor value stored in database: %v\n", sm)
		}
	}
}
