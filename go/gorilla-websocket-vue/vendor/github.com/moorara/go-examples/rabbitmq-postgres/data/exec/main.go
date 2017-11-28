package main

import (
	"bytes"
	"encoding/gob"
	"log"

	"github.com/moorara/go-examples/rabbitmq-postgres/data"
	"github.com/moorara/go-examples/rabbitmq-postgres/dt"
	"github.com/moorara/go-examples/rabbitmq-postgres/mq"
)

const url = "amqp://guest:guest@localhost:5672"

func main() {
	conn, ch := mq.GetChannel(url)
	defer conn.Close()
	defer ch.Close()

	msgs, err := ch.Consume(
		mq.PersistMessagesQueue, //queue string,
		"",    //consumer string,
		false, //autoAck bool,
		true,  //exclusive bool,
		false, //noLocal bool,
		false, //noWait bool,
		nil)   //args amqp.Table)

	if err != nil {
		log.Fatalln("Failed to get access to messages")
	}

	for msg := range msgs {
		sd := new(dt.SensorMessage)
		buf := bytes.NewReader(msg.Body)
		dec := gob.NewDecoder(buf)
		dec.Decode(sd)

		err := data.SaveSensorValue(sd)

		if err != nil {
			log.Printf("Failed to save value from sensor %v. Error: %s", sd.Name, err.Error())
		} else {
			msg.Ack(false)
			log.Printf("Sensor value stored in database: %v\n", sd)
		}
	}
}
