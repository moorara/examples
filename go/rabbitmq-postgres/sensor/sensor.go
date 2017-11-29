package sensor

import (
	"log"

	"github.com/streadway/amqp"
)

func PublishQueueName(ch *amqp.Channel, name string) {
	msg := amqp.Publishing{Body: []byte(name)}
	ch.Publish(
		"amq.fanout", //exchange string,
		"",           //key string,
		false,        //mandatory bool,
		false,        //immediate bool,
		msg)          //msg amqp.Publishing)
}

func ListenForDiscoveryRequests(ch *amqp.Channel, queue, name string) {
	msgs, _ := ch.Consume(
		queue, //queue string,
		"",    //consumer string,
		true,  //autoAck bool,
		false, //exclusive bool,
		false, //noLocal bool,
		false, //noWait bool,
		nil)   //args amqp.Table)

	for range msgs {
		log.Println("received discovery request")
		PublishQueueName(ch, name)
	}
}
