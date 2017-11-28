package coordinator

import (
	"bytes"
	"encoding/gob"
	"fmt"

	"github.com/moorara/go-examples/rabbitmq-postgres/dt"
	"github.com/moorara/go-examples/rabbitmq-postgres/mq"
	"github.com/streadway/amqp"
)

const url = "amqp://guest:guest@localhost:5672"

type QueueListener struct {
	conn    *amqp.Connection
	ch      *amqp.Channel
	ea      *EventAggregator
	sources map[string]<-chan amqp.Delivery
}

func NewQueueListener(ea *EventAggregator) *QueueListener {
	ql := &QueueListener{
		ea:      ea,
		sources: make(map[string]<-chan amqp.Delivery),
	}
	ql.conn, ql.ch = mq.GetChannel(url)

	return ql
}

func (ql *QueueListener) AddListener(msgs <-chan amqp.Delivery) {
	for msg := range msgs {
		sd := new(dt.SensorMessage)
		r := bytes.NewReader(msg.Body)
		d := gob.NewDecoder(r)
		d.Decode(sd)

		fmt.Printf("Received message: %v\n", sd)

		ed := EventData{
			Name:      sd.Name,
			Timestamp: sd.Timestamp,
			Value:     sd.Value,
		}

		ql.ea.PublishEvent("MessageReceived_"+msg.RoutingKey, ed)
	}
}

func (ql *QueueListener) DiscoverSensors() {
	ql.ch.ExchangeDeclare(
		mq.SensorDiscoveryExchange, //name string,
		"fanout",                   //kind string,
		false,                      //durable bool,
		false,                      //autoDelete bool,
		false,                      //internal bool,
		false,                      //noWait bool,
		nil)                        //args amqp.Table)

	ql.ch.Publish(
		mq.SensorDiscoveryExchange, //exchange string,
		"",                //key string,
		false,             //mandatory bool,
		false,             //immediate bool,
		amqp.Publishing{}) //msg amqp.Publishing)
}

func (ql *QueueListener) ListenForNewSource() {
	q := mq.GetQueue(ql.ch, "", true)
	ql.ch.QueueBind(
		q.Name,       //name string,
		"",           //key string,
		"amq.fanout", //exchange string,
		false,        //noWait bool,
		nil)          //args amqp.Table)

	msgs, _ := ql.ch.Consume(
		q.Name, //queue string,
		"",     //consumer string,
		true,   //autoAck bool,
		false,  //exclusive bool,
		false,  //noLocal bool,
		false,  //noWait bool,
		nil)    //args amqp.Table)

	ql.DiscoverSensors()

	fmt.Println("listening for new sources")

	for msg := range msgs {
		src := string(msg.Body)
		if ql.sources[src] == nil {
			fmt.Println("new source discovered " + src)
			ql.ea.PublishEvent("DataSourceDiscovered", src)
			srcChan, _ := ql.ch.Consume(
				src,   //queue string,
				"",    //consumer string,
				true,  //autoAck bool,
				false, //exclusive bool,
				false, //noLocal bool,
				false, //noWait bool,
				nil)   //args amqp.Table)

			ql.sources[string(msg.Body)] = srcChan
			go ql.AddListener(srcChan)
		}
	}
}
