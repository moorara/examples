package coordinator

import (
	"bytes"
	"encoding/gob"

	"github.com/moorara/toys/go/rabbitmq-postgres/config"
	"github.com/moorara/toys/go/rabbitmq-postgres/dt"
	"github.com/moorara/toys/go/rabbitmq-postgres/mq"
	"github.com/streadway/amqp"
)

type WebappConsumer struct {
	conn    *amqp.Connection
	ch      *amqp.Channel
	em      EventEmitter
	sources []string
}

func NewWebappConsumer(em EventEmitter) *WebappConsumer {
	wc := WebappConsumer{
		em: em,
	}
	wc.conn, wc.ch = mq.GetChannel(config.RabbitMQURL)

	mq.GetQueue(wc.ch, mq.PersistMessagesQueue, false)

	wc.ch.ExchangeDeclare(
		mq.WebappSourceExchange, //name string,
		"fanout",                //kind string,
		false,                   //durable bool,
		false,                   //autoDelete bool,
		false,                   //internal bool,
		false,                   //noWait bool,
		nil)                     //args amqp.Table)

	wc.ch.ExchangeDeclare(
		mq.WebappMessageExchange, //name string,
		"fanout",                 //kind string,
		false,                    //durable bool,
		false,                    //autoDelete bool,
		false,                    //internal bool,
		false,                    //noWait bool,
		nil)                      //args amqp.Table)

	go wc.ListenForDiscoveryRequests()

	wc.em.AddListener("DataSourceDiscovered", func(eventData interface{}) {
		wc.SubscribeToDataEvent(eventData.(string))
	})

	return &wc
}

func (wc *WebappConsumer) SendMessageSource(src string) {
	wc.ch.Publish(
		mq.WebappSourceExchange, //exchange string,
		"",    //key string,
		false, //mandatory bool,
		false, //immediate bool,
		amqp.Publishing{Body: []byte(src)}) //msg amqp.Publishing)
}

func (wc *WebappConsumer) ListenForDiscoveryRequests() {
	q := mq.GetQueue(wc.ch, mq.WebappDiscoveryQueue, false)
	msgs, _ := wc.ch.Consume(
		q.Name, //queue string,
		"",     //consumer string,
		true,   //autoAck bool,
		false,  //exclusive bool,
		false,  //noLocal bool,
		false,  //noWait bool,
		nil)    //args amqp.Table)

	for range msgs {
		for _, src := range wc.sources {
			wc.SendMessageSource(src)
		}
	}
}

func (wc *WebappConsumer) SubscribeToDataEvent(eventName string) {
	for _, v := range wc.sources {
		if v == eventName {
			return
		}
	}

	wc.sources = append(wc.sources, eventName)
	wc.SendMessageSource(eventName)

	wc.em.AddListener("MessageReceived_"+eventName, func(eventData interface{}) {
		ed := eventData.(EventData)
		sm := dt.SensorMessage{
			Name:      ed.Name,
			Value:     ed.Value,
			Timestamp: ed.Timestamp,
		}

		buf := new(bytes.Buffer)
		enc := gob.NewEncoder(buf)
		enc.Encode(sm)
		msg := amqp.Publishing{
			Body: buf.Bytes(),
		}

		wc.ch.Publish(
			mq.WebappMessageExchange, //exchange string,
			"",    //key string,
			false, //mandatory bool,
			false, //immediate bool,
			msg)   //msg amqp.Publishing)
	})
}
