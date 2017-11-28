package coordinator

import (
	"bytes"
	"encoding/gob"
	"time"

	"github.com/moorara/go-examples/rabbitmq-postgres/dt"
	"github.com/moorara/go-examples/rabbitmq-postgres/mq"
	"github.com/streadway/amqp"
)

const maxRate = 10 * time.Second

type DatabaseConsumer struct {
	conn    *amqp.Connection
	ch      *amqp.Channel
	queue   *amqp.Queue
	em      EventEmitter
	sources []string
}

func NewDatabaseConsumer(em EventEmitter) *DatabaseConsumer {
	dc := DatabaseConsumer{
		em: em,
	}
	dc.conn, dc.ch = mq.GetChannel(url)
	dc.queue = mq.GetQueue(dc.ch, mq.PersistMessagesQueue, false)

	dc.em.AddListener("DataSourceDiscovered", func(eventData interface{}) {
		dc.SubscribeToDataEvent(eventData.(string))
	})

	return &dc
}

func (dc *DatabaseConsumer) SubscribeToDataEvent(eventName string) {
	for _, v := range dc.sources {
		if v == eventName {
			return
		}
	}

	dc.em.AddListener("MessageReceived_"+eventName, func() func(interface{}) {
		prevTime := time.Unix(0, 0)
		buf := new(bytes.Buffer)

		return func(eventData interface{}) {
			ed := eventData.(EventData)
			if time.Since(prevTime) > maxRate {
				prevTime = time.Now()

				sm := dt.SensorMessage{
					Name:      ed.Name,
					Value:     ed.Value,
					Timestamp: ed.Timestamp,
				}

				buf.Reset()
				enc := gob.NewEncoder(buf)
				enc.Encode(sm)
				msg := amqp.Publishing{
					Body: buf.Bytes(),
				}

				dc.ch.Publish(
					"", //exchange string,
					mq.PersistMessagesQueue, //key string,
					false, //mandatory bool,
					false, //immediate bool,
					msg)   //msg amqp.Publishing)
			}
		}
	}())
}
