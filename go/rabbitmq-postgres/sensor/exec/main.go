package main

import (
	"bytes"
	"encoding/gob"
	"flag"
	"log"
	"math/rand"
	"strconv"
	"time"

	"github.com/moorara/toys/go/rabbitmq-postgres/config"
	"github.com/moorara/toys/go/rabbitmq-postgres/dt"
	"github.com/moorara/toys/go/rabbitmq-postgres/mq"
	"github.com/moorara/toys/go/rabbitmq-postgres/sensor"
	"github.com/streadway/amqp"
)

var (
	name     = flag.String("name", "sensor", "name of the sensor")
	freq     = flag.Uint("freq", 10, "update frequency in cycles/sec")
	min      = flag.Float64("min", 0., "minimum value for generated readings")
	max      = flag.Float64("max", 1., "maximum value for generated readings")
	stepSize = flag.Float64("step", 0.2, "maximum allowable change per measurement")
)

var nom, value float64
var r = rand.New(rand.NewSource(time.Now().UnixNano()))

func calcValue() {
	var maxStep, minStep float64

	if value < nom {
		maxStep = *stepSize
		minStep = -1 * *stepSize * (value - *min) / (nom - *min)
	} else {
		maxStep = *stepSize * (*max - value) / (*max - nom)
		minStep = -1 * *stepSize
	}

	value += r.Float64()*(maxStep-minStep) + minStep
}

func main() {
	flag.Parse()

	nom = *min + (*max-*min)/2
	value = *min + r.Float64()*(*max-*min)

	conn, ch := mq.GetChannel(config.RabbitMQURL)
	defer conn.Close()
	defer ch.Close()

	dataQueue := mq.GetQueue(ch, *name, false)
	discoveryQueue := mq.GetQueue(ch, "", true)

	sensor.PublishQueueName(ch, *name)

	ch.QueueBind(
		discoveryQueue.Name, //name string,
		"",                  //key string,
		mq.SensorDiscoveryExchange, //exchange string,
		false, //noWait bool,
		nil)   //args amqp.Table)

	go sensor.ListenForDiscoveryRequests(ch, discoveryQueue.Name, *name)

	buf := new(bytes.Buffer)
	dur, _ := time.ParseDuration(strconv.Itoa(1000/int(*freq)) + "ms")
	signal := time.Tick(dur)

	for range signal {
		calcValue()
		reading := dt.SensorMessage{
			Name:      *name,
			Value:     value,
			Timestamp: time.Now(),
		}

		buf.Reset()
		enc := gob.NewEncoder(buf)
		enc.Encode(reading)

		msg := amqp.Publishing{
			Body: buf.Bytes(),
		}

		ch.Publish(
			"",             //exchange string,
			dataQueue.Name, //key string,
			false,          //mandatory bool,
			false,          //immediate bool,
			msg)            //msg amqp.Publishing)

		log.Printf("[%s] Sensor value sent: %v\n", *name, value)
	}
}
