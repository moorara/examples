package main

import (
	"bytes"
	"encoding/gob"
	"flag"
	"log"
	"math/rand"
	"strconv"
	"time"

	"github.com/moorara/go-examples/rabbitmq-postgres/dt"
	"github.com/moorara/go-examples/rabbitmq-postgres/mq"
	"github.com/streadway/amqp"
)

const url = "amqp://guest:guest@localhost:5672"

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

func publishQueueName(ch *amqp.Channel) {
	msg := amqp.Publishing{Body: []byte(*name)}
	ch.Publish(
		"amq.fanout", //exchange string,
		"",           //key string,
		false,        //mandatory bool,
		false,        //immediate bool,
		msg)          //msg amqp.Publishing)
}

func listenForDiscoveryRequests(name string, ch *amqp.Channel) {
	msgs, _ := ch.Consume(
		name,  //queue string,
		"",    //consumer string,
		true,  //autoAck bool,
		false, //exclusive bool,
		false, //noLocal bool,
		false, //noWait bool,
		nil)   //args amqp.Table)

	for range msgs {
		log.Println("received discovery request")
		publishQueueName(ch)
	}
}

func main() {
	flag.Parse()

	nom = (*max-*min)/2 + *min
	value = r.Float64()*(*max-*min) + *min

	conn, ch := mq.GetChannel(url)
	defer conn.Close()
	defer ch.Close()

	dataQueue := mq.GetQueue(ch, *name, false)
	discoveryQueue := mq.GetQueue(ch, "", true)

	publishQueueName(ch)

	ch.QueueBind(
		discoveryQueue.Name, //name string,
		"",                  //key string,
		mq.SensorDiscoveryExchange, //exchange string,
		false, //noWait bool,
		nil)   //args amqp.Table)

	go listenForDiscoveryRequests(discoveryQueue.Name, ch)

	dur, _ := time.ParseDuration(strconv.Itoa(1000/int(*freq)) + "ms")
	signal := time.Tick(dur)

	buf := new(bytes.Buffer)

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

		log.Printf("Reading sent. Value: %v\n", value)
	}
}
