package controller

import (
	"bytes"
	"encoding/gob"
	"net/http"
	"sync"

	"github.com/gorilla/websocket"
	"github.com/moorara/toys/go/rabbitmq-postgres/config"
	"github.com/moorara/toys/go/rabbitmq-postgres/dt"
	"github.com/moorara/toys/go/rabbitmq-postgres/mq"
	"github.com/moorara/toys/go/rabbitmq-postgres/web/model"
	"github.com/streadway/amqp"
)

type (
	message struct {
		Type string      `json:"type"`
		Data interface{} `json:"data"`
	}

	websocketController struct {
		conn     *amqp.Connection
		ch       *amqp.Channel
		upgrader websocket.Upgrader
		sockets  []*websocket.Conn
		mutex    sync.Mutex
	}
)

func newWebsocketController() *websocketController {
	wsc := new(websocketController)
	wsc.conn, wsc.ch = mq.GetChannel(config.RabbitMQURL)
	wsc.upgrader = websocket.Upgrader{
		ReadBufferSize:  1024,
		WriteBufferSize: 1024,
	}

	go wsc.listenForSources()
	go wsc.listenForMessages()

	return wsc
}

func (wsc *websocketController) addSocket(socket *websocket.Conn) {
	wsc.mutex.Lock()
	wsc.sockets = append(wsc.sockets, socket)
	wsc.mutex.Unlock()
}

func (wsc *websocketController) removeSocket(socket *websocket.Conn) {
	wsc.mutex.Lock()

	socket.Close()
	for i := range wsc.sockets {
		if wsc.sockets[i] == socket {
			wsc.sockets = append(wsc.sockets[:i], wsc.sockets[i+1:]...)
		}
	}

	wsc.mutex.Unlock()
}

func (wsc *websocketController) sendMessage(msg message) {
	socketsToRemove := []*websocket.Conn{}

	for _, socket := range wsc.sockets {
		err := socket.WriteJSON(msg)
		if err != nil {
			socketsToRemove = append(socketsToRemove, socket)
		}
	}

	for _, socket := range socketsToRemove {
		wsc.removeSocket(socket)
	}
}

func (wsc *websocketController) listenForSources() {
	q := mq.GetQueue(wsc.ch, "", true)
	wsc.ch.QueueBind(
		q.Name, //name string,
		"",     //key string,
		mq.WebappSourceExchange, //exchange string,
		false, //noWait bool,
		nil)   //args amqp.Table)

	msgs, _ := wsc.ch.Consume(
		q.Name, //queue string,
		"",     //consumer string,
		true,   //autoAck bool,
		false,  //exclusive bool,
		false,  //noLocal bool,
		false,  //noWait bool,
		nil)    //args amqp.Table)

	for msg := range msgs {
		sensor, err := model.GetSensorByName(string(msg.Body))
		if err != nil {
			println(err.Error())
		}

		wsc.sendMessage(message{Type: "source", Data: sensor})
	}
}

func (wsc *websocketController) listenForMessages() {
	q := mq.GetQueue(wsc.ch, "", true)
	wsc.ch.QueueBind(
		q.Name, //name string,
		"",     //key string,
		mq.WebappMessageExchange, //exchange string,
		false, //noWait bool,
		nil)   //args amqp.Table)

	msgs, _ := wsc.ch.Consume(
		q.Name, //queue string,
		"",     //consumer string,
		true,   //autoAck bool,
		false,  //exclusive bool,
		false,  //noLocal bool,
		false,  //noWait bool,
		nil)    //args amqp.Table)

	for msg := range msgs {
		sm := dt.SensorMessage{}
		buf := bytes.NewBuffer(msg.Body)
		dec := gob.NewDecoder(buf)
		err := dec.Decode(&sm)
		if err != nil {
			println(err.Error())
		}

		wsc.sendMessage(message{Type: "reading", Data: sm})
	}
}

func (wsc *websocketController) handleMessage(w http.ResponseWriter, r *http.Request) {
	socket, _ := wsc.upgrader.Upgrade(w, r, nil)
	wsc.addSocket(socket)
	go wsc.listenForDiscoveryRequests(socket)
}

func (wsc *websocketController) listenForDiscoveryRequests(socket *websocket.Conn) {
	for {
		msg := message{}
		err := socket.ReadJSON(&msg)

		if err != nil {
			wsc.removeSocket(socket)
			break
		}

		if msg.Type == "discover" {
			wsc.ch.Publish(
				"", //exchange string,
				mq.WebappDiscoveryQueue, //key string,
				false,             //mandatory bool,
				false,             //immediate bool,
				amqp.Publishing{}) //msg amqp.Publishing)
		}
	}
}
