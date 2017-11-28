package service

import (
	"log"
	"net/http"

	"github.com/gorilla/websocket"
	"github.com/moorara/toys/go/gorilla-websocket-vue/model"
)

type MessageService struct {
	upgrader  *websocket.Upgrader
	clients   map[*websocket.Conn]bool
	broadcast chan model.Message
}

func NewMessageService() *MessageService {
	return &MessageService{
		upgrader: &websocket.Upgrader{
			CheckOrigin: func(r *http.Request) bool {
				return true
			},
		},
		clients:   make(map[*websocket.Conn]bool),
		broadcast: make(chan model.Message),
	}
}

func (service *MessageService) HandleConnection(w http.ResponseWriter, r *http.Request) {
	// Upgrade initial GET request to a websocket
	ws, err := service.upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Fatal(err)
	}
	defer ws.Close()

	// Register the new client
	service.clients[ws] = true

	for {
		var message model.Message
		err := ws.ReadJSON(&message)
		if err != nil {
			log.Printf("error: %v", err)
			delete(service.clients, ws)
			break
		}
		service.broadcast <- message
	}
}

func (service *MessageService) HandleMessage() {
	for {
		message := <-service.broadcast
		for client := range service.clients {
			err := client.WriteJSON(message)
			if err != nil {
				log.Printf("error: %v", err)
				client.Close()
				delete(service.clients, client)
			}
		}
	}
}
