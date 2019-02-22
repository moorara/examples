package main

import (
	"log"
	"net/http"

	"github.com/moorara/toys/go/gorilla-websocket-vue/service"
)

func main() {
	// Create a simple file server
	fs := http.FileServer(http.Dir("public"))
	http.Handle("/", fs)

	// Configure websocket route
	messageService := service.NewMessageService()
	http.HandleFunc("/ws", messageService.HandleConnection)

	go messageService.HandleMessage()

	// Start the server
	log.Println("http server started on :8000")
	err := http.ListenAndServe(":8000", nil)
	if err != nil {
		log.Fatal(err)
	}
}
