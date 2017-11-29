package controller

import (
	"net/http"
)

var ws = newWebsocketController()

func Initialize() {
	registerRoutes()
	registerFileServers()
}

func registerRoutes() {
	http.HandleFunc("/ws", ws.handleMessage)
}

func registerFileServers() {
	http.Handle("/", http.FileServer(http.Dir("public")))
}
