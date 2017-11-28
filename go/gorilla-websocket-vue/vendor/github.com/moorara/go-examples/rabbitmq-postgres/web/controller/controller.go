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
	// http.Handle("/public/lib/", http.StripPrefix("/public/lib/", http.FileServer(http.Dir("node_modules"))))
}
