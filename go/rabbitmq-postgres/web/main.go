package main

import (
	"log"
	"net/http"

	"github.com/moorara/toys/go/rabbitmq-postgres/web/controller"
)

const port = ":3000"

func main() {
	log.Printf("Listening on port %s\n", port)

	controller.Initialize()
	http.ListenAndServe(port, nil)
}
