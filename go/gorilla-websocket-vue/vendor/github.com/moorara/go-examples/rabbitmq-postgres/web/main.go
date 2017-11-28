package main

import (
	"log"
	"net/http"

	"github.com/moorara/go-examples/rabbitmq-postgres/web/controller"
)

func main() {
	log.Printf("Listening on port %d\n", 3000)

	controller.Initialize()
	http.ListenAndServe(":3000", nil)
}
