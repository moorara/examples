package main

import (
	"flag"
	"log"
	"os"
	"runtime"

	nats "github.com/nats-io/go-nats"
)

func main() {
	natsURL := os.Getenv("NATS_URL")
	if natsURL == "" {
		natsURL = nats.DefaultURL
	}

	name := flag.String("name", "producer", "name of producer")
	flag.Parse()

	// Create server connection
	natsConn, err := nats.Connect(natsURL)
	if err != nil {
		log.Fatalf("%s connection error %s\n", *name, err)
	}

	log.Printf("%s connected to %s\n", *name, natsConn.ConnectedUrl())

	// Keep the connection alive
	runtime.Goexit()
}
