package main

import (
	"flag"
	"fmt"
	"log"
	"math/rand"
	"os"
	"runtime"

	nats "github.com/nats-io/go-nats"
)

func main() {
	natsURL := os.Getenv("NATS_URL")
	if natsURL == "" {
		natsURL = nats.DefaultURL
	}

	defaultName := fmt.Sprintf("%s-%d", "subscriber", rand.Intn(100))

	name := flag.String("name", defaultName, "name of subscriber")
	subject := flag.String("subject", "news", "subject to subscribe")
	flag.Parse()

	// Create server connection
	natsConn, err := nats.Connect(natsURL)
	if err != nil {
		log.Fatalf("%s connection error %s\n", *name, err)
	}

	log.Printf("%s connected to %s\n", *name, natsConn.ConnectedUrl())

	// Subscribe to subject
	subscription, err := natsConn.Subscribe(*subject, func(msg *nats.Msg) {
		// Handle message
		message := string(msg.Data)
		log.Printf("%s received: %s\n", *name, message)
	})

	if err != nil {
		log.Fatalf("%s subscription error %s\n", *name, err)
	}

	// Wait for the server to process the request
	err = natsConn.Flush()
	if err := natsConn.LastError(); err != nil {
		log.Fatalf("%s flush error %s\n", *name, err)
	}

	log.Printf("%s subscribed to %s\n", *name, subscription.Subject)

	// Keep the connection alive
	runtime.Goexit()
}
