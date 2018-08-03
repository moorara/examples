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

	defaultName := fmt.Sprintf("%s-%d", "worker", rand.Intn(100))

	name := flag.String("name", defaultName, "name of worker")
	subject := flag.String("subject", "jobs", "subject to subscribe")
	queue := flag.String("queue", "", "queue to subscribe")
	flag.Parse()

	// Create server connection
	natsConn, err := nats.Connect(natsURL)
	if err != nil {
		log.Fatalf("%s connection error %s\n", *name, err)
	}

	log.Printf("%s connected to %s\n", *name, natsConn.ConnectedUrl())

	// Subscribe to subject
	subscription, err := natsConn.QueueSubscribe(*subject, *queue, func(msg *nats.Msg) {
		// Handle message
		message := string(msg.Data)
		log.Printf("%s received from %s: %s\n", *name, *queue, message)
	})

	if err != nil {
		log.Fatalf("%s subscription error %s\n", *name, err)
	}

	// Wait for the server to process the request
	err = natsConn.Flush()
	if err := natsConn.LastError(); err != nil {
		log.Fatalf("%s flush error %s\n", *name, err)
	}

	log.Printf("%s subscribed to %s %s\n", *name, subscription.Subject, subscription.Queue)

	// Keep the connection alive
	runtime.Goexit()
}
