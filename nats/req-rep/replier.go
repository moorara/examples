package main

import (
	"flag"
	"fmt"
	"log"
	"math/rand"
	"os"
	"runtime"
	"time"

	nats "github.com/nats-io/go-nats"
)

const (
	timeFormat = "15:04:05"
)

func main() {
	natsURL := os.Getenv("NATS_URL")
	if natsURL == "" {
		natsURL = nats.DefaultURL
	}

	defaultName := fmt.Sprintf("%s-%d", "replier", rand.Intn(100))

	name := flag.String("name", defaultName, "name of replier")
	subject := flag.String("subject", "reqs", "subject to subscribe")
	reply := flag.String("reply", "Good request!", "reply message to send")
	flag.Parse()

	// Create server connection
	natsConn, err := nats.Connect(natsURL)
	if err != nil {
		log.Fatalf("%s connection error %s\n", *name, err)
	}

	log.Printf("%s connected to %s\n", *name, natsConn.ConnectedUrl())

	// Subscribe to subject
	subscription, err := natsConn.Subscribe(*subject, func(msg *nats.Msg) {
		// Handle request
		requestMessage := string(msg.Data)
		log.Printf("%s received: %s\n", *name, requestMessage)

		// Reply
		replyMessage := fmt.Sprintf("[%s] [%s] %s", *name, time.Now().Format(timeFormat), *reply)
		err := natsConn.Publish(msg.Reply, []byte(replyMessage))
		if err != nil {
			log.Printf("%s reply error %s\n", *name, err)
		}

		log.Printf("%s replied %s\n", *name, replyMessage)
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
