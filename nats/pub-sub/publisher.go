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

	defaultName := fmt.Sprintf("%s-%d", "publisher", rand.Intn(100))

	name := flag.String("name", defaultName, "name of publisher")
	subject := flag.String("subject", "news", "subject to publish")
	message := flag.String("message", "No news!", "message to send")
	rate := flag.Duration("rate", time.Second, "rate of publishing messages")
	flag.Parse()

	// Create server connection
	natsConn, err := nats.Connect(natsURL)
	if err != nil {
		log.Fatalf("%s connection error %s\n", *name, err)
	}
	defer natsConn.Close()

	log.Printf("%s connected to %s\n", *name, natsConn.ConnectedUrl())

	// Publish messages
	ticker := time.Tick(*rate)
	for now := range ticker {
		msg := fmt.Sprintf("[%s] [%s] %s", *name, now.Format(timeFormat), *message)
		err = natsConn.Publish(*subject, []byte(msg))
		if err != nil {
			log.Printf("%s publish error %s\n", *name, err)
		}

		log.Printf("%s published: %s\n", *name, msg)
	}

	// Keep the connection alive
	runtime.Goexit()
}
