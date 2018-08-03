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
	reqTimeout = time.Second
)

func main() {
	natsURL := os.Getenv("NATS_URL")
	if natsURL == "" {
		natsURL = nats.DefaultURL
	}

	defaultName := fmt.Sprintf("%s-%d", "requestor", rand.Intn(100))

	name := flag.String("name", defaultName, "name of requestor")
	subject := flag.String("subject", "reqs", "subject to subscribe")
	request := flag.String("request", "No request!", "request to send")
	rate := flag.Duration("rate", time.Second, "rate of sending requests")
	flag.Parse()

	// Create server connection
	natsConn, err := nats.Connect(natsURL)
	if err != nil {
		log.Fatalf("%s connection error %s\n", *name, err)
	}
	defer natsConn.Close()

	log.Printf("%s connected to %s\n", *name, natsConn.ConnectedUrl())

	// Send requests
	ticker := time.Tick(*rate)
	for now := range ticker {
		requestMessage := fmt.Sprintf("[%s] [%s] %s", *name, now.Format(timeFormat), *request)
		replyMessage, err := natsConn.Request(*subject, []byte(requestMessage), reqTimeout)
		if err != nil {
			if natsConn.LastError() != nil {
				log.Printf("%s request error %s\n", *name, natsConn.LastError())
			}
			log.Printf("%s request error %s\n", *name, err)
		}

		log.Printf("%s requested: %s\n", *name, requestMessage)
		log.Printf("%s received: %s\n", *name, string(replyMessage.Data))
	}

	// Keep the connection alive
	runtime.Goexit()
}
