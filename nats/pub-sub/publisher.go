package main

import (
	"log"
	"os"

	nats "github.com/nats-io/go-nats"
)

func main() {
	natsURL := os.Getenv("NATS_URL")
	if natsURL == "" {
		natsURL = nats.DefaultURL
	}

	log.Println("Publisher: Hello, World!")
	log.Printf("NATS URL: %s\n", natsURL)
}
