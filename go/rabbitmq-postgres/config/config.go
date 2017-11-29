package config

import (
	"os"
)

const (
	defaultRabbitMQURL = ""
	defaultPostgresURL = ""
)

var (
	RabbitMQURL string
	PostgresURL string
)

func init() {
	RabbitMQURL = os.Getenv("RABBITMQ_URL")
	if RabbitMQURL == "" {
		RabbitMQURL = "amqp://guest:guest@localhost:5672"
	}

	PostgresURL = os.Getenv("POSTGRES_URL")
	if PostgresURL == "" {
		PostgresURL = "postgres://root:toor@localhost/distributed?sslmode=disable"
	}
}
