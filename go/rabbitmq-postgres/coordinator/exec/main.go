package main

import (
	"github.com/moorara/toys/go/rabbitmq-postgres/coordinator"
)

var (
	dc *coordinator.DatabaseConsumer
	wc *coordinator.WebappConsumer
)

func main() {
	ea := coordinator.NewEventAggregator()
	dc = coordinator.NewDatabaseConsumer(ea)
	wc = coordinator.NewWebappConsumer(ea)
	ql := coordinator.NewQueueListener(ea)
	go ql.ListenForNewSource()

	block := make(chan bool, 1)
	<-block
}
