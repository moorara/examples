package model

import (
	"database/sql"
	"fmt"
	"log"

	_ "github.com/lib/pq"
	"github.com/moorara/toys/go/rabbitmq-postgres/config"
)

var db *sql.DB

func init() {
	var err error
	db, err = sql.Open("postgres", config.PostgresURL)
	if err != nil {
		log.Fatalf("cannot connect to database: %s", err)
		panic(fmt.Sprintf("cannot connect to database: %s", err))
	}
}
