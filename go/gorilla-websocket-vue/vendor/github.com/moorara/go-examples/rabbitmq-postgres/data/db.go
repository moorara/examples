package data

import (
	"database/sql"
	"fmt"
	"log"

	_ "github.com/lib/pq"
)

var db *sql.DB

func init() {
	var err error
	db, err = sql.Open("postgres", "postgres://root:toor@localhost/distributed?sslmode=disable")
	if err != nil {
		log.Fatalf("cannot connect to database: %s", err)
		panic(fmt.Sprintf("cannot connect to database: %s", err))
	}
}
