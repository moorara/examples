package data

import (
	"fmt"

	"github.com/moorara/toys/go/rabbitmq-postgres/dt"
)

var sensors map[string]int

func getSensors() error {
	var id int
	var name string
	sensors = make(map[string]int)

	q := `SELECT id, name FROM sensors`
	rows, err := db.Query(q)
	if err != nil {
		return fmt.Errorf("Cannot query the database. Error: %v", err)
	}

	for rows.Next() {
		rows.Scan(&id, &name)
		sensors[name] = id
	}

	return nil
}

func SaveSensorValue(sm *dt.SensorMessage) error {
	if sensors[sm.Name] == 0 {
		if err := getSensors(); err != nil {
			return err
		}
	}

	if sensors[sm.Name] == 0 {
		return fmt.Errorf("Unable to find sensor %s", sm.Name)
	}

	q := `INSERT INTO sensor_values (value, sensor_id, taken_on) VALUES ($1, $2, $3)`
	_, err := db.Exec(q, sm.Value, sensors[sm.Name], sm.Timestamp)

	return err
}
