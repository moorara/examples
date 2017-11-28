package data

import (
	"fmt"

	"github.com/moorara/go-examples/rabbitmq-postgres/dt"
)

var sensors map[string]int

func getSensors() {
	var id int
	var name string
	sensors = make(map[string]int)

	q := `SELECT id, name FROM sensors`
	rows, _ := db.Query(q)

	for rows.Next() {
		rows.Scan(&id, &name)
		sensors[name] = id
	}
}

func SaveSensorValue(sm *dt.SensorMessage) error {
	if sensors[sm.Name] == 0 {
		getSensors()
	}

	if sensors[sm.Name] == 0 {
		return fmt.Errorf("Unable to find sensor %s", sm.Name)
	}

	q := `INSERT INTO sensor_values (value, sensor_id, taken_on) VALUES ($1, $2, $3)`
	_, err := db.Exec(q, sm.Value, sensors[sm.Name], sm.Timestamp)

	return err
}
