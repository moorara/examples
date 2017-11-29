package model

type Sensor struct {
	Name         string  `json:"name"`
	SerialNo     string  `json:"serialNo"`
	UnitType     string  `json:"unitType"`
	MinSafeValue float64 `json:"minSafeValue"`
	MaxSafeValue float64 `json:"maxSafeValue"`
}

func GetSensorByName(name string) (Sensor, error) {
	s := Sensor{}
	q := `SELECT name, serial_no, unit_type, min_safe_value, max_safe_value FROM sensors WHERE name = $1`

	row := db.QueryRow(q, name)
	err := row.Scan(&s.Name, &s.SerialNo, &s.UnitType, &s.MinSafeValue, &s.MaxSafeValue)
	return s, err
}
