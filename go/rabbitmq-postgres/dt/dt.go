package dt

import (
	"encoding/gob"
	"time"
)

// SensorMessage represents a senesor message!
type SensorMessage struct {
	Name      string
	Value     float64
	Timestamp time.Time
}

func init() {
	gob.Register(SensorMessage{})
}
