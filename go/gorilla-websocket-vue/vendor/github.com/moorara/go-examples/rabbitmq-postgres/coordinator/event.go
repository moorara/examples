package coordinator

import (
	"time"
)

type (
	EventEmitter interface {
		AddListener(name string, f func(interface{}))
	}

	EventData struct {
		Name      string
		Value     float64
		Timestamp time.Time
	}

	EventAggregator struct {
		listeners map[string][]func(interface{})
	}
)

func NewEventAggregator() *EventAggregator {
	return &EventAggregator{
		listeners: make(map[string][]func(interface{})),
	}
}

func (e *EventAggregator) AddListener(name string, f func(interface{})) {
	e.listeners[name] = append(e.listeners[name], f)
}

func (e *EventAggregator) PublishEvent(name string, eventData interface{}) {
	if e.listeners[name] != nil {
		for _, f := range e.listeners[name] {
			f(eventData)
		}
	}
}
