package model

import (
	"time"

	"gopkg.in/mgo.v2/bson"
)

type Task struct {
	ID          bson.ObjectId `json:"id"          bson:"_id"`
	Name        string        `json:"name"        bson:"name"`
	LastUpdated time.Time     `json:"lastUpdated" bson:"lastUpdated"`
}

func NewTask(name string) *Task {
	return &Task{
		ID:          bson.NewObjectId(),
		Name:        name,
		LastUpdated: time.Now(),
	}
}
