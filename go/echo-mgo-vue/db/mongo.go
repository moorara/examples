package db

import (
	"gopkg.in/mgo.v2"
)

type MongoDB struct {
	URL     string
	Mode    mgo.Mode
	Refresh bool
	Session *mgo.Session
}

func NewMongoDB(url string, mode mgo.Mode, refresh bool) *MongoDB {
	return &MongoDB{
		URL:     url,
		Mode:    mode,
		Refresh: refresh,
	}
}

func (mongoDB *MongoDB) Connect() (err error) {
	mongoDB.Session, err = mgo.Dial(mongoDB.URL)
	if err != nil {
		return
	}

	mongoDB.Session.SetMode(mongoDB.Mode, mongoDB.Refresh)
	return
}

func (mongoDB *MongoDB) Disconnect() {
	mongoDB.Session.Close()
}
