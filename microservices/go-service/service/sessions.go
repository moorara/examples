package service

import (
	"context"
	"encoding/json"
	"time"

	"github.com/google/uuid"
	"github.com/pkg/errors"
)

const (
	sessionTTLInMin = 60
)

type (
	// Session represents a session
	Session struct {
		ID    string `json:"id"`
		Name  string `json:"name"`
		Value int    `json:"value"`
	}

	// SessionManager abstracts CRUD operations for Session
	SessionManager interface {
		Create(ctx context.Context, name string, value int) (*Session, error)
		Get(ctx context.Context, id string) (*Session, error)
	}

	redisSessionManager struct {
		persister  Persister
		sessionTTL time.Duration
	}
)

// NewSessionManager creates a new session manager
func NewSessionManager(persister Persister) SessionManager {
	return &redisSessionManager{
		persister:  persister,
		sessionTTL: sessionTTLInMin * time.Minute,
	}
}

func (sm *redisSessionManager) Create(ctx context.Context, name string, value int) (*Session, error) {
	session := &Session{
		ID:    uuid.New().String(),
		Name:  name,
		Value: value,
	}

	data, err := json.Marshal(session)
	if err != nil {
		return nil, err
	}

	chErr := make(chan error, 1)
	go func() {
		chErr <- sm.persister.Save(session.ID, data, sm.sessionTTL)
	}()

	select {
	case <-ctx.Done():
		err = ctx.Err()
	case err = <-chErr:
	}

	if err != nil {
		return nil, errors.Wrap(err, "Session creation failed")
	}
	return session, nil
}

func (sm *redisSessionManager) Get(ctx context.Context, id string) (*Session, error) {
	var err error
	session := new(Session)

	chErr := make(chan error, 1)
	go func() {
		data, err := sm.persister.Load(id)
		if err != nil {
			chErr <- err
			return
		}
		chErr <- json.Unmarshal(data, session)
	}()

	select {
	case <-ctx.Done():
		err = ctx.Err()
	case err = <-chErr:
	}

	if err != nil {
		return nil, errors.Wrap(err, "Session retrieval failed")
	}
	return session, nil
}
