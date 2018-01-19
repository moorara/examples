package service

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

const (
	delay   = 200 * time.Millisecond
	timeout = 100 * time.Millisecond
)

type mockPersister struct {
	SaveCalled bool
	SaveError  error

	LoadCalled bool
	LoadData   []byte
	LoadError  error
}

func (mp *mockPersister) Save(key string, data []byte, ttl time.Duration) error {
	time.Sleep(delay)
	mp.SaveCalled = true
	return mp.SaveError
}

func (mp *mockPersister) Load(key string) ([]byte, error) {
	time.Sleep(delay)
	mp.LoadCalled = true
	return mp.LoadData, mp.LoadError
}

func TestNewSessionManager(t *testing.T) {
	tests := []struct {
		name     string
		redisURL string
	}{
		{
			"WithoutUserPass",
			"redis://redis:6379",
		},
		{
			"WithUserPass",
			"redis://user:pass@redis:6389",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			rp := NewRedisPersister(tc.redisURL)
			sm := NewSessionManager(rp)
			assert.NotNil(t, sm)
		})
	}
}

func TestSessionManagerCreate(t *testing.T) {
	tests := []struct {
		name         string
		saveError    error
		context      func() context.Context
		sessionName  string
		sessionValue int
		expectError  bool
	}{
		{
			"PersisterError",
			errors.New("error"),
			func() context.Context {
				return context.Background()
			},
			"",
			0,
			true,
		},
		{
			"ContextTimeout",
			nil,
			func() context.Context {
				ctx, _ := context.WithTimeout(context.Background(), timeout)
				return ctx
			},
			"",
			0,
			true,
		},
		{
			"Successful",
			nil,
			func() context.Context {
				return context.Background()
			},
			"Milad",
			27,
			false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			sm := &redisSessionManager{
				persister: &mockPersister{
					SaveError: tc.saveError,
				},
				sessionTTL: 1 * time.Minute,
			}

			session, err := sm.Create(tc.context(), tc.sessionName, tc.sessionValue)

			if tc.expectError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tc.sessionName, session.Name)
				assert.Equal(t, tc.sessionValue, session.Value)
			}
		})
	}
}

func TestSessionManagerGet(t *testing.T) {
	tests := []struct {
		name                 string
		loadData             []byte
		loadError            error
		context              func() context.Context
		sessionID            string
		expectError          bool
		expectedSessionID    string
		expectedSessionName  string
		expectedSessionValue int
	}{
		{
			"PersisterError",
			nil, errors.New("error"),
			func() context.Context {
				return context.Background()
			},
			"",
			true,
			"", "", 0,
		},
		{
			"ContextTimeout",
			nil, nil,
			func() context.Context {
				ctx, _ := context.WithTimeout(context.Background(), timeout)
				return ctx
			},
			"",
			true,
			"", "", 0,
		},
		{
			"Successful",
			[]byte(`{"id": "abcd", "name": "Milad", "value": 27}`), nil,
			func() context.Context {
				return context.Background()
			},
			"Milad",
			false,
			"abcd", "Milad", 27,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			sm := &redisSessionManager{
				persister: &mockPersister{
					LoadData:  tc.loadData,
					LoadError: tc.loadError,
				},
				sessionTTL: 1 * time.Minute,
			}

			session, err := sm.Get(tc.context(), tc.sessionID)

			if tc.expectError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tc.expectedSessionID, session.ID)
				assert.Equal(t, tc.expectedSessionName, session.Name)
				assert.Equal(t, tc.expectedSessionValue, session.Value)
			}
		})
	}
}
