package handler

import (
	"context"
	"errors"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/go-kit/kit/log"
	"github.com/gorilla/mux"
	"github.com/moorara/toys/microservices/go-service/service"
	"github.com/stretchr/testify/assert"
)

type mockSessionManager struct {
	CreateCalled  bool
	CreateSession *service.Session
	CreateError   error

	GetCalled  bool
	GetSession *service.Session
	GetError   error
}

func (sm *mockSessionManager) Create(ctx context.Context, name string, value int) (*service.Session, error) {
	sm.CreateCalled = true
	return sm.CreateSession, sm.CreateError
}

func (sm *mockSessionManager) Get(ctx context.Context, id string) (*service.Session, error) {
	sm.GetCalled = true
	return sm.GetSession, sm.GetError
}

func TestNewSessionHandler(t *testing.T) {
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
			rp := service.NewRedisPersister(tc.redisURL)
			logger := log.NewNopLogger()
			sh := NewSessionHandler(rp, logger)

			assert.NotNil(t, sh)
		})
	}
}

func TestPostSession(t *testing.T) {
	tests := []struct {
		name            string
		createSession   *service.Session
		createError     error
		reqBody         string
		expectedStatus  int
		expectedResBody string
	}{
		{
			"InvalidRequest",
			nil, nil,
			`{}`,
			400,
			"",
		},
		{
			"InvalidJSON",
			nil, nil,
			`{"name": "Milad"`,
			400,
			"",
		},
		{
			"SessionManagerError",
			nil, errors.New("error"),
			`{"name": "Milad", "value": 27}`,
			500,
			"",
		},
		{
			"Successful",
			&service.Session{
				ID:    "22bb",
				Name:  "Milad",
				Value: 27,
			},
			nil,
			`{"name": "Milad", "value": 27}`,
			201,
			`{"id":"22bb","name":"Milad","value":27}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			sm := &mockSessionManager{
				CreateSession: tc.createSession,
				CreateError:   tc.createError,
			}

			sh := &redisSessionHandler{
				sm:     sm,
				logger: log.NewNopLogger(),
			}

			reqBody := strings.NewReader(tc.reqBody)
			r := httptest.NewRequest("POST", "http://service/sessions", reqBody)
			w := httptest.NewRecorder()

			sh.PostSession(w, r)
			res := w.Result()
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err)

			assert.Equal(t, tc.expectedStatus, res.StatusCode)
			if tc.expectedStatus == http.StatusCreated {
				assert.Contains(t, string(body), tc.expectedResBody)
			}
		})
	}
}

func TestGetSession(t *testing.T) {
	tests := []struct {
		name            string
		getSession      *service.Session
		getError        error
		sessionID       string
		expectedStatus  int
		expectedResBody string
	}{
		{
			"NoSessionID",
			nil, nil,
			"",
			404,
			`{}`,
		},
		{
			"SessionManagerError",
			nil, errors.New("error"),
			"22bb",
			404,
			`{}`,
		},
		{
			"Successful",
			&service.Session{
				ID:    "22bb",
				Name:  "Milad",
				Value: 27,
			},
			nil,
			"44dd",
			200,
			`{"id":"22bb","name":"Milad","value":27}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			sm := &mockSessionManager{
				GetSession: tc.getSession,
				GetError:   tc.getError,
			}

			sh := &redisSessionHandler{
				sm:     sm,
				logger: log.NewNopLogger(),
			}

			mr := mux.NewRouter()
			mr.HandleFunc("/sessions/{id:[0-9a-f]+}", sh.GetSession)
			ts := httptest.NewServer(mr)
			defer ts.Close()

			res, err := http.Get(ts.URL + "/sessions/" + tc.sessionID)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err)

			assert.Equal(t, tc.expectedStatus, res.StatusCode)
			if tc.expectedStatus == http.StatusOK {
				assert.Contains(t, string(body), tc.expectedResBody)
			}
		})
	}
}
