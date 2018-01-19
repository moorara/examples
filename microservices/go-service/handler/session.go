package handler

import (
	"context"
	"encoding/json"
	"net/http"

	"github.com/go-kit/kit/log"
	"github.com/gorilla/mux"
	"github.com/moorara/toys/microservices/go-service/service"
)

// SessionHandler provides http handlers for Session
type SessionHandler interface {
	PostSession(w http.ResponseWriter, r *http.Request)
	GetSession(w http.ResponseWriter, r *http.Request)
}

// redisSessionHandler implements SessionHandler interface
type redisSessionHandler struct {
	sm     service.SessionManager
	logger log.Logger
}

// NewSessionHandler creates a new session handler
func NewSessionHandler(persister service.Persister, logger log.Logger) SessionHandler {
	return &redisSessionHandler{
		sm:     service.NewSessionManager(persister),
		logger: logger,
	}
}

func (sh *redisSessionHandler) PostSession(w http.ResponseWriter, r *http.Request) {
	req := struct {
		Name  string `json:"name"`
		Value int    `json:"value"`
	}{}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")

	defer r.Body.Close()
	err := json.NewDecoder(r.Body).Decode(&req)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	if req.Name == "" {
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	session, err := sh.sm.Create(context.Background(), req.Name, req.Value)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	w.WriteHeader(http.StatusCreated)
	err = json.NewEncoder(w).Encode(session)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
}

func (sh *redisSessionHandler) GetSession(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["id"]

	w.Header().Set("Content-Type", "application/json; charset=utf-8")

	if sessionID == "" {
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	session, err := sh.sm.Get(context.Background(), sessionID)
	if err != nil {
		w.WriteHeader(http.StatusNotFound)
		return
	}

	w.WriteHeader(http.StatusOK)
	err = json.NewEncoder(w).Encode(session)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
}
