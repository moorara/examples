package server

import (
	"context"
	"net/http"
	"os"

	"github.com/go-kit/kit/log"
	"github.com/gorilla/mux"
	"github.com/moorara/toys/microservices/go-service/config"
	"github.com/moorara/toys/microservices/go-service/handler"
	"github.com/moorara/toys/microservices/go-service/middleware"
	"github.com/moorara/toys/microservices/go-service/service"
	"github.com/moorara/toys/microservices/go-service/util"
)

type (
	// Server represents a generic server
	Server interface {
		ListenAndServe() error
		Shutdown(context.Context) error
	}

	// HTTPServer representa a http server
	HTTPServer struct {
		config config.Config
		logger log.Logger
		server Server
	}
)

// New creates a new http server
func New(config config.Config) *HTTPServer {
	logger := log.NewJSONLogger(os.Stdout)
	metrics := util.NewMetrics("go_service")

	loggerMiddleware := middleware.NewLoggerMiddleware(logger)
	metricsMiddleware := middleware.NewMetricsMiddleware(metrics)

	redisPersister := service.NewRedisPersister(config.RedisURL)
	sessionHandler := handler.NewSessionHandler(redisPersister, logger)
	postSessionHandler := middleware.WrapAll(sessionHandler.PostSession, metricsMiddleware, loggerMiddleware)
	getSessionHandler := middleware.WrapAll(sessionHandler.GetSession, metricsMiddleware, loggerMiddleware)

	router := mux.NewRouter()
	router.NotFoundHandler = handler.GetNotFoundHandler(logger)
	router.HandleFunc("/health", handler.HealthHandler).Methods("GET")
	router.HandleFunc("/metrics", metrics.GetHandler().ServeHTTP)
	router.HandleFunc("/sessions", postSessionHandler).Methods("POST")
	router.HandleFunc("/sessions/{id}", getSessionHandler).Methods("GET")

	return &HTTPServer{
		config: config,
		logger: logger,
		server: &http.Server{
			Addr:    config.ServicePort,
			Handler: router,
		},
	}
}

// Start starts the server
func (s *HTTPServer) Start() error {
	s.logger.Log("message", "Listening on port "+s.config.ServicePort+" ...")
	return s.server.ListenAndServe()
}
