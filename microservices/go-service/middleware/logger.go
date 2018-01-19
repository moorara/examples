package middleware

import (
	"net/http"
	"strings"
	"time"

	"github.com/go-kit/kit/log"
	"github.com/gorilla/mux"
	"github.com/moorara/toys/microservices/go-service/util"
)

type loggerMiddleware struct {
	logger log.Logger
}

// NewLoggerMiddleware creates a new middleware for logging
func NewLoggerMiddleware(logger log.Logger) Middleware {
	return &loggerMiddleware{
		logger: logger,
	}
}

func (lm *loggerMiddleware) Wrap(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		method := r.Method
		endpoint := r.URL.Path

		// This only works with mux router
		for p, v := range mux.Vars(r) {
			endpoint = strings.Replace(endpoint, v, ":"+p, 1)
		}

		rw := util.NewResponseWriter(w)
		next(rw, r)

		duration := time.Now().Sub(start).Seconds()
		statusCode := rw.StatusCode()
		statusClass := rw.StatusClass()

		lm.logger.Log(
			"method", method,
			"endpoint", endpoint,
			"statusCode", statusCode,
			"statusClass", statusClass,
			"responseTime", duration,
		)
	}
}
