package middleware

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/moorara/toys/microservices/go-service/util"
	"github.com/stretchr/testify/assert"
)

func TestGetMetricsWrapper(t *testing.T) {
	r := httptest.NewRequest("GET", "http://service/sessions", nil)
	w := httptest.NewRecorder()

	metrics := util.NewMetrics("go_service")
	metricsMiddleware := NewMetricsMiddleware(metrics)
	handler := metricsMiddleware.Wrap(http.NotFound)
	handler(w, r)
	res := w.Result()

	assert.Equal(t, res.StatusCode, http.StatusNotFound)
}
