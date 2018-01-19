package middleware

import (
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/go-kit/kit/log"
	"github.com/stretchr/testify/assert"
)

func TestWrapWithLogger(t *testing.T) {
	r := httptest.NewRequest("GET", "http://service/sessions", nil)
	w := httptest.NewRecorder()

	logger := log.NewJSONLogger(os.Stdout)
	loggerMiddleware := NewLoggerMiddleware(logger)
	handler := loggerMiddleware.Wrap(http.NotFound)
	handler(w, r)
	res := w.Result()

	assert.Equal(t, res.StatusCode, http.StatusNotFound)
}
