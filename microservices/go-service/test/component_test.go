package test

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/gorilla/mux"
	"github.com/stretchr/testify/assert"
)

func isComponentTest() bool {
	value := os.Getenv("COMPONENT_TEST")
	return value == "true" || value == "TRUE"
}

func getServiceURL() string {
	serviceURL := os.Getenv("SERVICE_URL")
	if serviceURL == "" {
		serviceURL = "http://localhost:4010"
	}
	return serviceURL
}

func TestUnit(t *testing.T) {
	if isComponentTest() {
		t.SkipNow()
	}

	tests := []struct {
		name             string
		method, endpoint string
		reqBody          JSON
		resBody          JSON
	}{
		{
			"EmptyBody",
			"GET", "/",
			nil,
			nil,
		},
		{
			"SimpleGET",
			"GET", "/",
			JSON{},
			JSON{},
		},
		{
			"SimplePOST",
			"POST", "/sessions",
			JSON{"name": "me", "value": 2.0},
			JSON{"id": "0", "name": "me", "value": 2.0},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			rt := mux.NewRouter()
			rt.HandleFunc(tc.endpoint, func(w http.ResponseWriter, r *http.Request) {
				w.Header().Set("Content-Type", "application/json")
				json.NewEncoder(w).Encode(tc.resBody)
			}).Methods(tc.method)
			ts := httptest.NewServer(rt)
			defer ts.Close()

			os.Setenv("SERVICE_URL", ts.URL)
			cmp := NewComponent()

			statusCode, resBody, err := cmp.Call(context.Background(), tc.method, tc.endpoint, tc.reqBody)
			assert.NoError(t, err)
			assert.Equal(t, 200, statusCode)
			assert.Equal(t, tc.resBody, resBody)
			os.Unsetenv("SERVICE_URL")
		})
	}
}

func TestComponentGetHealth(t *testing.T) {
	if !isComponentTest() {
		t.SkipNow()
	}

	endpoint := getServiceURL() + "/health"
	res, err := http.Get(endpoint)
	assert.NoError(t, err)
	assert.Equal(t, 200, res.StatusCode)
}

func TestComponentGetMetrics(t *testing.T) {
	if !isComponentTest() {
		t.SkipNow()
	}

	endpoint := getServiceURL() + "/metrics"
	res, err := http.Get(endpoint)
	assert.NoError(t, err)
	assert.Equal(t, 200, res.StatusCode)

	data, err := ioutil.ReadAll(res.Body)
	assert.NoError(t, err)
	body := string(data)

	assert.Contains(t, body, "# TYPE go_goroutines gauge")
	assert.Contains(t, body, "# TYPE go_memstats_mallocs_total counter")
	assert.Contains(t, body, "# TYPE go_service_process_open_fds gauge")
}

func TestComponentPostSessions(t *testing.T) {
	if !isComponentTest() {
		t.SkipNow()
	}

	tests := []struct {
		name         string
		sessionName  string
		sessionValue float64
	}{
		{
			"WithName",
			"me",
			0,
		},
		{
			"WithNameAndValue",
			"you",
			20,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cmp := NewComponent()

			reqBody := JSON{"name": tc.sessionName}
			if tc.sessionValue > 0 {
				reqBody["value"] = tc.sessionValue
			}

			statusCode, resBody, err := cmp.Call(context.Background(), "POST", "/sessions", reqBody)
			assert.NoError(t, err)
			assert.Equal(t, 201, statusCode)
			assert.NotEmpty(t, resBody["id"])
			assert.Equal(t, tc.sessionName, resBody["name"])
			if tc.sessionValue > 0 {
				assert.Equal(t, tc.sessionValue, resBody["value"])
			}
		})
	}
}

func TestComponentGetSession(t *testing.T) {
	if !isComponentTest() {
		t.SkipNow()
	}

	tests := []struct {
		name               string
		sessionID          string
		expectedStatusCode int
		expectedBody       JSON
	}{
		{
			"InvalidSessionID",
			"2222-bbbb-4444-dddd",
			404,
			JSON{
				"message": "Not found",
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cmp := NewComponent()

			endpoint := "/sessions" + tc.sessionID
			statusCode, resBody, err := cmp.Call(context.Background(), "GET", endpoint, nil)
			assert.NoError(t, err)
			assert.Equal(t, tc.expectedStatusCode, statusCode)
			assert.Equal(t, tc.expectedBody, resBody)
		})
	}
}
