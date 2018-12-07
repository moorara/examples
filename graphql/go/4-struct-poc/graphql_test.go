package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

type (
	Query    struct{}
	Mutation struct{}
	Schema   struct {
		Query
		Mutation
	}
)

func (q Query) Hello() (string, error) {
	return "Hello, World!", nil
}

func TestNewSchema(t *testing.T) {
	tests := []struct {
		name          string
		schema        interface{}
		expectedError string
	}{
		{
			name:          "NoStruct",
			schema:        "This is not a struct type",
			expectedError: "schema should be struct",
		},
		{
			name:          "NoQuery",
			schema:        struct{}{},
			expectedError: "Query should be struct",
		},
		{
			name: "NoStructQuery",
			schema: struct {
				Query string
			}{},
			expectedError: "Query should be struct",
		},
		{
			name: "NoMutation",
			schema: struct {
				Query struct{}
			}{},
			expectedError: "",
		},
		{
			name: "NoStructMutation",
			schema: struct {
				Query    struct{}
				Mutation string
			}{},
			expectedError: "Mutation should be struct",
		},
		{
			name:          "Success",
			schema:        &Schema{},
			expectedError: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := NewSchema(tc.schema)

			if tc.expectedError == "" {
				assert.NoError(t, err)
			} else {
				assert.Equal(t, tc.expectedError, err.Error())
			}
		})
	}
}
