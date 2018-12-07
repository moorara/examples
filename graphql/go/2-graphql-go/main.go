package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"

	graphql "github.com/graph-gophers/graphql-go"
)

const (
	schemaStr = `
		schema {
			query: Query
		}

		type Query {
			hello: String!
		}
	`
)

type resolver struct{}

func (r *resolver) Hello(ctx context.Context) (string, error) {
	return "Hello, World!", nil
}

func main() {
	query := `{ hello }`
	schema := graphql.MustParseSchema(schemaStr, &resolver{})

	result := schema.Exec(context.Background(), query, "", nil)
	if len(result.Errors) > 0 {
		log.Fatalf("Error on executing GraphQL query. %v", result.Errors)
	}

	resp := make(map[string]interface{})
	json.Unmarshal(result.Data, &resp)

	fmt.Printf("%+v\n", resp["hello"])
}
