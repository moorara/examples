package main

import (
	"fmt"
	"log"

	"github.com/graphql-go/graphql"
)

func main() {
	schema, _ := graphql.NewSchema(
		graphql.SchemaConfig{
			Query: graphql.NewObject(
				graphql.ObjectConfig{
					Name: "RootQuery",
					Fields: graphql.Fields{
						"hello": &graphql.Field{
							Type: graphql.String,
							Resolve: func(params graphql.ResolveParams) (interface{}, error) {
								return "Hello, World!", nil
							},
						},
					},
				},
			),
		},
	)

	query := `{ hello }`

	params := graphql.Params{
		Schema:        schema,
		RequestString: query,
	}

	result := graphql.Do(params)
	if len(result.Errors) > 0 {
		log.Fatalf("Error on executing GraphQL query. %v", result.Errors)
	}

	resp, _ := result.Data.(map[string]interface{})

	fmt.Printf("%+v\n", resp["hello"])
}
