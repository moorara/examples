package main

import (
	"fmt"
	"net/http"

	"github.com/graphql-go/graphql"
	"github.com/graphql-go/handler"
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

	h := handler.New(&handler.Config{
		Schema:     &schema,
		Pretty:     true,
		GraphiQL:   false,
		Playground: true,
	})

	http.Handle("/", h)

	fmt.Println("Listening on port 5000 ...")
	http.ListenAndServe(":5000", nil)
}
