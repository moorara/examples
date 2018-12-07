package main

import (
	"errors"
	"reflect"
	"unicode"

	"github.com/graphql-go/graphql"
)

const (
	idName       = "ID"
	schemaName   = "Schema"
	queryName    = "Query"
	mutationName = "Mutation"

	nameTag     = "graphql"
	descriptTag = "description"
)

/*
 * tokenize breaks a field name into its tokens (words).
 *   Query  --> Query
 *   UserID --> User, ID
 */
func tokenize(name string) []string {
	tokens := []string{}
	current := string(name[0])
	lastLower := unicode.IsLower(rune(name[0]))

	add := func(slice []string, str string) []string {
		if str == "" {
			return slice
		}
		return append(slice, str)
	}

	for i := 1; i < len(name); i++ {
		r := rune(name[i])

		if unicode.IsUpper(r) && lastLower {
			// The case is changing from lower to upper
			tokens = add(tokens, current)
			current = string(name[i])
		} else if unicode.IsLower(r) && !lastLower {
			// The case is changing from upper to lower
			l := len(current) - 1
			tokens = add(tokens, current[:l])
			current = current[l:] + string(name[i])
		} else {
			// Increment current token
			current += string(name[i])
		}

		lastLower = unicode.IsLower(r)
	}

	tokens = append(tokens, string(current))

	return tokens
}

func getName(val reflect.Value) string {
	tokens := tokenize()
}

func getDescription(val reflect.Value) string {

}

// GetOutput returns a graphql.Output for a value/type
func GetOutput(val reflect.Value) *graphql.Object {

}

// GetObject returns a graphql.Object for a value/type
func GetObject(val reflect.Value) *graphql.Object {
	objectConfig := graphql.ObjectConfig{
		Name:        getName(val),
		Description: getDescription(val),
		Fields:      graphql.Fields{},
	}

	for i := 0; i < val.NumMethod(); i++ {
		v := val.Method(i)
		t := v.Type()

		name := getName()
		field := graphql.Field{
			Name: name,
			Type: GetOutput(),
			Resolve: func(params graphql.ResolveParams) (interface{}, error) {
				return nil, nil
			},
		}

		objectConfig.Fields[name] = field
	}

	return graphql.NewObject(objectConfig)
}

// NewSchema creates a new graphql.Schema from struct
func NewSchema(schema interface{}) (*graphql.Schema, error) {
	v := reflect.ValueOf(schema)
	t := reflect.TypeOf(schema)

	// If a pointer is passed, navigate to the value
	if t.Kind() == reflect.Ptr {
		v = v.Elem()
		t = t.Elem()
	}

	// Make sure schema is a struct
	if t.Kind() != reflect.Struct /* || t.Name() != schemaName */ {
		return nil, errors.New("schema should be struct")
	}

	// Schema must have Query
	qv := v.FieldByName(queryName)
	if !qv.IsValid() || qv.Type().Kind() != reflect.Struct /* || qv.Type().Name() != queryName */ {
		return nil, errors.New("Query should be struct")
	}

	schemaConfig := graphql.SchemaConfig{
		Query: GetObject(qv),
	}

	// Check if schema has Mutation
	mv := v.FieldByName(mutationName)
	if mv.IsValid() {
		if mv.Type().Kind() == reflect.Struct /* && mv.Type().Name() != mutationName */ {
			schemaConfig.Mutation = GetObject(mv)
		} else {
			return nil, errors.New("Mutation should be struct")
		}
	}

	graphqlSchema, err := graphql.NewSchema(schemaConfig)
	if err != nil {
		return nil, err
	}

	return &graphqlSchema, nil
}
