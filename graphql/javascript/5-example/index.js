const fs = require('fs')
const path = require('path')
const express = require('express')
const graphqlHTTP = require('express-graphql')
const { buildSchema } = require('graphql')

const typeDefs = fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'utf-8')
const schema = buildSchema(typeDefs)
const rootValue = require('./resolvers')

const port = 4000
const app = express()
app.use('/', graphqlHTTP({
  schema,
  rootValue,
  graphiql: true
}))

app.listen(port)
console.log(`Listening on port ${port} ...`)
