const fs = require('fs')
const path = require('path')
const express = require('express')
const cors = require('cors')
const graphqlHTTP = require('express-graphql')
const { makeExecutableSchema } = require('graphql-tools')

const typeDefs = fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'utf-8')
const resolvers = require('./resolvers')
const schema = makeExecutableSchema({ typeDefs, resolvers })

const port = 4000
const app = express()
app.use(cors())
app.use((req, res, next) => {
  console.info(`Serving ${req.method} ${req.path} ...`)
  next()
})
app.use('/', graphqlHTTP((req, res, graphQlParams) => {
  const context = { token: 'jwt' }
  return { schema, context, graphiql: true }
}))

app.listen(port)
console.log(`Listening on port ${port} ...`)
