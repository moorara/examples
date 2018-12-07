const express = require('express')
const graphqlHTTP = require('express-graphql')
const { buildSchema } = require('graphql')

const schema = buildSchema(`
  type Query {
    hello: String
  }
`)

const rootValue = {
  hello () {
    return 'Hello, World!'
  }
}

const port = 4000
const app = express()
app.use('/', graphqlHTTP({
  schema,
  rootValue,
  graphiql: true
}))

app.listen(port)
console.log(`Listening on port ${port} ...`)
