const http = require('http')
const express = require('express')
const bodyParser = require('body-parser')
const { execute, subscribe } = require('graphql')
const { SubscriptionServer } = require('subscriptions-transport-ws')
const { graphqlExpress, graphiqlExpress } = require('apollo-server-express')

const schema = require('./schema')
const connectMongo = require('./mongo')
const Dataloaders = require('./dataloaders')
const { authenticate } = require('./auth')
const { formatError } = require('./util')

const PORT = 3000

async function start () {
  let mongo

  try {
    mongo = await connectMongo()
  } catch (err) {
    return console.log(err)
  }

  const buildOptions = async (req, res) => {
    const user = await authenticate(req, mongo.Users)
    return {
      schema,
      formatError,
      context: {
        mongo,
        user,
        dataloaders: Dataloaders(mongo)
      }
    }
  }

  const app = express()
  app.use('/graphql', bodyParser.json(), graphqlExpress(buildOptions))
  app.get('/graphiql', graphiqlExpress({
    endpointURL: '/graphql',
    subscriptionsEndpoint: `ws://localhost:${PORT}/subscriptions`,
    passHeader: `'Authorization': 'bearer token-moorara@yahoo.com'`,
  }))

  const server = http.createServer(app)
  server.listen(PORT, (err) => {
    if (err) {
      return console.log(err)
    }

    SubscriptionServer.create(
      { execute, subscribe, schema },
      { server, path: '/subscriptions' },
    )

    console.log(`GraphQL server running on port ${PORT} ...`)
  })
}

start()
