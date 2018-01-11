const fs = require('fs')
const { makeExecutableSchema } = require('graphql-tools')

const resolvers = require('./resolvers')

const typeDefs = fs.readFileSync(__dirname + '/types.graphql').toString()

module.exports = makeExecutableSchema({ typeDefs, resolvers })
