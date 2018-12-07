const {
  graphql,
  GraphQLSchema,
  GraphQLObjectType,
  GraphQLString
} = require('graphql')

const schema = new GraphQLSchema({
  query: new GraphQLObjectType({
    name: 'RootQueryType',
    fields: {
      hello: {
        type: GraphQLString,
        resolve () {
          return 'Hello, World!'
        }
      }
    }
  })
})

const query = '{ hello }'

graphql(schema, query).then(result => {
  console.log(result.data.hello)
}).catch(err => {
  console.error(err.message)
})
