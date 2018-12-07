const { graphql, buildSchema } = require('graphql')

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

const query = '{ hello }'

const run = async () => {
  try {
    const result = await graphql(schema, query, rootValue)
    console.log(result.data.hello)
  } catch (err) {
    console.error(err.message)
  }
}

run()
