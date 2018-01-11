const { URL } = require('url')
const { ObjectID } = require('mongodb')
const { PubSub } = require('graphql-subscriptions')

const pubsub = new PubSub()

class ValidationError extends Error {
  constructor(message, field) {
    super(message)
    this.field = field
  }
}

function assertValidLink ({ url }) {
  try {
    new URL(url)
  } catch (err) {
    throw new ValidationError(`${err}`, url)
  }
}

function buildFilters({ url_contains, description_contains }) {
  const filter = (url_contains || description_contains) ? {} : null
  if (url_contains) filter.url = { $regex: `.*${url_contains}.*` }
  if (description_contains) filter.description = { $regex: `.*${description_contains}.*` }
  let filters = filter ? [filter] : []
  return filters
}

module.exports = {
  Query: {
    allLinks: async (root, data, context) => {
      let query = data.filter ? { $or: buildFilters(data.filter) } : {}
      const cursor = context.mongo.Links.find(query)
      if (data.first) {
        cursor.limit(data.first);
      }
      if (data.skip) {
        cursor.skip(data.skip);
      }
      return cursor.toArray()
    },
  },
  Mutation: {
    createUser: async (root, data, context) => {
      const newUser = {
        name: data.name,
        email: data.authProvider.email,
        password: data.authProvider.password
      }
      const response = await context.mongo.Users.insert(newUser)
      return Object.assign({ id: response.insertedIds[0] }, newUser)
    },
    signinUser: async (root, data, context) => {
      const user = await context.mongo.Users.findOne({ email: data.authProvider.email })
      if (data.authProvider.password === user.password) {
        return { user, token: `token-${user.email}` }
      }
    },
    createLink: async (root, data, { mongo, user }) => {
      assertValidLink(data)
      const newLink = Object.assign({ postedById: user && user._id }, data)
      const response = await mongo.Links.insert(newLink)
      newLink.id = response.insertedIds[0]
      pubsub.publish('Link', { Link: { mutation: 'CREATED', link: newLink } })
      return newLink
    },
    createVote: async (root, data, { mongo, user }) => {
      const newVote = {
        userId: user && user._id,
        linkId: new ObjectID(data.linkId)
      }
      const response = await mongo.Votes.insert(newVote)
      return Object.assign({ id: response.insertedIds[0] }, newVote)
    },
  },
  User: {
    id: root => root._id || root.id,
    votes: async ({ _id }, data, { mongo }) => {
      return await mongo.Votes.find({ userId: _id }).toArray()
    },
  },
  Link: {
    id: root => root._id || root.id,
    postedBy: async ({ postedById }, data, context) => {
      return await context.dataloaders.userLoader.load(postedById)
    },
    votes: async ({ _id }, data, { mongo }) => {
      return await mongo.Votes.find({ linkId: _id }).toArray()
    },
  },
  Vote: {
    id: root => root._id || root.id,
    user: async ({ userId }, data, context) => {
      return await context.dataloaders.userLoader.load(userId)
    },
    link: async ({ linkId }, data, context) => {
      return await context.mongo.Links.findOne({ _id: linkId })
    },
  },
  Subscription: {
    Link: {
      subscribe: () => pubsub.asyncIterator('Link'),
    },
  },
}
