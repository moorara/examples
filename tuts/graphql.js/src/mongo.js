const { MongoClient, Logger } = require('mongodb')

const MONGO_URL = 'mongodb://localhost:27017';

module.exports = async () => {
  let db

  try {
  const client = await MongoClient.connect(MONGO_URL)
  db = client.db('hackernews')
  } catch (err) {
    throw err
  }

  let logCount = 0;
  Logger.setLevel('debug')
  Logger.filter('class', [ 'Cursor' ])
  Logger.setCurrentLogger((msg, state) => {
    console.log(`MONGO DB REQUEST ${++logCount}: ${msg}`)
  })

  return {
    Links: db.collection('links'),
    Users: db.collection('users'),
    Votes: db.collection('votes')
  }
}
