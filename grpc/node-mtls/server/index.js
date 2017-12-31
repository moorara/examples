const fs = require('fs')
const grpc = require('grpc')
const users = require('./data.js')

const SERVER_ADDR = 'localhost:8000'

const PROTOBUF_PATH = './pb/user.proto'
const protoBuf = grpc.load(PROTOBUF_PATH)

const caChain = fs.readFileSync('./certs/interm.ca.chain'),
      serverKey = fs.readFileSync('./certs/localhost.key'),
      serverCert = fs.readFileSync('./certs/localhost.cert')

function startServer () {
  const creds = grpc.ServerCredentials.createSsl(caChain, [{
    'private_key': serverKey,
    'cert_chain': serverCert,
  }], true)

  const server = new grpc.Server()
  server.addService(protoBuf.UserService.service, {
    sendMetadata, getUser, getAllUsers, saveUser, saveAllUsers, uploadPhoto
  })
  
  console.log(`Starting server on ${SERVER_ADDR}`)
  server.bind(SERVER_ADDR, creds)
  server.start()
}

function sendMetadata(call, callback) {
  const md = call.metadata.getMap()
  for (let key in md) {
    console.log(key, md[key])
  }

  callback(null, {})
}

function getUser(call, callback) {
  const userId = call.request.id
  for (let user of users) {
    if (user.id === userId) {
      return callback(null, { user })
    }
  }

  callback('User not fonud')
}

function getAllUsers(call) {
  users.forEach(user => {
    call.write({ user })
  })

  call.end()
}

function saveUser(call, callback) {
  const user = call.request.user
  for (let u of users) {
    if (u.id === user.id) {
      return callback('User exists')
    }
  }

  users.push(user)
  callback(null, { user })
}

function saveAllUsers(call) {
  call.on('data', (res) => {
    users.push(res.user)
    call.write({ user: res.user })
  })
  call.on('end', () => {
    call.end()
  })
}

function uploadPhoto(call, callback) {
  const md = call.metadata.getMap()
  const userId = md['userid']

  let buff = new Buffer(0)
  call.on('data', (chunk) => {
    buff = Buffer.concat([buff, chunk.data])
    console.log(`Image received for user ${userId} with length ${chunk.data.length}`)
  })
  call.on('end', () => {
    callback(null, { done: true })
    console.log(`Received ${buff.length} bytes`)
  })
}

startServer()
