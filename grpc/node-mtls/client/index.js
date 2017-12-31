const fs = require('fs')
const grpc = require('grpc')
const process = require('process')

const SERVER_ADDR = 'localhost:8000'

const PROTOBUF_PATH = './pb/user.proto'
const protoBuf = grpc.load(PROTOBUF_PATH)

const caChain = fs.readFileSync('./certs/interm.ca.chain'),
      clientKey = fs.readFileSync('./certs/client.key'),
      clientCert = fs.readFileSync('./certs/client.cert')

function runClient () {
  const creds = grpc.credentials.createSsl(caChain, clientKey, clientCert)
  const client = new protoBuf.UserService(SERVER_ADDR, creds, {
    'grpc.ssl_target_name_override' : 'localhost',
    'grpc.default_authority': 'localhost'
  })

  const command = process.argv[2]
  switch (command) {
    case 'metadata':
      sendMetadata(client)
      break
    case 'getuser':
      getUser(client)
      break
    case 'getallusers':
      getAllUsers(client)
      break
    case "saveuser":
      saveUser(client)
      break
    case "saveallusers":
      saveAllUsers(client)
      break
    case "uploadphoto":
      uploadPhoto(client)
      break
  }
}

function sendMetadata (client) {
  const md = new grpc.Metadata()
  md.add('username', 'moorara')
  md.add('token', 'api-token')

  client.sendMetadata({}, md, () => console.log('done'))
}

function getUser (client) {
  client.getUser({ id: 3 }, (err, res) => {
    if (err) {
      console.log(err)
      return
    }

    console.log(res.user)
  })
}

function getAllUsers (client) {
  const call = client.getAllUsers({})

  call.on('data', (data) => {
    console.log(data.user)
  })
}

function saveUser (client) {
  let user = {
    id: 5,
    username: 'lusk',
    email: 'lusk@example.com',
    type: 'FREE'
  }

  client.saveUser({ user }, (err, res) => {
    if (err) {
      console.log(err)
      return
    }

    console.log(res.user)
  })
}

function saveAllUsers (client) {
  const users = [
		{
			id:       6,
			username: 'phoenix',
			email:    'phoenix@example.com',
			type:     'PREMIUM',
		},
		{
			id:       7,
			username: 'griffin',
			email:    'griffin@example.com',
			type:     'PREMIUM',
		}
  ]
  
  const call = client.saveAllUsers()
  call.on('data', (res) => {
    console.log(res.user)
  })
  users.forEach((user) => {
    call.write({ user })
  })
  call.end()
}

function uploadPhoto (client) {
  const md = new grpc.Metadata()
  md.add('userid', '1')

  const call = client.uploadPhoto(md, (err, res) => {
    if (err) {
      console.log(err)
      return
    }

    console.log(res)
  })

  const stream = fs.createReadStream('./Vegan.png')
  stream.on('error', (err) => {
    console.log(err)
  })
  stream.on('data', (chunk) => {
    call.write({ data: chunk })
  })
  stream.on('end', () => {
    call.end()
  })
}

runClient()
