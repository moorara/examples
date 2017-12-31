const path = require('path')
const grpc = require('grpc')

const SERVER_ADDR = 'localhost:8000'
const PROTO_PATH = path.join('protobuf', 'messages.proto')

const HelloService = grpc.load(PROTO_PATH).HelloService
const client = new HelloService(SERVER_ADDR, grpc.credentials.createInsecure())


function start () {
  client.sayHello({ name: 'Milad' }, (err, res) => {
    if (err) {
      console.log(err)
      return
    }
    console.log(res.message)
  })
}


start()
