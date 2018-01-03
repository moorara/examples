# gRPC Toys
This repo includes code examples demoing [gRPC](https://grpc.io) in miscellaneous languages.

## hello-world
Simple gRPC hello-world server in Go and client in Node.

## go-mtls
Example gRPC server and client with mutual TLS both written in Go.

```
cd grpc/go-mtls
make server && ./server/server
make client && ./client/client -c metadata
make client && ./client/client -c getuser
make client && ./client/client -c getallusers
make client && ./client/client -c saveuser
make client && ./client/client -c saveallusers
make client && ./client/client -c uploadphoto
```

## node-mtls
Example gRPC server and client with mutual TLS both written in Node.

```
cd grpc/node-mtls
yarn
yarn server
yarn client metadata
yarn client getuser
yarn client getallusers
yarn client saveuser
yarn client saveallusers
yarn client uploadphoto
```
