install:
	go get -u github.com/golang/protobuf/proto
	go get -u github.com/golang/protobuf/protoc-gen-go

generate:
	protoc protobuf/messages.proto --go_out=plugins=grpc:${GOPATH}/src

server:
	go run server/main.go

client:
	node client/index.js


.PHONY: install generate server client
