path := $(shell pwd)


install:
	go get -u github.com/golang/protobuf/proto
	go get -u github.com/golang/protobuf/protoc-gen-go

pb:
	@ cd pb && \
	  protoc user.proto --go_out=plugins=grpc:.

server:
	@ go build -o ./server/server ./server

client:
	@ go build -o ./client/client ./client


.PHONY: install pb server client
