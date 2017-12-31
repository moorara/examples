package main

import (
	"context"
	"log"
	"net"

	"github.com/moorara/toys/grpc/hello-world/server/service"
	"google.golang.org/grpc"
)

const port = ":8000"

type server struct{}

func (s *server) SayHello(ctx context.Context, req *service.HelloRequest) (*service.HelloResponse, error) {
	return &service.HelloResponse{Message: "Hello, " + req.Name}, nil
}

func main() {
	l, err := net.Listen("tcp", port)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	service.RegisterHelloServiceServer(s, &server{})
	s.Serve(l)
}
