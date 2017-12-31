package main

import (
	"crypto/tls"
	"crypto/x509"
	"io/ioutil"
	"log"
	"net"

	"github.com/moorara/toys/grpc/go-mtls/config"
	"github.com/moorara/toys/grpc/go-mtls/pb"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"
)

func main() {
	log.Printf("Using %s", config.CAChainFile)
	log.Printf("Using %s", config.ServerCertFile)
	log.Printf("Using %s", config.ServerKeyFile)

	cert, err := tls.LoadX509KeyPair(config.ServerCertFile, config.ServerKeyFile)
	if err != nil {
		log.Fatal(err)
	}

	ca, err := ioutil.ReadFile(config.CAChainFile)
	if err != nil {
		log.Fatal(err)
	}

	certPool := x509.NewCertPool()
	ok := certPool.AppendCertsFromPEM(ca)
	if !ok {
		log.Fatal("Failed to append certificate authority")
	}

	conn, err := net.Listen("tcp", config.ServerAddr)
	if err != nil {
		log.Fatal(err)
	}

	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{cert},
		ClientAuth:   tls.RequireAndVerifyClientCert,
		ClientCAs:    certPool,
	}

	creds := credentials.NewTLS(tlsConfig)
	opts := []grpc.ServerOption{grpc.Creds(creds)}
	server := grpc.NewServer(opts...)
	pb.RegisterUserServiceServer(server, NewUserServiceServer())

	log.Printf("Starting server on %s\n", config.ServerAddr)
	server.Serve(conn)
}
