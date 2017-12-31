package main

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"flag"
	"io"
	"io/ioutil"
	"log"
	"os"

	"github.com/moorara/toys/grpc/go-mtls/config"
	"github.com/moorara/toys/grpc/go-mtls/pb"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/metadata"
)

func main() {
	log.Printf("Using %s\n", config.CAChainFile)
	log.Printf("Using %s\n", config.ClientCertFile)
	log.Printf("Using %s\n", config.ClientKeyFile)

	cert, err := tls.LoadX509KeyPair(config.ClientCertFile, config.ClientKeyFile)
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

	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{cert},
		RootCAs:      certPool,
		ServerName:   "localhost",
	}

	creds := credentials.NewTLS(tlsConfig)
	opts := []grpc.DialOption{grpc.WithTransportCredentials(creds)}
	conn, err := grpc.Dial(config.ServerAddr, opts...)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	client := pb.NewUserServiceClient(conn)
	log.Printf("Client connected to server %s\n", config.ServerAddr)

	command := flag.String("c", "", "Command to run")
	flag.Parse()

	switch *command {
	case "metadata":
		sendMetadata(client)
	case "getuser":
		getUser(client)
	case "getallusers":
		getAllUsers(client)
	case "saveuser":
		saveUser(client)
	case "saveallusers":
		saveAllUsers(client)
	case "uploadphoto":
		uploadPhoto(client)
	}
}

func sendMetadata(client pb.UserServiceClient) {
	md := metadata.New(map[string]string{
		"user":  "moorara",
		"token": "api-token",
	})

	ctx := metadata.NewOutgoingContext(context.Background(), md)
	_, err := client.SendMetadata(ctx, &pb.MetadataRequest{})
	if err != nil {
		log.Fatal(err)
	}
}

func getUser(client pb.UserServiceClient) {
	ctx := context.Background()
	res, err := client.GetUser(ctx, &pb.GetUserRequest{Id: 3})
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("%+v\n", res.User)
}

func getAllUsers(client pb.UserServiceClient) {
	ctx := context.Background()
	stream, err := client.GetAllUsers(ctx, &pb.GetAllUsersRequest{})
	if err != nil {
		log.Fatal(err)
	}

	for {
		res, err := stream.Recv()
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}

		log.Printf("%+v\n", res.User)
	}
}

func saveUser(client pb.UserServiceClient) {
	user := &pb.User{
		Id:       5,
		Username: "lusk",
		Email:    "lusk@example.com",
		Type:     pb.User_FREE,
	}

	ctx := context.Background()
	res, err := client.SaveUser(ctx, &pb.UserRequest{User: user})
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("%+v\n", res.User)
}

func saveAllUsers(client pb.UserServiceClient) {
	users := []pb.User{
		pb.User{
			Id:       6,
			Username: "phoenix",
			Email:    "phoenix@example.com",
			Type:     pb.User_PREMIUM,
		},
		pb.User{
			Id:       7,
			Username: "griffin",
			Email:    "griffin@example.com",
			Type:     pb.User_PREMIUM,
		},
	}

	ctx := context.Background()
	stream, err := client.SaveAllUsers(ctx)
	if err != nil {
		log.Fatal(err)
	}

	doneCh := make(chan struct{})
	go func() {
		for {
			res, err := stream.Recv()
			if err == io.EOF {
				doneCh <- struct{}{}
				break
			} else if err != nil {
				log.Fatal(err)
			}

			log.Printf("%+v\n", res.User)
		}
	}()

	for _, u := range users {
		err := stream.Send(&pb.UserRequest{User: &u})
		if err != nil {
			log.Fatal(err)
		}
	}

	err = stream.CloseSend()
	if err != nil {
		log.Fatal(err)
	}

	<-doneCh
}

func uploadPhoto(client pb.UserServiceClient) {
	f, err := os.Open("Vegan.png")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	md := metadata.Pairs("userid", "1")
	ctx := metadata.NewOutgoingContext(context.Background(), md)
	stream, err := client.UploadPhoto(ctx)
	if err != nil {
		log.Fatal(err)
	}

	for {
		chunk := make([]byte, 4*1024)
		n, err := f.Read(chunk)
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}

		if n < len(chunk) {
			chunk = chunk[:n]
		}
		stream.Send(&pb.UploadPhotoRequest{Data: chunk})
	}

	res, err := stream.CloseAndRecv()
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("%+v\n", res.Done)
}
