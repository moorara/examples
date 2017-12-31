package main

import (
	"context"
	"errors"
	"io"
	"log"

	"github.com/moorara/toys/grpc/go-mtls/pb"
	"google.golang.org/grpc/metadata"
)

type UserService struct {
}

func NewUserServiceServer() pb.UserServiceServer {
	return &UserService{}
}

func (s *UserService) SendMetadata(ctx context.Context, in *pb.MetadataRequest) (*pb.MetadataResponse, error) {
	if md, ok := metadata.FromIncomingContext(ctx); ok {
		log.Printf("Metadata received: %+v\n", md)
	}

	return &pb.MetadataResponse{}, nil
}

func (s *UserService) GetUser(ctx context.Context, req *pb.GetUserRequest) (*pb.UserResponse, error) {
	for _, u := range users {
		if u.Id == req.Id {
			return &pb.UserResponse{User: &u}, nil
		}
	}

	return nil, errors.New("User not found")
}

func (s *UserService) GetAllUsers(req *pb.GetAllUsersRequest, stream pb.UserService_GetAllUsersServer) error {
	for _, u := range users {
		stream.Send(&pb.UserResponse{User: &u})
	}

	return nil
}

func (s *UserService) SaveUser(ctx context.Context, req *pb.UserRequest) (*pb.UserResponse, error) {
	for _, u := range users {
		if u.Id == req.User.Id {
			return nil, errors.New("User exists")
		}
	}

	users = append(users, *req.User)
	return &pb.UserResponse{User: req.User}, nil
}

func (s *UserService) SaveAllUsers(stream pb.UserService_SaveAllUsersServer) error {
	for {
		req, err := stream.Recv()
		if err == io.EOF {
			break
		} else if err != nil {
			return err
		}

		user := req.User
		users = append(users, *user)
		log.Printf("Add %+v\n", user)

		err = stream.Send(&pb.UserResponse{User: user})
		if err != nil {
			log.Fatal(err)
		}
	}

	return nil
}

func (s *UserService) UploadPhoto(stream pb.UserService_UploadPhotoServer) error {
	md, ok := metadata.FromIncomingContext(stream.Context())
	if !ok {
		return errors.New("Metadata not set")
	}
	userID := md["userid"][0]

	imageData := []byte{}
	for {
		chunk, err := stream.Recv()
		if err == io.EOF {
			log.Printf("Image received for user %v with length %v\n", userID, len(imageData))
			return stream.SendAndClose(&pb.UploadPhotoResponse{Done: true})
		} else if err != nil {
			return err
		}
		log.Printf("Received %v bytes\n", len(chunk.Data))
		imageData = append(imageData, chunk.Data...)
	}
}
