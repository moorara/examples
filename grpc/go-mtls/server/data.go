package main

import (
	"github.com/moorara/toys/grpc/go-mtls/pb"
)

var users = []pb.User{
	pb.User{
		Id:       1,
		Username: "milad",
		Email:    "milad@example.com",
		Type:     pb.User_FREE,
	},
	pb.User{
		Id:       2,
		Username: "mona",
		Email:    "mona@example.com",
		Type:     pb.User_FREE,
	},
	pb.User{
		Id:       3,
		Username: "moorara",
		Email:    "moorara@example.com",
		Type:     pb.User_PREMIUM,
	},
	pb.User{
		Id:       4,
		Username: "mimnic",
		Email:    "mimnic@example.com",
		Type:     pb.User_PREMIUM,
	},
}
