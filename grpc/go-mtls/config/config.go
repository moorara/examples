package config

import (
	"os"
	"path/filepath"
)

const (
	ServerAddr = "localhost:8000"
)

var (
	CAChainFile                   string
	ServerCertFile, ServerKeyFile string
	ClientCertFile, ClientKeyFile string
)

func init() {
	path, _ := filepath.Abs(os.Args[0])

	CAChainFile = os.Getenv("CA_CHAIN_FILE")
	if CAChainFile == "" {
		CAChainFile = filepath.Dir(filepath.Dir(path)) + "/certs/interm.ca.chain"
	}

	ServerCertFile = os.Getenv("SEREVR_CERT_FILE")
	if ServerCertFile == "" {
		ServerCertFile = filepath.Dir(filepath.Dir(path)) + "/certs/localhost.cert"
	}

	ServerKeyFile = os.Getenv("SEREVR_KEY_FILE")
	if ServerKeyFile == "" {
		ServerKeyFile = filepath.Dir(filepath.Dir(path)) + "/certs/localhost.key"
	}

	ClientCertFile = os.Getenv("CLIENT_CERT_FILE")
	if ClientCertFile == "" {
		ClientCertFile = filepath.Dir(filepath.Dir(path)) + "/certs/client.cert"
	}

	ClientKeyFile = os.Getenv("CLIENT_KEY_FILE")
	if ClientKeyFile == "" {
		ClientKeyFile = filepath.Dir(filepath.Dir(path)) + "/certs/client.key"
	}
}
