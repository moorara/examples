wasm:
	GOOS=js GOARCH=wasm go build -o ./public/main.wasm

server:
	go run ./server/main.go -dir ./public

.PHONY: wasm server
