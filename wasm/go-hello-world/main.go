package main

import (
	"fmt"
	"syscall/js"
)

func main() {
	fmt.Println("Hello, World!")
	js.Global().Call("alert", "Hello, World!")
	js.Global().Get("document").Call("write", "Hello, World!")
}
