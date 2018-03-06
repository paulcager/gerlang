package main

import (
	"C"
	"fmt"
)

//export Call
func Call(pkg string, funcName string, args []interface{}) {
	fmt.Printf("Call(%q, %q, %v)\n", pkg, funcName, args)
}

func main() {}
