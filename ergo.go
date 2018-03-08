package main

import (
	"fmt"
)

// #cgo LDFLAGS: -L /usr/local/lib/erlang/usr/lib -l erts -l erl_interface -l erts_r -l ei  -l ei_st  -l erl_interface  -l erl_interface_st  -l ic

// #cgo CFLAGS:  -I /usr/local/lib/erlang/usr/include
// #cgo LDFLAGS: -shared -L /usr/local/lib/erlang/usr/lib -L/usr/local/lib/erlang/lib/erl_interface-3.10.1/lib -lerl_interface -lei
import "C"

//export Call
func Call(pkg string, funcName string, args []interface{}) {
	fmt.Printf("Call(%q, %q, %v)\n", pkg, funcName, args)
}

func main() {}
