package main

import (
	"fmt"
)

// #cgo LDFLAGS: -L /usr/local/lib/erlang/usr/lib -l erts -l erl_interface -l erts_r -l ei  -l ei_st  -l erl_interface  -l erl_interface_st  -l ic

// #cgo CFLAGS:  -I ./include
// #cgo LDFLAGS: -shared -o gerlang.so -L./lib -L/home/paul/kerl/20.2/usr/lib -lerl_interface -lei -lerl_interface_st -lic

// /* Comment here just so Goland doesn't remove import. */
import "C"

//export Call
func Call(pkg string, funcName string, args []interface{}) {
	fmt.Printf("Call(%q, %q, %v)\n", pkg, funcName, args)
}

func main() {}
