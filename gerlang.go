package main

import (
	"fmt"
)

// /* Comment here just so Goland doesn't remove import. */
import "C"

//export Call
func Call(pluginFile string, funcName string, args []interface{}) []interface{} {
	fmt.Printf("Call(%q, %q, %v)\n", pluginFile, funcName, args)
	return nil
}

func main() {}
