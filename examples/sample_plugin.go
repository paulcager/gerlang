// +build examples

package main

import "fmt"

func SimpleParams(a int, b string) string {
	return fmt.Sprintf("a=%d, b=%s", a, b)
}

func main() {}
