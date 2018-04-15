package testing

import (
	"fmt"
)

type Struct struct {
	S   string
	I64 int64
	I   int
	Sub struct {
		B []byte
	}
}

func TestStruct(s Struct) string {
	return fmt.Sprintf("%v", s)
}

func TestMap(m map[string]interface{}) string {
	return fmt.Sprintf("%v", m)
}
