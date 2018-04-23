package testing

import (
	"errors"
	"fmt"
	"time"
)

type Struct struct {
	S   string
	I64 int64
	I   int
	Sub struct {
		B []byte
	}
}

func TestBasic(i int, i32 int32, ui uint64, b bool, f float64, by byte, s string, bs []byte) string {
	return fmt.Sprint(i, i32, ui, b, f, by, " ", s, " ", bs)
}

func TestStruct(s Struct) *Struct {
	return &s
}

func TestMap(m map[string]interface{}) interface{} {
	m["new"] = 22
	return m
}

func TestReturnMultiple(returnError bool) (Struct, string, error) {
	if returnError {
		return Struct{}, "", errors.New("returnedError")
	}

	return Struct{S: "str", I64: 123, I: 456, Sub: struct{ B []byte }{B: []byte{4, 5}}}, "ok", nil
}

func TestReturnTime() time.Time {
	return time.Now()
}
