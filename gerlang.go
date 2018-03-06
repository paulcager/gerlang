package main

import (
	"fmt"
	"reflect"

	"unsafe"

	"github.com/kr/pretty"
)

// #cgo CFLAGS: -Inif/include
// #cgo LDFLAGS: -Lnif/lib -lerl_interface -lei -fpic -shared
//
// #include "erl_nif.h"
// extern ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
// extern int get_long(ErlNifEnv* env, ERL_NIF_TERM term, long* ip);
// extern char *sprint_term(ErlNifEnv* env, ERL_NIF_TERM term);
//
// // NB: do not attempt to define functions here: having Call exported means that
// // they would be defined twice.
import "C"

//export Call
func Call(env *C.ErlNifEnv, pluginFile string, funcName string, args []C.ERL_NIF_TERM) []C.ERL_NIF_TERM {
	fmt.Printf("[inline version] Call(%q, %q, %v)\n", pluginFile, funcName, args)

	for _, term := range args {
		value := new(int)
		err := translate(env, term, reflect.ValueOf(value))
		fmt.Println("err is", err)
		pretty.Println(value)
	}

	return nil
}

func translate(env *C.ErlNifEnv, term C.ERL_NIF_TERM, value reflect.Value) error {
	if !value.Elem().CanSet() {
		return fmt.Errorf("Cannot translate %v because %v is not settable", term, value)
	}

	kind := value.Elem().Kind()
	switch kind {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64, reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		var i C.long
		if C.get_long(env, term, &i) == 0 {
			fmt.Printf("Not an int: %v", term)
			return fmt.Errorf("Cannot translate %x (%s) because it is not an int", term, SprintTerm(env, term))
		}
		value.Elem().SetInt(int64(i))
	default:
		fmt.Printf("Unexpected kind %d, %v", kind, value)
	}
	return nil
}

func SprintTerm(env *C.ErlNifEnv, term C.ERL_NIF_TERM) string {
	cStr := C.sprint_term(env, term)
	s := C.GoString(cStr)
	C.free(unsafe.Pointer(cStr))
	return s
}

func main() {}
