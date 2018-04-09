package old

import (
	"fmt"
	"reflect"

	"unsafe"

	"plugin"

	"github.com/kr/pretty"
)

// #cgo CFLAGS: -Inif/include
// #cgo LDFLAGS: -Lnif/lib -lerl_interface -lei -fpic -shared
//
// #include "erl_nif.h"
// extern ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
// extern ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* reason);
// extern int get_long(ErlNifEnv* env, ERL_NIF_TERM term, long* ip);
// extern char *sprint_term(ErlNifEnv* env, ERL_NIF_TERM term);
//
// // NB: do not attempt to define functions here: having Call exported means that
// // they would be defined twice.
import "C"

func makeAtom(env *C.ErlNifEnv, s string) C.ERL_NIF_TERM {
	str := C.CString(s)
	defer C.free(unsafe.Pointer(str))
	return C.mk_atom(env, str)
}

func makeError(env *C.ErlNifEnv, s string) C.ERL_NIF_TERM {
	str := C.CString(s)
	defer C.free(unsafe.Pointer(str))
	return C.mk_error(env, str)
}

//export Call
func Call(env *C.ErlNifEnv, pluginFile string, funcName string, args []C.ERL_NIF_TERM) C.ERL_NIF_TERM {
	p, err := plugin.Open(pluginFile)
	if err != nil {
		return makeError(env, fmt.Sprintf("Could not open %#q: %s", pluginFile, err))
	}

	sym, err := p.Lookup(funcName)
	if err != nil {
		return makeError(env, fmt.Sprintf("Could not find %#q: %s", funcName, err))
	}

	fmt.Printf("sym is %T: %v", sym, sym)

	for _, term := range args {
		value := new(int)
		err := translate(env, term, reflect.ValueOf(value))
		fmt.Println("err is", err)
		pretty.Println(value)
	}

	return makeAtom(env, "todo")
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
			return fmt.Errorf("Cannot translate (%s) - expected an int", SprintTerm(env, term))
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
