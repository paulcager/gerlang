package runtime

// #include "erl_nif.h"
import "C"

import (
	"fmt"
	"reflect"
	"unsafe"
)

// MakeTerm creates an Erlang term corresponding to the Golang object passed in.
func MakeTerm(env *C.ErlNifEnv, value interface{}) C.ERL_NIF_TERM {
	// First some special cases.
	if err, ok := value.(error); ok {
		return MakeError(env, err.Error())
	}

	if b, ok := value.([]byte); ok {
		return MakeBinary(env, b)
	}

	v := reflect.ValueOf(value)

	// TODO - where to take care of nils? Prob best in each kind, since a nil map and a nil []byte
	// would be different things to erlang.

	switch kind := v.Kind(); kind {
	case reflect.Ptr:
		return MakeTerm(env, v.Elem().Interface())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		l := v.Int()
		return C.enif_make_int64(env, C.long(l))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		l := v.Uint()
		return C.enif_make_int64(env, C.long(l))
	case reflect.String:
		return MakeString(env, v.String())
	case reflect.Bool:
		if v.Bool() {
			return MakeAtom(env, "true")
		}
		return MakeAtom(env, "false")
	case reflect.Map:
		keyType := v.Type().Key()
		valueType := v.Type().Elem()
		keys := v.MapKeys()
		_, _, _ = keyType, valueType, keys
		panic("TODO")
	case reflect.Slice, reflect.Array:
		sLen := v.Len()
		elements := make([]interface{}, sLen)
		for i := 0; i < sLen; i++ {
			elements[i] = v.Index(i).Interface()
		}
		return MakeList(env, elements)
	case reflect.Struct:
		sLen := v.NumField()
		elements := make([]interface{}, sLen)
		for i := 0; i < sLen; i++ {
			elements[i] = v.Field(i).Interface()
		}
		return MakeTuple(env, elements)

		// And all the others - Channel, Func, ... ... ...

	default:
		fmt.Println("Kind", kind, value == nil)
	}

	return 0
}

func MakeReturnValue(env *C.ErlNifEnv, results []interface{}) C.ERL_NIF_TERM {
	if len(results) == 0 {
		return MakeAtom(env, "ok")
	}
	return MakeTuple(env, results)
}

func MakeAtom(env *C.ErlNifEnv, name string) C.ERL_NIF_TERM {
	str := C.CString(name)
	defer C.free(unsafe.Pointer(str))
	return C.enif_make_atom(env, str)
}

func MakeError(env *C.ErlNifEnv, reason string) C.ERL_NIF_TERM {
	err := MakeAtom(env, "error")
	rsn := C.CString(reason)
	defer C.free(unsafe.Pointer(rsn))

	return C.enif_make_tuple2(env, err, C.enif_make_string(env, rsn, C.ERL_NIF_LATIN1))
}

func MakeString(env *C.ErlNifEnv, s string) C.ERL_NIF_TERM {
	cStr := C.CString(s)
	defer C.free(unsafe.Pointer(cStr))
	return C.enif_make_string(env, cStr, C.ERL_NIF_LATIN1)
}

func MakeBinary(env *C.ErlNifEnv, b []byte) C.ERL_NIF_TERM {
	cb := C.CBytes(b)
	defer C.free(unsafe.Pointer(cb))
	bin := C.ErlNifBinary{
		size: C.size_t(len(b)),
		data: (*C.uchar)(cb),
	}
	return C.enif_make_binary(env, &bin)
}

func MakeTuple(env *C.ErlNifEnv, items []interface{}) C.ERL_NIF_TERM {
	var elements []C.ERL_NIF_TERM
	for i := range items {
		elements = append(elements, MakeTerm(env, items[i]))
	}
	return C.enif_make_tuple_from_array(env, &elements[0], C.uint(len(elements)))
}

func MakeList(env *C.ErlNifEnv, items []interface{}) C.ERL_NIF_TERM {
	var elements []C.ERL_NIF_TERM
	for i := range items {
		elements = append(elements, MakeTerm(env, items[i]))
	}
	return C.enif_make_list_from_array(env, &elements[0], C.uint(len(elements)))
}
