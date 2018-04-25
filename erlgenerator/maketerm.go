package main

const MakeTermStr = `package main

// #include "erl_nif.h"
import "C"

import (
	"fmt"
	"reflect"
	"unsafe"
)

// MakeTerm creates an Erlang term corresponding to the Golang object passed in.
func MakeTerm(env *C.ErlNifEnv, value interface{}) C.ERL_NIF_TERM {
	return makeTerm(env, reflect.ValueOf(value))
}

func MakeTuple(env *C.ErlNifEnv, items []interface{}) C.ERL_NIF_TERM {
	var values []reflect.Value
	for i := range items {
		values = append(values, reflect.ValueOf(items[i]))
	}
	return makeTuple(env, values)
}

func makeTerm(env *C.ErlNifEnv, v reflect.Value) C.ERL_NIF_TERM {
	// First some special cases.
	if v.IsValid() && v.CanInterface() {
		if err, ok := v.Interface().(error); ok {
			return MakeError(env, err.Error())
		}

		if b, ok := v.Interface().([]byte); ok {
			return MakeBinary(env, b)
		}
	}

	// TODO - where to take care of nils? Prob best in each kind, since a nil map and a nil []byte
	// would be different things to erlang.

	switch kind := v.Kind(); kind {
	case reflect.Ptr:
		if v.IsNil() {
			return MakeAtom(env, "nil")
		}
		e := v.Elem()
		return makeTerm(env, e)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		l := v.Int()
		return C.enif_make_int64(env, C.long(l))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		l := v.Uint()
		return C.enif_make_uint64(env, C.ulong(l))
	case reflect.String:
		return MakeString(env, v.String())
	case reflect.Bool:
		if v.Bool() {
			return MakeAtom(env, "true")
		}
		return MakeAtom(env, "false")
	case reflect.Map:
		m := C.enif_make_new_map(env)
		keys := v.MapKeys()

		for _, key := range keys {
			newKey := makeTerm(env, key)
			newValue := makeTerm(env, v.MapIndex(key))
			if C.enif_make_map_put(env, m, newKey, newValue, &m) == 0 {
				return MakeError(env, "Could not enif_make_map_put")
			}
		}

		return m
	case reflect.Slice, reflect.Array:
		sLen := v.Len()
		elements := make([]reflect.Value, sLen)
		for i := 0; i < sLen; i++ {
			elements[i] = v.Index(i)
		}
		return MakeList(env, elements)
	case reflect.Struct:
		sLen := v.NumField()
		elements := make([]reflect.Value, sLen)
		for i := 0; i < sLen; i++ {
			elements[i] = v.Field(i)
		}
		return makeTuple(env, elements)
	case reflect.Interface:
		return makeTerm(env, reflect.ValueOf(v.Interface()))

		// And all the others - Channel, Func, ... ... ...

	default:
		fmt.Printf("Kind %q for type %T of value %#v\n", kind, v.Interface(), v)
		panic("!")
	}

	return 0
}

func MakeReturnValue(env *C.ErlNifEnv, results []interface{}) C.ERL_NIF_TERM {
	if len(results) == 0 {
		return MakeAtom(env, "ok")
	}

	if len(results) == 1 {
		return MakeTerm(env, results[0])
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

func makeTuple(env *C.ErlNifEnv, items []reflect.Value) C.ERL_NIF_TERM {
	//fmt.Printf("makeTuple: %v\n", items)
	var elements []C.ERL_NIF_TERM
	for i := range items {
		elements = append(elements, makeTerm(env, items[i]))
	}
	return C.enif_make_tuple_from_array(env, &elements[0], C.uint(len(elements)))
}

func MakeList(env *C.ErlNifEnv, items []reflect.Value) C.ERL_NIF_TERM {
	if len(items) == 0 {
		// NB: enif_make_list is declared as "...", which CGO can't handle.
		return C.enif_make_list_from_array(env, nil, 0)
	}

	var elements []C.ERL_NIF_TERM
	for i := range items {
		elements = append(elements, makeTerm(env, items[i]))
	}
	return C.enif_make_list_from_array(env, &elements[0], C.uint(len(elements)))
}
`
