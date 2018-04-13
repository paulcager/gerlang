package main

const ConvertStr = `
package main

import (
	"fmt"
	"reflect"
	"unsafe"
)

// #cgo CFLAGS: -I../nif/include
// #cgo LDFLAGS: -L../nif/lib -lerl_interface -lei -fpic -shared
//
// #include "erl_nif.h"
//
// static inline int print_erl(char *buff, size_t size, const ERL_NIF_TERM term) {
//     return enif_snprintf(buff, size, "%T", term);
// }
//
// // NB: do not attempt to define non-inline functions here: having exports means that
// // they would be defined twice.
import "C"

func Convert(env *C.ErlNifEnv, term C.ERL_NIF_TERM, value interface{}) error {
	val := reflect.ValueOf(value)
	if !val.Elem().CanSet() {
		return fmt.Errorf("Cannot translate %v because %v is not settable", term, val)
	}

	kind := val.Elem().Kind()
	switch kind {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if l, ok := getLong(env, term); ok {
			val.Elem().SetInt(int64(l))
		} else {
			return fmt.Errorf("Cannot translate (%s) - expected an int", SprintTerm(env, term))
		}
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		if l, ok := getLong(env, term); ok {
			val.Elem().SetUint(uint64(l))
		} else {
			return fmt.Errorf("Cannot translate (%s) - expected an int", SprintTerm(env, term))
		}
	case reflect.String:
		if s, ok := getString(env, term); ok {
			val.Elem().SetString(s)
		} else {
			return fmt.Errorf("Cannot translate (%s) - expected a string", SprintTerm(env, term))
		}
	case reflect.Bool:
		if b, ok := getBool(env, term); ok {
			val.Elem().SetBool(b)
		} else {
			return fmt.Errorf("Cannot translate (%s) - expected a bool", SprintTerm(env, term))
		}
	case reflect.Map:
		return ConvertMap(env, term, value)
	case reflect.Array, reflect.Slice:
		return ConvertSlice(env, term, value)
	case reflect.Struct, reflect.Ptr:
		panic("TODO - " + fmt.Sprintf("kind=%v, type=%T", kind, value))
		// And all the others - Channel, Func, ... ... ...
	case reflect.Interface:
		// Can store anything, so try most restrictive first.
		if l, ok := getLong(env, term); ok {
			val.Elem().Set(reflect.ValueOf(l))
			return nil
		}
		if s, ok := getString(env, term); ok {
			val.Elem().Set(reflect.ValueOf(s))
			return nil
		}
		var obj interface{}
		if err := ConvertSlice(env, term, &obj); err == nil {
			val.Elem().Set(reflect.ValueOf(obj))
			return nil
		}
		// TODO - and all the others
		panic("TODO - " + fmt.Sprintf("kind=%v, type=%T, term=%s", kind, value, term))
	default:
		fmt.Printf("Unexpected kind %q, %v", kind, val)
	}
	return nil
}

func ConvertSlice(env *C.ErlNifEnv, term C.ERL_NIF_TERM, value interface{}) error {
	// TODO - special case when expecting []byte: allow binaries and lists.

	v := reflect.ValueOf(value)
	fmt.Printf("value is type %T, value %v, v is %v\n", value, value, v.Type())
	var length C.unsigned
	if C.enif_get_list_length(env, term, &length) != 0 {
		var head, tail C.ERL_NIF_TERM
		// TODO - don't do for array.
		slice := reflect.MakeSlice(v.Elem().Type(), int(length), int(length))
		v.Elem().Set(slice)
		i := 0
		head = term
		for C.enif_get_list_cell(env, head, &head, &tail) != 0 {
			var item interface{}
			err := Convert(env, head, &item)
			if err != nil {
				return err
			}
			fmt.Printf("Setting index %d in slice of len %d to %v\n", i, int(length), item)
			slice.Index(i).Set(reflect.ValueOf(item))
			i++
			head = tail
		}

		return nil
	}

	var arity C.int
	var array *C.ERL_NIF_TERM
	if C.enif_get_tuple(env, term, &arity, &array) != 0 {
		// TODO - don't do for array.
		slice := reflect.MakeSlice(v.Elem().Type(), int(arity), int(arity))
		v.Elem().Set(slice)
		for i := 0; i < int(arity); i++ {
			var item interface{}
			err := Convert(env, *array, &item)
			if err != nil {
				return err
			}
			fmt.Printf("Setting index %d in tuple of len %d to %v\n", i, int(arity), item)
			slice.Index(i).Set(reflect.ValueOf(item))
			array = (*C.ERL_NIF_TERM)(unsafe.Pointer(uintptr(unsafe.Pointer(array)) + 8))
		}

		return nil
	}

	// TODO - try tuple.

	return fmt.Errorf("Cannot translate (%s) - expected a slice/array", SprintTerm(env, term))
}

func ConvertMap(env *C.ErlNifEnv, term C.ERL_NIF_TERM, value interface{}) error {
	// Can be represented as either a map (http://erlang.org/doc/man/maps.html),
	// or as a proplist (http://erlang.org/doc/man/proplists.html).

	var k, v C.ERL_NIF_TERM
	var iter C.ErlNifMapIterator

	if C.enif_map_iterator_create(env, term, &iter, C.ERL_NIF_MAP_ITERATOR_FIRST) != 0 {
		// Is a map
		defer C.enif_map_iterator_destroy(env, &iter)

		for C.enif_map_iterator_get_pair(env, &iter, &k, &v) != 0 {
			fmt.Println("k", k, "v", v)
			C.enif_map_iterator_next(env, &iter)
		}

		return nil
	}

	panic("TODO")
}

func SprintTerm(env *C.ErlNifEnv, term C.ERL_NIF_TERM) string {
	size := C.size_t(80)
	buff := C.malloc(size)
	count := C.print_erl((*C.char)(buff), size, term)
	if count >= C.int(size) {
		C.free(buff)
		size = C.size_t(count + 1)
		buff = C.malloc(size)
		C.print_erl((*C.char)(buff), size, term)
	}

	s := C.GoString((*C.char)(buff))
	C.free(buff)
	return s
}

func getLong(env *C.ErlNifEnv, term C.ERL_NIF_TERM) (int64, bool) {
	var cl C.long
	ok := C.enif_get_int64(env, term, &cl) != 0
	return int64(cl), ok
}

func getBool(env *C.ErlNifEnv, term C.ERL_NIF_TERM) (bool, bool) {
	var (
		bp  C.long
		str string
	)
	ok := C.enif_get_int64(env, term, &bp) != 0
	if ok {
		return bp != 0, ok
	}
	str, ok = getString(env, term)
	if ok && (str == "true" || str == "false") {
		return str == "true", ok
	}

	return false, false
}

func getString(env *C.ErlNifEnv, term C.ERL_NIF_TERM) (string, bool) {
	var bin C.ErlNifBinary
	if C.enif_inspect_binary(env, term, &bin) != 0 {
		size := C.int(bin.size)
		data := C.GoBytes(unsafe.Pointer(bin.data), size)
		return string(data), true
	}

	var length C.unsigned
	if C.enif_get_list_length(env, term, &length) != 0 {
		buf := C.malloc(C.size_t(length + 1))
		defer C.free(buf)
		if C.enif_get_string(env, term, (*C.char)(buf), length+1, C.ERL_NIF_LATIN1) != 0 {
			return C.GoString((*C.char)(buf)), true
		}
	}

	if C.enif_get_atom_length(env, term, &length, C.ERL_NIF_LATIN1) != 0 {
		buf := C.malloc(C.size_t(length + 1))
		defer C.free(buf)
		if C.enif_get_atom(env, term, (*C.char)(buf), length+1, C.ERL_NIF_LATIN1) != 0 {
			return C.GoString((*C.char)(buf)), true
		}
	}

	return "", false
}
`
