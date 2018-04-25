package main

const ConvertStr = `package main

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
	return convert(env, term, reflect.ValueOf(value))
}

func convert(env *C.ErlNifEnv, term C.ERL_NIF_TERM, val reflect.Value) error {
	//fmt.Printf("Convert %s into %v [%+v]\n", SprintTerm(env, term), val.Type(), val)
	//defer fmt.Printf("Converted to %v\n", val)

	kind := val.Elem().Kind()
	switch kind {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if l, ok := getLong(env, term); ok {
			val.Elem().SetInt(int64(l))
		} else {
			return fmt.Errorf("Cannot translate %#q - expected an int", SprintTerm(env, term))
		}
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		if l, ok := getULong(env, term); ok {
			val.Elem().SetUint(uint64(l))
		} else {
			return fmt.Errorf("Cannot translate %#q - expected a uint", SprintTerm(env, term))
		}
	case reflect.Float32, reflect.Float64:
		if d, ok := getDouble(env, term); ok {
			val.Elem().SetFloat(d)
		} else {
			return fmt.Errorf("Cannot translate %#q - expected a float", SprintTerm(env, term))
		}
	case reflect.String:
		if s, ok := getString(env, term); ok {
			val.Elem().SetString(s)
		} else {
			return fmt.Errorf("Cannot translate %#q - expected a string", SprintTerm(env, term))
		}
	case reflect.Bool:
		if b, ok := getBool(env, term); ok {
			val.Elem().SetBool(b)
		} else {
			return fmt.Errorf("Cannot translate %#q - expected a bool", SprintTerm(env, term))
		}
	case reflect.Map:
		return ConvertMap(env, term, val)
	case reflect.Array, reflect.Slice:
		return ConvertSlice(env, term, val)
	case reflect.Ptr:
		if atom, ok := getAtom(env, term); ok && atom == "nil" {
			return nil
		}
		newVal := reflect.New(val.Type().Elem().Elem())
		val.Elem().Set(newVal)
		return convert(env, term, val.Elem())
	case reflect.Struct:
		return ConvertStruct(env, term, val)
	case reflect.Interface:
		// TODO - but only interface{}, not io.Reader. Prob best checked near start
		// Can store anything, so try most restrictive first.
		if l, ok := getLong(env, term); ok {
			val.Elem().Set(reflect.ValueOf(l))
			return nil
		}

		if s, ok := getAtom(env, term); ok {
			switch s {
			case "true":
				val.Elem().Set(reflect.ValueOf(true))
			case "false":
				val.Elem().Set(reflect.ValueOf(false))
			default:
				val.Elem().Set(reflect.ValueOf(s))
			}
			return nil
		}

		if s, ok := getString(env, term); ok {
			val.Elem().Set(reflect.ValueOf(s))
			return nil
		}

		var obj interface{}
		objPtr := reflect.ValueOf(&obj)
		if err := ConvertSlice(env, term, objPtr); err == nil {
			val.Elem().Set(objPtr)
			return nil
		}

		switch {
		case C.enif_is_list(env, term) != 0, C.enif_is_tuple(env, term) != 0:
			var slice []interface{}
			err := convert(env, term, reflect.ValueOf(&slice))
			if err == nil {
				val.Elem().Set(reflect.ValueOf(slice))
			}
			return err
		}

		panic("TODO - " + fmt.Sprintf("kind=%v, type=%T, term=%s", kind, val.Type(), term))
		// TODO - And all the others - Channel, Func, ... ... ...
	default:
		fmt.Printf("Unexpected kind %q, %v\n", kind, val)
		panic(fmt.Sprintf("TODO kind=%v, type=%T, term=%s", kind, val.Type(), SprintTerm(env, term)))
	}
	return nil
}

func ConvertSlice(env *C.ErlNifEnv, term C.ERL_NIF_TERM, val reflect.Value) error {
	if val.Elem().Type().Kind() != reflect.Slice {
		return fmt.Errorf("Not a slice: %s", val.Elem().Type().String())
	}

	// Special case when expecting []byte: allow binaries.
	if val.Elem().Type().Elem().Kind() == reflect.Uint8 {
		var bin C.ErlNifBinary
		if C.enif_inspect_binary(env, term, &bin) != 0 {
			size := C.int(bin.size)
			data := C.GoBytes(unsafe.Pointer(bin.data), size)
			val.Elem().Set(reflect.ValueOf(data))
			return nil
		}
	}

	//fmt.Printf("value is type %T, value %val, val is %val\n", value, value, val.Type())
	var length C.unsigned
	if C.enif_get_list_length(env, term, &length) != 0 {
		var head, tail C.ERL_NIF_TERM
		// TODO - don't do for array.
		slice := reflect.MakeSlice(val.Elem().Type(), int(length), int(length))
		val.Elem().Set(slice)
		i := 0
		head = term
		for C.enif_get_list_cell(env, head, &head, &tail) != 0 {
			var item interface{}
			err := convert(env, head, reflect.ValueOf(&item))
			if err != nil {
				return err
			}
			slice.Index(i).Set(reflect.ValueOf(item).Convert(slice.Index(i).Type()))
			i++
			head = tail
		}

		return nil
	}

	var arity C.int
	var array *C.ERL_NIF_TERM
	if C.enif_get_tuple(env, term, &arity, &array) != 0 {
		// TODO - don't do for array.
		slice := reflect.MakeSlice(val.Elem().Type(), int(arity), int(arity))
		val.Elem().Set(slice)
		for i := 0; i < int(arity); i++ {
			var item interface{}
			err := convert(env, *array, reflect.ValueOf(&item))
			if err != nil {
				return err
			}
			slice.Index(i).Set(reflect.ValueOf(item))
			array = (*C.ERL_NIF_TERM)(unsafe.Pointer(uintptr(unsafe.Pointer(array)) + 8))
		}

		return nil
	}

	return fmt.Errorf("Cannot translate (%s) - expected a slice/array", SprintTerm(env, term))
}

func ConvertMap(env *C.ErlNifEnv, term C.ERL_NIF_TERM, val reflect.Value) error {
	// Can be represented as either a map (http://erlang.org/doc/man/maps.html),
	// or as a proplist (http://erlang.org/doc/man/proplists.html).

	mp := reflect.MakeMap(val.Elem().Type())
	val.Elem().Set(mp)

	var iter C.ErlNifMapIterator
	if C.enif_map_iterator_create(env, term, &iter, C.ERL_NIF_MAP_ITERATOR_FIRST) != 0 {
		// Is a map
		defer C.enif_map_iterator_destroy(env, &iter)

		var kTerm, vTerm C.ERL_NIF_TERM
		for C.enif_map_iterator_get_pair(env, &iter, &kTerm, &vTerm) != 0 {
			newKey := reflect.New(mp.Type().Key())
			newValue := reflect.New(mp.Type().Elem())
			if err := convert(env, kTerm, newKey); err != nil {
				return err
			}
			if err := convert(env, vTerm, newValue); err != nil {
				return err
			}
			mp.SetMapIndex(newKey.Elem(), newValue.Elem())
			C.enif_map_iterator_next(env, &iter)
		}

		return nil
	}

	if C.enif_is_list(env, term) != 0 {
		var head C.ERL_NIF_TERM
		for C.enif_get_list_cell(env, term, &head, &term) != 0 {
			var (
				tuple   = head
				atomLen C.unsigned
				arity   C.int
				array   *C.ERL_NIF_TERM
			)
			if C.enif_get_atom_length(env, head, &atomLen, C.ERL_NIF_LATIN1) != 0 {
				tuple = C.enif_make_tuple2(env, tuple, MakeAtom(env, "true"))
			}
			if C.enif_get_tuple(env, tuple, &arity, &array) != 0 && arity == 2 {
				newKey := reflect.New(mp.Type().Key())
				newValue := reflect.New(mp.Type().Elem())
				if err := convert(env, *array, newKey); err != nil {
					return err
				}
				if err := convert(env, *(*C.ERL_NIF_TERM)(unsafe.Pointer(uintptr(unsafe.Pointer(array)) + 8)), newValue); err != nil {
					return err
				}
				mp.SetMapIndex(newKey.Elem(), newValue.Elem())
			}
		}

		return nil
	}

	return fmt.Errorf("Cannot translate %#q - expected a map / proplist", SprintTerm(env, term))
}

func ConvertStruct(env *C.ErlNifEnv, term C.ERL_NIF_TERM, val reflect.Value) error {
	//fmt.Printf("ConvertStruct %s into %v [%+v]\n", SprintTerm(env, term), val, val.Type())
	// Accept a Map<FieldName>FieldValue; a tuple; or a list
	var iter C.ErlNifMapIterator
	if C.enif_map_iterator_create(env, term, &iter, C.ERL_NIF_MAP_ITERATOR_FIRST) != 0 {
		defer C.enif_map_iterator_destroy(env, &iter)
		var kTerm, vTerm C.ERL_NIF_TERM
		for C.enif_map_iterator_get_pair(env, &iter, &kTerm, &vTerm) != 0 {
			var fieldName string
			if err := Convert(env, kTerm, &fieldName); err != nil {
				return err
			}
			field := val.Elem().FieldByName(fieldName)
			//fmt.Printf("field: %T %val\n", field, field.Interface())
			if !field.IsValid() {
				// Could just ignore, but more likely it is an error.
				return fmt.Errorf("No field named %q", fieldName)
			}
			fieldValue := reflect.New(field.Type())
			//fmt.Printf("fieldValue: %T %val\n", fieldValue.Interface(), fieldValue.Interface())
			if err := convert(env, vTerm, fieldValue); err != nil {
				return err
			}
			field.Set(fieldValue.Elem())
			C.enif_map_iterator_next(env, &iter)
		}

		return nil
	}

	var listLen C.unsigned
	var list []C.ERL_NIF_TERM
	if C.enif_get_list_length(env, term, &listLen) != 0 {
		var head, tail C.ERL_NIF_TERM
		head = term
		for C.enif_get_list_cell(env, head, &head, &tail) != 0 {
			list = append(list, head)
			head = tail
		}
	} else {
		var arity C.int
		var array *C.ERL_NIF_TERM
		if C.enif_get_tuple(env, term, &arity, &array) != 0 {
			for i := 0; i < int(arity); i++ {
				list = append(list, *array)
				array = (*C.ERL_NIF_TERM)(unsafe.Pointer(uintptr(unsafe.Pointer(array)) + 8))
			}
		} else {
			return fmt.Errorf("Expected map, tuple or list to decode into struct. Got %s", SprintTerm(env, term))
		}
	}

	for i := range list {
		field := reflect.New(val.Elem().Field(i).Type())
		if err := convert(env, list[i], field); err != nil {
			return err
		}
		ptr := reflect.NewAt(val.Elem().Field(i).Type(), unsafe.Pointer(val.Elem().Field(i).Addr().Pointer()))
		ptr.Elem().Set(field.Elem())
		//val.Elem().Field(i).Set(field.Elem())
	}

	return nil
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

func getULong(env *C.ErlNifEnv, term C.ERL_NIF_TERM) (uint64, bool) {
	var cl C.ulong
	ok := C.enif_get_uint64(env, term, &cl) != 0
	return uint64(cl), ok
}

func getDouble(env *C.ErlNifEnv, term C.ERL_NIF_TERM) (float64, bool) {
	var d C.double
	ok := C.enif_get_double(env, term, &d) != 0
	if ok {
		return float64(d), ok
	}
	l, ok := getLong(env, term)
	return float64(l), ok
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

	return getAtom(env, term)
}

func getAtom(env *C.ErlNifEnv, term C.ERL_NIF_TERM) (string, bool) {
	var length C.unsigned
	if C.enif_get_atom_length(env, term, &length, C.ERL_NIF_LATIN1) == 0 {
		return "", false
	}
	buf := C.malloc(C.size_t(length + 1))
	defer C.free(buf)
	if C.enif_get_atom(env, term, (*C.char)(buf), length+1, C.ERL_NIF_LATIN1) == 0 {
		return "", false
	}
	return C.GoString((*C.char)(buf)), true
}
`
