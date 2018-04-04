#include "erl_nif.h"
#include "gerlang.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void call_go();

// Part 1 - Implement the NIF
// http://erlang.org/doc/tutorial/nif.html

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
doCall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    call_go();

    return mk_atom(env, "TODO - decode argv into Golang equivalent");

    // First load the target function using plugin + reflection/
    // Translate each arg to GoString etc.
}

static ErlNifFunc nif_funcs[] = {
    {"call", 3, doCall}
};

ERL_NIF_INIT(ergo, nif_funcs, NULL, NULL, NULL, NULL);


// Part 2 - Call Go from NIF
// https://golang.org/cmd/cgo/

static void
call_go() {
    // Call(pkg string, funcName string, args []interface{})
        GoString pkg = {"my.package", strlen("my.package")};
        GoString func = {"myFunc", strlen("myFunc")};
        GoSlice args = {0, 0, 0};
        Call(pkg, func, args);
}