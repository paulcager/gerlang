#include "erl_nif.h"
#include "libgerlang.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static ERL_NIF_TERM call_go(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static GoInterface term_to_interface(ErlNifEnv* env, const ERL_NIF_TERM term);

// Part 1 - Implement the NIF
// http://erlang.org/doc/tutorial/nif.html
// http://erlang.org/doc/man/erl_nif.html

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* reason) {
    ERL_NIF_TERM ret = enif_make_tuple2(env,
        mk_atom(env, "error"),
        enif_make_string(env, reason, ERL_NIF_LATIN1));

    return ret;
}

static ERL_NIF_TERM
doCall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return call_go(env, argc, argv);
}

static ErlNifFunc nif_funcs[] = {
    {"call", 3, doCall}
};

ERL_NIF_INIT(ergo, nif_funcs, NULL, NULL, NULL, NULL);


// Part 2 - Call Go from NIF
// https://golang.org/cmd/cgo/
// https://stackoverflow.com/a/32217287/683825
// https://github.com/vladimirvivien/go-cshared-examples

static ERL_NIF_TERM
call_go(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Map from Erlang call(PluginPath, Name, Params) when is_list(Params)
    //       to GoLang func Call(pluginFile string, funcName string, args []interface{}) []interface{}

    if (argc != 3) {
        return mk_error(env, "Expected 3 args");
    }



    char path[1024];
    int len = enif_get_string(env, argv[0], path, sizeof path, ERL_NIF_LATIN1);
    if (len <= 0) {
        return mk_error(env, "PluginPath must be an ASCII string");
    }

    char funcName[1024];
    len = enif_get_string(env, argv[1], funcName, sizeof funcName, ERL_NIF_LATIN1);
    if (len <= 0) {
        return mk_error(env, "FuncName must be an ASCII string");
    }

    unsigned param_count;
    if (!enif_get_list_length(env, argv[2], &param_count)) {
        return mk_error(env, "Expected list of parameters");
    }

    GoInterface params[param_count];
    ERL_NIF_TERM p_list = argv[2];
    ERL_NIF_TERM item;
    int i = 0;
    while(enif_get_list_cell(env, p_list, &item, &p_list)) {
        params[i++] = term_to_interface(env, item);
        printf("Param %d\n", i);
    }

    // Call(pkg string, funcName string, args []interface{})
        GoString pluginPath = {path, strlen(path)};
        GoString func = {funcName, strlen(funcName)};
        GoSlice args = {0, 0, 0};
        Call(pluginPath, func, args);

    // TODO

    return mk_atom(env, "ok");
}

static GoInterface term_to_interface(ErlNifEnv* env, const ERL_NIF_TERM term) {
    // TODO
}