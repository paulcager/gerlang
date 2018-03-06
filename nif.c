#include "erl_nif.h"
#include "gerlang.h"
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

    ERL_NIF_TERM p_list = argv[2];
    unsigned param_count;
    if (!enif_get_list_length(env, argv[2], &param_count)) {
        return mk_error(env, "Expected list of parameters");
    }
    ERL_NIF_TERM params[param_count];
    int i = 0;
    while(enif_get_list_cell(env, p_list, &(params[i]), &p_list)) {
        printf("Param %d\n", i);
        i++;
    }


    // Call(pkg string, funcName string, args []interface{})
    GoString pluginPath = {path, strlen(path)};
    GoString func = {funcName, strlen(funcName)};
    GoSlice args = {params, param_count, param_count};
    Call(env, pluginPath, func, args);

    // TODO

    return mk_atom(env, "ok");
}

static GoInterface term_to_interface(ErlNifEnv* env, const ERL_NIF_TERM term) {
    // TODO
}

int get_long(ErlNifEnv* env, ERL_NIF_TERM term, long* ip) {
//    printf("env is %lx, enif_get_int64 is %lx\n", env, enif_get_int64);
    return enif_get_int64(env, term, ip);
}

char *sprint_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    int size = 80;
    char *buff = malloc(size);
    int count = enif_snprintf(buff, size, "%T", term);
    if (count >= size) {
        free(buff);
        size = count + 1;
        buff = malloc(size);
        enif_snprintf(buff, size, "%T", term);
    }
    return buff;
}

