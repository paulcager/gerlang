#include "erl_nif.h"
//#include "erl_nif_api_funcs.h"

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
doCall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return mk_atom(env, "ok!!");
}

static ErlNifFunc nif_funcs[] = {
    {"call", 3, doCall}
};

ERL_NIF_INIT(gerlang, nif_funcs, NULL, NULL, NULL, NULL);
