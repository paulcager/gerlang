#include "erl_nif.h"
#include "runtime.h"

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
