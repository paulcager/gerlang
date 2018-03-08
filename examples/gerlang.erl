-module(gerlang).
-export([
	call/3
]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("../gerlang", 0).


call(_Mod, _Fun, Params) when is_list(Params) ->
    exit("No NIF").
