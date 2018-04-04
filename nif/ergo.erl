-module(ergo).
-export([
	call/3
]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./ergo", 0).


call(_ModPath, _Name, Params) when is_list(Params) ->
    exit("No NIF").
