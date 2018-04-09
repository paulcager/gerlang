-module(maketerm_test).
-compile(export_all).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("./r", 0).


