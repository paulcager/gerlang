package main

const ErlHeaderStr = `-module(ergo).
-compile(export_all).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./ergo", 0).

`
