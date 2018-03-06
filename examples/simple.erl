-module(simple).
-export([
  simple_call/0
]).


simple_call() ->
  gerlang:call('strings', 'Title', ["her royal highness"]).
