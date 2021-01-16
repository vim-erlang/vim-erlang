%% @spec This is an Edoc error.
-module(edoc_error_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok.

stop(_State) ->
    ok.
