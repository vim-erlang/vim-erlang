-module(compilation_error_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    compilation_error_sup:start_link().

stop(_State) ->
    ok.
