%%%-------------------------------------------------------------------
%% @doc myapp public API
%% @end
%%%-------------------------------------------------------------------

-module(myapp_app).

% Test include_lib
-include_lib("mylib/include/mylib.hrl").

-behaviour(application).

-export([start/2, stop/1, f/0]).

start(_StartType, _StartArgs) ->
    myapp_sup:start_link().

stop(_State) ->
    ok.

f() ->
    ?MY_LIB_MACRO.
