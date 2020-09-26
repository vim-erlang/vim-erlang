%%%-------------------------------------------------------------------
%% @doc mylib public API
%% @end
%%%-------------------------------------------------------------------

-module(mylib_app).
-behaviour(application).

-include("mylib_app.hrl").

-define(MY_MACRO_ERL, 1).

-record(my_rec_erl, {}).

-type my_rec_erl() :: #my_rec_erl{}.

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f(?MY_MACRO_HRL),
    mylib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
-spec f(_) -> Result when
      Result :: my_rec_erl().
f(_) ->
    #my_rec_erl{}.
