-module(mylib).

-include("../include/mylib.hrl").

-export([f/0]).

f() ->
    ?MY_LIB_MACRO.
