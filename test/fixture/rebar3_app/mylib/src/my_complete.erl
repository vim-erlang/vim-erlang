%%------------------------------------------------------------------------------
%% @doc This module contains test cases for the erlang_complete.erl script in
%% vim-erlang-complete.
%% @end
%%------------------------------------------------------------------------------

-module(my_complete).
-export([
         % No type specs
         complete_a1/0,
         complete_a2/1,

         % Normal type specs
         complete_b1/0,
         complete_b2/1,
         complete_b3/3,
         complete_b4/6,
         complete_c1/1,
         complete_c2/3,
         complete_c3/6,
         complete_d1/0,
         complete_d2/1,
         complete_d3/3,
         complete_d4/6,

         % Plain Edoc
         complete_e1/1,
         complete_e2/1,
         complete_e3/1,
         complete_e4/1,

         % Edoc type specs
         complete_f1/0,
         complete_f2/1,
         complete_f3/3,
         complete_f4/6,
         complete_g1/1,
         complete_g2/3,
         complete_g3/6,
         complete_h1/0,
         complete_h2/1,
         complete_h3/3,
         complete_h4/6,

         % Normal type specs with patterns
         complete_i1/0,
         complete_i2/1,
         complete_i3/3,
         complete_i4/6,

         % Normal type specs when function arguments are patterns
         complete_j1/1,
         complete_j2/3]).

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions don't have type specs
%%%-----------------------------------------------------------------------------

% Test that if there is no type spec, only the arity is used.
%
% erlang_complete returns:
%
%     complete_a1/0

complete_a1() ->
    ok.

% Test that if there is no type spec, only the arity is used.
%
% erlang_complete returns:
%
%     complete_a2/1

complete_a2(_Arg) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have type specs, and
%%% - type specs don't contain variable names
%%%-----------------------------------------------------------------------------

% Test that if the return type does not have a variable name, then the variable
% type is returned.
%
% erlang_complete returns:
%
%     complete_b1() -> integer()

-spec complete_b1() -> integer().
complete_b1() ->
    ok.

% Test that if an argument does not have a variable name in the type spec, then
% the variable name in the first clause of the function is returned.
%
% erlang_complete returns:
%
%     complete_b2(MyFloat) -> integer()

-spec complete_b2(float()) -> integer().
complete_b2(MyFloat) when is_float(MyFloat) ->
    MyFloat;
complete_b2(MyOtherFloat) ->
    MyOtherFloat.

% Test that underscores are removed from variable names.
%
% erlang_complete returns:
%
%     complete_b3(MyFloat, MyInteger, MyTerm) -> integer()

-spec complete_b3(float(), integer(), term()) -> integer().
complete_b3(_MyFloat, __MyInteger, ___MyTerm) ->
    ok.

% Test that variables are replaced with X<index> in case of a collision.
%
% erlang_complete returns:
%
%     complete_b4(MyArg, X2, X3, X4, X5, Last) -> integer()

-spec complete_b4(term(), term(), term(), term(), term(), term()) -> integer().
complete_b4(MyArg, MyArg, _MyArg, __MyArg, ___MyArg, _Last) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have type specs, and
%%% - type specs contain variable names
%%% - variable names in type specs use inline notation
%%%-----------------------------------------------------------------------------

% Test that if an argument has a name in the type spec, then that name is
% returned. This does not work for the return type.
%
% erlang_complete returns:
%
%     complete_c1(MyFloat) -> integer()

-spec complete_c1(MyFloat :: float()) -> Result :: integer().
complete_c1(_Arg) ->
    ok.

% Test that underscores are not removed from argument names.
%
% erlang_complete returns:
%
%     complete_c2(_MyFloat, _MyFloat, __MyTerm) -> integer()

-spec complete_c2(_MyFloat :: float(),
                  _MyFloat :: float(),
                  __MyTerm :: term()) -> _Result :: integer().
complete_c2(_Arg1, _Arg2, _Arg3) ->
    ok.

% Test that argument names are left intact in case of a collision.
%
% erlang_complete returns:
%
%     complete_c3(MyArg, MyArg, _MyArg, __MyArg, __MyArg, Last) -> integer()

-spec complete_c3(MyArg :: term(),
                  MyArg :: term(),
                  _MyArg :: term(),
                  __MyArg :: term(),
                  __MyArg :: term(),
                  Last :: term()) -> MyArg :: integer().
complete_c3(_Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have type specs, and
%%% - type specs contain variable names
%%% - variable names in type specs use 'when' notation
%%%-----------------------------------------------------------------------------

% Test that if the return type has a name in the type spec, then that name is
% returned.
%
% erlang_complete returns:
%
%     complete_d1() -> Result

-spec complete_d1() -> Result when
      Result :: integer().
complete_d1() ->
    ok.

% Test that if an argument has a name in the type spec, then that name is
% returned.
%
% erlang_complete returns:
%
%     complete_d2(MyFloat) -> Result

-spec complete_d2(MyFloat) -> Result when
      MyFloat :: float(),
      Result :: integer().
complete_d2(_Arg) ->
    ok.

% Test that underscores are not removed from argument names.
%
% erlang_complete returns:
%
%     complete_d3(_MyFloat, _MyFloat, __MyTerm) -> Result

-spec complete_d3(_MyFloat, _MyFloat, __MyTerm) -> Result when
      _MyFloat :: float(),
      _MyFloat :: float(),
      __MyTerm :: term(),
      Result :: integer().
complete_d3(_Arg1, _Arg2, _Arg3) ->
    ok.

% Test that argument names are left intact in case of a collision.
%
% erlang_complete returns:
%
%     complete_d4(MyArg, MyArg, _MyArg, __MyArg, __MyArg, Last) -> Result

-spec complete_d4(MyArg, MyArg, _MyArg, __MyArg, __MyArg, Last) -> Result when
      MyArg :: term(),
      MyArg :: term(),
      _MyArg :: term(),
      __MyArg :: term(),
      __MyArg :: term(),
      Last :: term(),
      Result :: integer().
complete_d4(_Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have Edoc docstrings
%%% - the Edoc docstrings don't contain type specs
%%%-----------------------------------------------------------------------------

% Test that the Edoc docstring does not affect the result.
%
% erlang_complete returns:
%
%     complete_e1/1

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @end
%%------------------------------------------------------------------------------
complete_e1(_Arg) ->
    ok.

% Test that the Edoc docstring does not affect the result.
%
% erlang_complete returns:
%
%     complete_e2(MyFloat) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @end
%%------------------------------------------------------------------------------
-spec complete_e2(float()) -> integer().
complete_e2(MyFloat) when is_float(MyFloat) ->
    MyFloat.

% Test that the Edoc docstring does not affect the result.
%
% erlang_complete returns:
%
%     complete_e3(MyFloat) -> integer()

-spec complete_e3(MyFloat :: float()) -> integer().
complete_e3(_Arg) ->
    ok.

% Test that the Edoc docstring does not affect the result.
%
% erlang_complete returns:
%
%     complete_e4(MyFloat) -> Result

-spec complete_e4(MyFloat) -> Result when
      MyFloat :: float(),
      Result :: integer().
complete_e4(_Arg) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have **Edoc** type specs, and
%%% - type specs don't contain variable names
%%%-----------------------------------------------------------------------------

% Test that if the return type does not have a variable name, then the variable
% type is returned.
%
% erlang_complete returns:
%
%     complete_f1() -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_f1() -> integer()
%% @end
%%------------------------------------------------------------------------------
complete_f1() ->
    ok.

% Test that if an argument does not have a variable name in the type spec, then
% the variable name in the first clause of the function is returned.
%
% erlang_complete returns:
%
%     complete_f2(MyFloat) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_f2(float()) -> integer()
%% @end
%%------------------------------------------------------------------------------
complete_f2(MyFloat) when is_float(MyFloat) ->
    MyFloat;
complete_f2(MyOtherFloat) ->
    MyOtherFloat.

% Test that underscores are removed from variable names.
%
% erlang_complete returns:
%
%     complete_f3(MyFloat, MyInteger, MyTerm) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_f3(float(), integer(), term()) -> integer()
%% @end
%%------------------------------------------------------------------------------
complete_f3(_MyFloat, __MyInteger, ___MyTerm) ->
    ok.

% Test that variables are replaced with X<index> in case of a collision.
%
% erlang_complete returns:
%
%     complete_f4(MyArg, X2, X3, X4, X5, Last) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_f4(term(), term(), term(), term(), term(),
%%                   term()) -> integer()
%% @end
%%------------------------------------------------------------------------------
complete_f4(MyArg, MyArg, _MyArg, __MyArg, ___MyArg, _Last) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have **Edoc** type specs, and
%%% - type specs contain variable names
%%% - variable names in type specs use inline notation
%%%-----------------------------------------------------------------------------

% Test that if an argument has a name in the type spec, then that name is
% returned. This does not work for the return type.
%
% erlang_complete returns:
%
%     complete_g1(MyFloat) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_g1(MyFloat :: float()) -> Result :: integer()
%% @end
%%------------------------------------------------------------------------------
complete_g1(_Arg) ->
    ok.

% Test that underscores are not removed from argument names.
%
% erlang_complete returns:
%
%     complete_g2(_MyFloat, _MyFloat, __MyTerm) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_g2(_MyFloat :: float(),
%%                   _MyFloat :: float(),
%%                   __MyTerm :: term()) -> _Result :: integer()
%% @end
%%------------------------------------------------------------------------------
complete_g2(_Arg1, _Arg2, _Arg3) ->
    ok.

% Test that argument names are left intact in case of a collision.
%
% erlang_complete returns:
%
%     complete_g3(MyArg, MyArg, _MyArg, __MyArg, __MyArg, Last) -> integer()

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_g3(MyArg :: term(),
%%                   MyArg :: term(),
%%                   _MyArg :: term(),
%%                   __MyArg :: term(),
%%                   __MyArg :: term(),
%%                   Last :: term()) -> MyArg :: integer()
%% @end
%%------------------------------------------------------------------------------
complete_g3(_Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have **Edoc** type specs, and
%%% - type specs contain variable names
%%% - variable names in type specs use 'when' notation
%%%-----------------------------------------------------------------------------

% Test that if the return type has a name in the type spec, then that name is
% returned.
%
% erlang_complete returns:
%
%     complete_h1() -> Result

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_h1() -> Result
%%       Result = integer()
%% @end
%%------------------------------------------------------------------------------
complete_h1() ->
    ok.

% Test that if an argument has a name in the type spec, then that name is taken
% from the the function's argument list.
%
% Compare this with the complete_d2 test case, which shows that if this were a
% normal type spec (not an Edoc type spec), then the name would be taken from
% the type spec.
%
% erlang_complete returns:
%
%     complete_h2(Arg) -> Result

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_h2(MyFloat) -> Result
%%       MyFloat = float()
%%       Result = integer()
%% @end
%%------------------------------------------------------------------------------
complete_h2(_Arg) ->
    ok.

% Test that underscores are not removed from argument names.
%
% erlang_complete returns:
%
%     complete_h3(Arg1, Arg2, Arg3) -> Result

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_h3(_MyFloat, _MyFloat, __MyTerm) -> Result
%%       _MyFloat = float()
%%       _MyFloat = float()
%%       __MyTerm = term()
%%       Result = integer()
%% @end
%%------------------------------------------------------------------------------
complete_h3(_Arg1, _Arg2, _Arg3) ->
    ok.

% Test that argument names are left intact in case of a collision.
%
% erlang_complete returns:
%
%     complete_h4(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) -> Result

%%------------------------------------------------------------------------------
%% @doc Edoc docstring.
%% @spec complete_h4(MyArg, MyArg, _MyArg, __MyArg, __MyArg, Last) -> Result
%%       MyArg = term()
%%       MyArg = term()
%%       _MyArg = term()
%%       __MyArg = term()
%%       __MyArg = term()
%%       Last = term()
%%       Result = integer()
%% @end
%%------------------------------------------------------------------------------
complete_h4(_Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have type specs, and
%%% - type specs don't contain variable names
%%% - type specs contain patterns
%%%-----------------------------------------------------------------------------

% Test that if the return type does not have a variable name, then the variable
% type is returned.
%
% erlang_complete returns:
%
%     complete_i1() -> {integer(), integer()}

-spec complete_i1() -> {integer(), integer()}.
complete_i1() ->
    ok.

% Test that if an argument does not have a variable name in the type spec, then
% the variable name in the first clause of the function is returned.
%
% erlang_complete returns:
%
%     complete_i2(MyFloat) -> {integer(), integer()}

-spec complete_i2({float(), float()}) -> {integer(), integer()}.
complete_i2(MyFloat) when is_float(MyFloat) ->
    MyFloat;
complete_i2(MyOtherFloat) ->
    MyOtherFloat.

% Test that underscores are removed from variable names.
%
% erlang_complete returns:
%
%     complete_i3(MyFloat, MyInteger, MyTerm) -> {integer(), integer()}

-spec complete_i3({float(), float()},
                  {integer(), integer()},
                  {term(), term()}) -> {integer(), integer()}.
complete_i3(_MyFloat, __MyInteger, ___MyTerm) ->
    ok.

% Test that variables are replaced with X<index> in case of a collision.
%
% erlang_complete returns:
%
%     complete_i4(MyArg, X2, X3, X4, X5, Last) -> {integer(), integer()}

-spec complete_i4({term(), term()},
                  {term(), term()},
                  {term(), term()},
                  {term(), term()},
                  {term(), term()},
                  {term(), term()}) -> {integer(), integer()}.
complete_i4(MyArg, MyArg, _MyArg, __MyArg, ___MyArg, _Last) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test cases where:
%%%
%%% - functions have type specs, and
%%% - type specs don't contain variable names
%%% - arguments are patterns
%%%-----------------------------------------------------------------------------

% Test that if an argument does not have a variable name in the type spec, then
% the variable name in the first such clause of the function is returned, where
% the corresponding argument is not a pattern.
%
% erlang_complete returns:
%
%     complete_j1(MyOtherFloat) -> integer()

-spec complete_j1(float()) -> integer().
complete_j1({MyFloat}) when is_float(MyFloat) ->
    MyFloat;
complete_j1(MyOtherFloat) ->
    MyOtherFloat.

% Test that underscores are removed from variable names.
%
% erlang_complete returns:
%
%     complete_j2(X1, X2, X3) -> integer()

-spec complete_j2(float(), list(), map()) -> integer().
complete_j2({_MyFloat}, [_ListItem], #{key := _Value}) ->
    ok.
