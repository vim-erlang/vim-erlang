%%%-------------------------------------------------------------------
%% @doc mylib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mylib_sup).
-behaviour(supervisor).

-define(MY_MACRO_ERL, 1).

-record(my_rec_erl, {}).

-type my_rec_erl() :: #my_rec_erl{}.

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    f(?MY_MACRO_ERL),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
-spec f(_) -> Result when
      Result :: my_rec_erl().
f(x) ->
    #my_rec_erl{};
f(_) ->
    #my_rec_erl{}.
