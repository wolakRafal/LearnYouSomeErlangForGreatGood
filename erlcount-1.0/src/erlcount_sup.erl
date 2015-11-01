%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 26. Oct 2015 19:25
%%%-------------------------------------------------------------------
-module(erlcount_sup).
-behaviour(supervisor).
-author("Rafal Wolak").

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init([]) ->
  MaxRestart = 5,
  MaxTime = 100,
  {ok, {{one_for_one, MaxRestart, MaxTime},
    [{dispatch,
      {erlcount_dispatch, start_link, []},
      transient,
      60000,
      worker,
      [erlcount_dispatch]}]}}.