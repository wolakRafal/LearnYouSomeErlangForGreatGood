%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 19. paŸ 2015 18:42
%%%-------------------------------------------------------------------
-module(sup).
-author("Rafal Wolak").

%% API
-export([init/1, start_link/2, start/2]).

start(Mod, Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mode, Args}) ->
  process_flag(trap_exit, true),
  loop({Mode, start_link, Args)}).

loop(In = {M, F, A}) ->
  Pid = apply(M,F,A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown); % will kill the child too
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p ~n", [Pid,Reason]),
      loop(In)
  end.