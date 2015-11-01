%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Oct 2015 22:50
%%%-------------------------------------------------------------------
-module(ppool_supersup).
-behaviour(supervisor).
-author("Rafal Wolak").

%% API
-export([start_link/0, start_pool/3, stop_pool/1]).

%% callback implemenation of supervisor behaviour
-export([init/1]).

start_link() ->
  supervisor:start_link({local,ppool}, ?MODULE, []).

%% technically, a supervisor can not be killed in an easy way.
%% Let's do it brutally!
%%%%% UPDATE: OTP application tools will take care of that for us.
%% stop() ->
%%   case whereis(ppool) of
%%     P when is_pid(P) ->
%%       exit(P,kill);
%%     _ -> ok
%%   end.

%%% Limit - the number of workers the pool will accept,
%%% MFA - and the {M,F,A} tuple that the worker supervisor will need to start each worker.
start_pool(Name, Limit, MFA) ->
  ChildSpec = {Name,
                {ppool_sup, start_link, [Name, Limit, MFA]},
                permanent, 10500, supervisor, [ppool_sup]},
  supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool,Name).



init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime},[]}}.

