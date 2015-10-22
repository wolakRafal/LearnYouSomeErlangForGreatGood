%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Oct 2015 23:14
%%%-------------------------------------------------------------------
-module(ppool_sup).
-author("Rafal Wolak").

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the pool supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Name,Limit,MFA) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name,Limit,MFA) ->
  supervisor:start_link(?MODULE, {Name,Limit,MFA}).
%%   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init({Name,Limit,MFA}) ->
  MaxRestart = 1,
  MaxTime = 3600,
  SupFlags = {one_for_all,MaxRestart,MaxTime},
  AChild = {serv, {ppool_serv, start_link, [Name,Limit, self(), MFA]},
            permanent,
            5000, %Shutdown time
            worker,
            [ppool_serv]},
  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
