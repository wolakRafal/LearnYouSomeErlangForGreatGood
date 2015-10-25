%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2015 19:40
%%%-------------------------------------------------------------------
-module(ppool_serv).
-author("Rafal Wolak").

-behaviour(gen_server).

%% API
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
%% The friendly supervisor is started dynamically!
-define(SPEC(MFA),
  {worker_sup,
    {ppool_worker_sup, start_link, [MFA]},
    temporary,
    10000,
    supervisor,
    [ppool_worker_sup]}).

%% We know we will have to track a few pieces of data:
% the number of process that can be running,
%  the pid of the supervisor and a queue for all the jobs.
% To know when a worker's done running and to fetch one from the queue to
% start it, we will need to track each worker from the server.
% The sane way to do this is with monitors, so we'll also add a
% refs field to our state record to keep all the monitor references in memory
-record(state, {limit=0,
                sup,
                refs,
                queue=queue:new()}).

%%%===================================================================
%%% API
%%%===================================================================
start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
  gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
  gen_server:cast(Name, {async, Args}).

stop(Name) ->
  gen_server:call(Name, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Limit, MFA, Sup}) ->
  %% We need to find the Pid of the worker supervisor from here,
  %% but alas, this would be calling the supervisor while it waits for us!
  self() ! {start_worker_supervisor, Sup, MFA},
  {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
  {reply, noalloc, S};
%% the calls to sync_queue/2
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({sync, Args},  From, S = #state{queue=Q}) ->
  {noreply, S#state{queue=queue:in({From, Args}, Q)}};
%% stop and unknown messages
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 ->
  {noreply, S#state{queue=queue:in(Args,Q)}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
% When do we know it's time to dequeue something?
% Well, we have monitors set all around the place
% and we're storing their references in a gb_sets.
% Whenever a worker goes down, we're notified of it. Let's work from there:
handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
  io:format("received down msg~n"),
  case gb_sets:is_element(Ref, Refs) of
    true ->
      handle_down_worker(Ref, S);
    false -> %% Not our responsibility
      {noreply, S}
  end;

handle_info({start_worker_supervisor, Sup, MFA}, State=#state{}) ->
  {ok, Pid} = supervisor:start_child(Sup,?SPEC(MFA)),
  link(Pid),
  {noreply, State#state{sup = Pid}};
handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
  case queue:out(S#state.queue) of
    {{value, {From, Args}}, Q} ->
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
      gen_server:reply(From, {ok, Pid}),
      {noreply, S#state{refs=NewRefs, queue=Q}};
    {{value, Args}, Q} ->
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
      {noreply, S#state{refs=NewRefs, queue=Q}};
    {empty, _} ->
      {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref,Refs)}}
  end.