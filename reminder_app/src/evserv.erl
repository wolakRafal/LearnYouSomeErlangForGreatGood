%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2015 15:44
%%%-------------------------------------------------------------------
-module(evserv).
-author("Rafal Wolak").

%% API
-compile(export_all).

-record(state, {events,     %% list of #event{} records
                clients}).  %% list of Pids
-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

init() ->
  %% Loading events from a static file could be done here.
  %% You would need to pass an argument to init telling where the
  %% resource to find the events is. Then load it from here.
  %% Another option is to just pass the events straight to the server
  %% through this function.
  loop(#state{events=orddict:new(),
    clients=orddict:new()}).

loop(S = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients = NewClients});

    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime() of
        true ->
          EventPid = event:start_link(Name,TimeOut),
          NewEvents = orddict:store(Name,
                                      #event{name=Name,
                                             description = Description,
                                             pid = EventPid,
                                            timeout = TimeOut})
      end;

    {Pid, MsgRef, {cancel, Name}} ->

    {done, Name} ->

    shutdown ->

    {'DOWN', Ref, process, _Pid, _Reason} ->

    code_change ->

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]) ->
  end.

valid_datetime({Date,Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
      false
  end;
valid_datetime(_) ->
  true.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M <60,
                      S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

