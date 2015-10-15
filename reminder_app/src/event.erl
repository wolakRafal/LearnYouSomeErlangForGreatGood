%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2015 11:40
%%%-------------------------------------------------------------------
-module(event).
-author("Rafal Wolak").

%% API
-compile(export_all).
-record(state, {server, name = "", to_go = 0}).

%% Loop uses a list for times in order to go around the ~49 days limit
%% on timeouts.
loop(S = #state{server = Server, to_go = [T | Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
    if  Next =:= [] ->
          Server ! {done, S#state.name};
        Next =/= [] ->
          loop(S#state{to_go = Next})
    end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalized(N) ->
  Limit = 49*24*60*60,
  [N rem Limit| lists:duplicate(N div Limit, Limit)].

