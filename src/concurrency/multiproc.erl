%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 20:11
%%%-------------------------------------------------------------------
-module(multiproc).
-author("RafalW").

%% API
-compile(export_all).

%% sleep/1 implementation
sleep(T) ->
  receive
    after T -> ok
  end.

%% Selective Receives
important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
    after 0 ->
      normal()
  end.

normal() ->
  receive
    {_,Message} ->
      [Message | normal()]
    after 0 ->
      []
  end.

%% optimized in R14A
optimized(Pid) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, hello},
  receive
    {Pid, Ref, Msg} ->
      io:format("~p~n", [Msg])
  end.
