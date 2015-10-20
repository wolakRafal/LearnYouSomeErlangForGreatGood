%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%% So this is a kitty server/store.
%%%  The behavior is extremely simple:
%%     you describe a cat and you get that cat. If someone returns a cat,
%%     it's added to a list and is then automatically sent as the
%%     next order instead of what the client actually asked for
%%     (we're in this kitty store for the money, not smiles)
%%% @end
%%% Created : 19. Oct 2015 18:53
%%%-------------------------------------------------------------------

%%%%% Naive version
-module(kitty_server).
-author("Rafal Wolak").

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).
%% client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  Ref = erlang:monitor(process,Pid),
  Pid ! {self(), Ref, {order, Name, Color, Description}},
  receive
    {Ref, Cat} ->
      erlang:demonitor(Ref, [flush]),
      Cat;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 -> %% in millis
    erlang:error(timeout)
  end.

%% this call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
  Pid ! {return, Cat},
  ok.

%% Synchronous call
close_shop(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, terminate},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref),
      ok;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 -> %% 5 sec
    erlang:error(timeout)
  end.

%%% Server functions
init() -> loop([]).

loop(Cats) ->
  receive
    {Pid, Ref, {order, Name, Color, Description}} ->
      if Cats =:= [] ->
        Pid ! {Ref, make_cat(Name, Color, Description)},
        loop(Cats);
        Cats =/= [] -> % got to empty the stock
          Pid ! {Ref, hd(Cats)},
          loop(tl(Cats))
      end;
    {return, Cat = #cat{}} ->
      loop([Cat|Cats]);
    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok},
      terminate(Cats);
    Unknown ->
      %% do some logging here too
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(Cats)
  end.

%%% Private functions

make_cat(Name, Col, Desc) ->
  #cat{name = Name, color = Col, description = Desc}.

terminate(Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  ok.