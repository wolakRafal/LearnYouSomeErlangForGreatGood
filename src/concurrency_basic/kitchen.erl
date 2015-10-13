%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 13:56
%%%-------------------------------------------------------------------
-module(kitchen).
-author("Rafal Wolak").

%% API
-compile(export_all).

fridge(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge([Food | FoodList]);

    {From, {get, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge(FoodList)
      end;
    terminate ->
      ok
  end.

%% hides starting the process
start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  end.