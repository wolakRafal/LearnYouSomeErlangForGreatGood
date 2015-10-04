%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2015 14:54
%%%-------------------------------------------------------------------
-module(in_case_of).
-author("Rafal Wolak").

%% API
-export([insert/2, beach/1]).

insert(X, []) ->
  [X];
insert(X, Set) ->
  case lists:member(X,Set) of
    true ->   Set;
    false ->  [X | Set]
  end.

%% In this case, the pattern matching was really simple.
%% It can get more complex see this code:
beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
      'i like it';
    {kelvin, N} when N >= 293, N =< 318 ->
      'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 ->
      'favorable in the US';
    _ -> 'avoid beach'
  end.