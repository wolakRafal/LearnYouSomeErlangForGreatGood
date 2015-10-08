%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2015 19:37
%%%-------------------------------------------------------------------
-module(hhfuns).
-author("Rafal Wolak").

%% API
-compile(export_all).
%% find the maximum of a list
max([H | T]) -> max2(T, H).

max2([], M) -> M;
max2([H | T], M) when H > M -> max2(T, H);
max2([_ | T], M) -> max2(T, M).

%% sum of all the elements of a list
sum(L) -> sum(L, 0).
sum([], S) -> S;
sum([H | T], S) -> sum(T, S + H).

fold(_, Start, []) -> Start;
fold(Op, Start, [H | T]) -> fold(Op, Op(H, Start), T).

reverse(L) ->
  fold(fun(X, Acc) -> [X | Acc] end, [], L).

map2(F, L) ->
  reverse(fold(fun(X, Acc) -> [F(X) | Acc] end, [], L)).

filter(Pred, L) ->
  F = fun (X,Acc) -> case Pred(X) of
                  true -> [X|Acc];
                  false -> Acc
                end
  end,
  reverse(fold(F,[],L)).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

%%%% Anonymous functions
%% fun(Args1) ->
%%       Expression1, Exp2, ..., ExpN;
%%     (Args2) ->
%%       Expression1, Exp2, ..., ExpN;
%%     (Args3) ->
%%       Expression1, Exp2, ..., ExpN
%% end
%% Fn = fun() -> a end.

%%%% It is important to note that the inherited scope follows the anonymous function wherever it is,
%%%% even when it is passed to another function:

a() ->
  Secret = "pony",
  fun() -> Secret end.

b(F) ->
  "a/0's password is "++F().

%%%% TRAP:
%A little trap you might fall into when writing
%% anonymous functions is when you try to redefine the scope:


base_wrong() ->
  A = 1,
  (fun() -> A = 2 end)(). % Will fail

base_shadowing() ->
  A = 1,
  (fun(A) -> A = 2 end)(2).

%% only keep even numbers
even(L) -> lists:reverse(even(L, [])).
even([], Acc) -> Acc;
even([H | T], Acc) when H rem 2 == 0 -> even(T, [H | Acc]);
even([_ | T], Acc) -> even(T, Acc).

%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).

old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 -> old_men(People,[Person|Acc]);
old_men([_ |People], Acc) -> old_men(People,Acc).

filter2(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) -> Acc;
%% filter(Pred, [H | T], Acc) when Pred(H) -> filter(Pred, T, [H | Acc]);
%% filter(Pred, [_ | T], Acc) -> filter(Pred, T, Acc).
% using case
filter(Pred, [H | T], Acc) ->
  case Pred(H) of
    true -> filter(Pred, T, [H | Acc]);
    false -> filter(Pred, T, Acc)
  end.