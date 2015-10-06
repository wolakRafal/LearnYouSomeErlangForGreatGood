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
