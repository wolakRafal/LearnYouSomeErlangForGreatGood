%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2015 17:02
%%%-------------------------------------------------------------------
-module(recursive).
-author("Rafal Wolak").

%% API
-export([fac/1, fac2/1, len/1, duplicate/2, len_tail/1, fac_tail/1, tail_duplicate/2, reverse/1, tail_reverse/1, sublist/2]).
fac(N) when N =< 1 -> 1;
fac(N) -> N * fac(N - 1).

%% shorter version
fac2(0) -> 1;
fac2(N) -> N * fac2(N - 1).

len([]) -> 0;
len([_ | Tail]) -> 1 + len(Tail).

%%% taile recursive versions:

fac_tail(N) -> fac_tail(N, 1).

fac_tail(0, Acc) -> Acc;
fac_tail(N, Acc) -> fac_tail(N - 1, N * Acc).


len_tail(List) -> len_tail(List, 0).

len_tail([], Acc) -> Acc;
len_tail([X | Tail], Acc) -> fac_tail(Tail, X * Acc).


duplicate(0, _) -> [];
duplicate(N, Term) -> [Term | duplicate(N - 1, Term)].

tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).

tail_duplicate(0, _, Acc) -> Acc;
tail_duplicate(N, Term, Acc) -> tail_duplicate(N, Term, [Term | Acc]).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

tail_reverse(L) -> tail_reverse(L,[]).
tail_reverse([], Acc) -> Acc;
tail_reverse([H | T], Acc) -> tail_reverse(T, [H | Acc]).

sublist(_,0) -> [];
sublist([],N) -> [];
sublist([H | T], N) -> [H|sublist(T,N-1)].

