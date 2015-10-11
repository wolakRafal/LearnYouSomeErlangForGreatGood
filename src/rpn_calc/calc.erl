%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2015 19:05
%%%-------------------------------------------------------------------
-module(calc).
-author("Rafal Wolak").

%% API
-export([rpn/1, rpn_test/0]).

%% rpn(List()) -> Int() | Float()
%% parses an RPN string and outputs the results.
rpn([]) -> 0;
rpn(L) -> rpn_stack(string:tokens(L, " "), []).


rpn_stack([], [Result]) -> Result; %% When input is empty ,stack has one element - the result
rpn_stack(["+" | T], [A, B | Stack]) -> rpn_stack(T, [A + B | Stack]); %%
rpn_stack(["-" | T], [A, B | Stack]) -> rpn_stack(T, [B - A | Stack]); %%
rpn_stack(["*" | T], [A, B | Stack]) -> rpn_stack(T, [A * B | Stack]); %%
rpn_stack(["/" | T], [A, B | Stack]) -> rpn_stack(T, [A / B | Stack]); %%
rpn_stack(["^" | T], [A, B | Stack]) -> rpn_stack(T, [math:pow(A, B) | Stack]); %%
rpn_stack([Num | T], Stack) -> rpn_stack(T, [element(1,string:to_integer(Num)) | Stack]). %% Push number onto stack


%% returns "ok" iff successful
rpn_test() ->
  5 = rpn("2 3 +"),
  87 = rpn("90 3 -"),
  -4 = rpn("10 4 3 + 2 * -"),
  -2.0 = rpn("10 4 3 + 2 * - 2 /"),
  ok = try
         rpn("90 34 12 33 55 66 + * - +")
       catch
         error:{badmatch, [_ | _]} -> ok
       end,
  4037 = rpn("90 34 12 33 55 66 + * - + -"),
  8.0 = rpn("2 3 ^"),
  true = math:sqrt(2) == rpn("2 0.5 ^"),
  true = math:log(2.7) == rpn("2.7 ln"),
  true = math:log10(2.7) == rpn("2.7 log10"),
  50 = rpn("10 10 10 20 sum"),
  10.0 = rpn("10 10 10 20 sum 5 /"),
  1000.0 = rpn("10 10 20 0.5 prod"),
  ok.