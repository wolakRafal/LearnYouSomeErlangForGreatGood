%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2015 22:20
%%%-------------------------------------------------------------------
-module(dolphins).
-author("Rafal Wolak").

%% API
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip ->
      io:format("How about no?~n");
    fish ->
      io:format("Thanks for fish~n");
    _ ->
      io:format("Heh, not supported!")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} ->
      From ! "No, cant do this!";
    {From, fish} ->
      From ! "Thanks for the fish a lot! 2";
    _ -> io:format("Heh, we're smarter than ypu humans.")
  end.


dolphin3() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?",
      dolphin3();
    {From, fish} ->
      From ! "So long and thanks for all the fish!";
    _ ->
      io:format("Heh, we're smarter than you humans.~n"),
      dolphin3()
  end.