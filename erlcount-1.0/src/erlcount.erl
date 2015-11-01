%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 26. Oct 2015 19:22
%%%-------------------------------------------------------------------
-module(erlcount).
-author("Rafal Wolak").
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
  erlcount_sup:start_link().

stop(_State) ->
  ok.