%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc This is a skeleton that we can use for every gen_event callback module
%%%
%%% @end
%%% Created : 21. Oct 2015 21:22
%%%-------------------------------------------------------------------
-module(gen_event_template).
-author("Rafal Wolak").

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
  terminate/2, format_status/2]).

init([]) ->
  {ok, []}.

handle_event(_, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

format_status(_Opt, _StatusData) ->
  ok.
