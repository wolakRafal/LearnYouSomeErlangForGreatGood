%%%-------------------------------------------------------------------
%%% @author robo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2015 16:30
%%%-------------------------------------------------------------------
-module(hello).
-author("robo").

%% API
-export([hello_world/1]).
hello_world(Name) -> io:format("hello, ~s\n", [Name]).