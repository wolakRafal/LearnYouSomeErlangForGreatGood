%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 20. paŸ 2015 22:38
%%%-------------------------------------------------------------------
-module(my_behaviour).
-author("Rafal Wolak").

%% API
-export([behaviour_info/1]).
%% init/1, some_fun/0 and other/3 are now expected callbacks
behaviour_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}];
behaviour_info(_) -> undefined.