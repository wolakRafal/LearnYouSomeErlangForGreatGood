%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2015 15:30
%%%-------------------------------------------------------------------
-author("RafalW").

%% this is a .hrl (header) file.
-record(included, {some_field,
  some_default = "yeah!",
  unimaginative_name}).