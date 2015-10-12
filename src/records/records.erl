%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2015 15:20
%%%-------------------------------------------------------------------
-module(records).
-author("RafalW").


%% API
-compile(export_all).

-record(robot, {name,
  type = industrial,
  hobbies,
  details = []}).

first_robot() ->
  #robot{name = "Mechatron", type = handmade, details = ["Moved by smal man inside"]}.

-record(user, {id, name, group, age}).

%% use pattern matching to filter
admin_panel(#user{name = Name, group = admin}) ->
  Name ++ " is allowed!";
admin_panel(#user{name = Name}) ->
  Name ++ "is not allowed".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
%% Show stuff that can't be written in such a text
  allowed;
adult_section(_) ->
%% redirect to sesame street site
  forbidden.

repairman(Rob) ->
  Details = Rob#robot.details,
  NewRob = Rob#robot{details = ["Repaired by repairman" | Details]},
  {repaired, NewRob}.

-include("records.hrl").
included() -> #included{some_field="Some value"}.