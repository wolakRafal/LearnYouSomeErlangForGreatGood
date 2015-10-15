%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2015, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 23:13
%%%-------------------------------------------------------------------
-module(linkmon).
-author("Rafal Wolak").

%% API
-compile(export_all).

myproc() ->
  timer:sleep(5000),
  exit(reason).

echo(Name) ->
  receive
    exit ->
      io:format("Process '" + Name + "' exits"),
      exit("Process" + Name + " dies ");
    Msg ->
      io:format("Process '" + Name + "' receive" + Msg),
      echo(Name)
  end.


echo_link(Name, ChildPid) ->
  erlang:link(ChildPid),
  receive
    exit ->
      io:format("Process '" + Name + "' exits"),
      exit("Process" + Name + " dies ");
    Msg ->
      io:format("Process '" + Name + "' receive" + Msg),
      echo(Name)
  end.

echo_monitor(Name, ChildPid) ->
  erlang:monitor(process, ChildPid),
  receive
    exit ->
      io:format("Process '" + Name + "' exits"),
      exit("Process" + Name + " dies ");
    Msg ->
      io:format("Process '" + Name + "' receive" + Msg),
      echo(Name)
  end.


chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;
chain(N) ->
  Pid = spawn(fun() -> chain(N-1) end),
  link(Pid),
  receive
    _ -> ok
  end.

