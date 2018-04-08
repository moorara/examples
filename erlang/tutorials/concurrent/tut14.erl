% Each thread of execution is called a process
% Threads of execution (processes) in Erlang share no data
% Erlang BIF spawn is used to create a new process

-module(tut14).
-export([start/0, say/2]).

say(What, 0) ->
  done;
say(What, Times) ->
  io:format("~p~n", [What]),
  say(What, Times - 1).

start() ->
  spawn(tut14, say, [hello, 3]),
  spawn(tut14, say, [goodbye, 3]).
