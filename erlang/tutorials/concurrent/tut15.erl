% spawn returns a process identifier (pid)
% self() returns the pid of the executor process
% the operator ! is used to send messages
% Messages are valid Erlang terms (integers, atoms, tuples, lists, pids, etc.)
% Each process has its own input queue for messages
% When a process executes a receive, the first message in the queue is matched against the patterns in the receive
% If there is no pattern to match, the message is kept in the queue and the next message is tried

-module(tut15).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("ping finished~n", []);
ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1, Pong_PID).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start() ->
  Pong_PID = spawn(tut15, pong, []),
  spawn(tut15, ping, [3, Pong_PID]).
