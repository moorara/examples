% there is no ; before end
% If no condition matches, a run-time failure occurs
% A condition that always succeeds is the atom true, and often used last in an if

-module(tut9).
-export([test_if/2]).

test_if(A, B) ->
  if 
    A == 5 ->
      io:format("A == 5~n", []),
      a_equals_5;
    B == 6 ->
      io:format("B == 6~n", []),
      b_equals_6;
    A == 2, B == 3 ->  % AND
      io:format("A == 2, B == 3~n", []),
      a_equals_2_b_equals_3;
    A == 1; B == 7 ->  % OR
      io:format("A == 1; B == 7~n", []),
      a_equals_1_or_b_equals_7
  end.
