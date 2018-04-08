% Variables must start with a capital letter
% A variable can only be given a value once in its scope
% The semicolon indicates that there is more of the function to come

-module(tut1).
-export([fact/1, mult/2]).

fact(1) ->
  1;
fact(N) ->
  N * fact(N - 1).

mult(X, Y) ->
  X * Y.
