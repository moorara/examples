% Each file contains an Erlang module
% The file used to store the module must have the same name as the module

-module(tut).
-export([double/1]).

double(X) ->
  2 * X.
