% Atom is a data type in Erlang
% Atoms start with a small letter
% The two parts of the function are called its clauses

-module(tut2).
-export([convert/2]).

convert(M, inch) ->
  M / 2.54;
convert(N, centimeter) ->
  N * 2.54.
