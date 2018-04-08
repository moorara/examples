% Tuples are surrounded by curly brackets
% Each item in a tuple is called an element
% Tuples can have many elements and contain any valid Erlang term

-module(tut3).
-export([convert/1]).

convert({centimeter, X}) ->
  {inch, X / 2.54};
convert({inch, Y}) ->
  {centimeter, Y * 2.54}.
