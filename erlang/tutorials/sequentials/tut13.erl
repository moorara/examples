% When a function defined elsewhere is used as a fun, it can be referred to as Function/Arity
% An anonymous variable _ is simply shorthand for a variable that gets a value, but the value is ignored
% sort(Fun, List): Fun has two arguments and returns true if the first argument is less than the second argument, or else false

-module(tut13).
-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
  {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
  {Name, {c, Temp}}.

convert_list_to_c(List) ->
  New_list = lists:map(fun convert_to_c/1, List),
  lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) -> Temp1 < Temp2 end, New_list).
