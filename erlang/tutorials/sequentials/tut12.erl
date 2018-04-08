% foreach takes a list and applies a fun to every element in the list
% map creates a new list by applying a fun to every element in a list

-module(tut13).
-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
  {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
  {Name, {c, Temp}}.

convert_list_to_c(List) ->
  lists:map(fun convert_to_c/1, List).
