% The | operator can also be used to add a head to a list
% The module lists contains many functions for manipulating lists

-module(tut8).
-export([reverse/1]).

reverse(List) ->
  reverse(List, []).

reverse([Head | Rest], Reversed_List) ->
  reverse(Rest, [Head | Reversed_List]);
reverse([], Reversed_List) ->
  Reversed_List.
