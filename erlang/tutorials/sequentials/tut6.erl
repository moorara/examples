% Functions with the same name can be distinguished by different arity (no. of arguments)
% Functions can have gaurds. If the guard fails (the guard is false), the next clause is tried

-module(tut6).
-export([list_max/1]).

list_max([Head | Rest]) ->
  list_max(Rest, Head).

list_max([], Res) ->
  Res;
list_max([Head | Rest], Max) when Head > Max ->
  list_max(Rest, Head);
list_max([Head | Rest], Max)  ->
  list_max(Rest, Max).
