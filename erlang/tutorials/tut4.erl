% Lists are surrounded by square brackets
% Strings can be represented by lists of characters
% The Erlang shell is smart and outputs lists in the most appropriate form

-module(tut4).
-export([list_len/1]).

list_len([]) ->
  0;
list_len([First | Rest]) ->
  1 + list_len(Rest).
