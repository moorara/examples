% To look up information about standard modules, the command erl -man <module> can be used
% The module io contains many functions that help in doing formatted input/output

-module(tut5).
-export([format_temps/1]).

format_temps([]) ->
  ok;
format_temps([City | Rest]) ->
  print_temp(convert_to_celsius(City)),
  format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->
  {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->
  {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
  io:format("~-15w ~w c~n", [Name, Temp]).
