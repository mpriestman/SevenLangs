-module(count_up).
-export([count/1]).

count(N) -> io:format()
count(10) -> io:format("number ~w~n", [N]).