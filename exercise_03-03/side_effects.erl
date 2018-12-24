-module(side_effects).
-export([print/1, print_even/1]).


print(N) -> 
  io:format("Number: ~p~n", [create(N)]).

print_even(N) -> 
    io:format("Number: ~p~n", [create_even(N)]).

create(N) when N >= 0 -> create_acc(N, []).

create_acc(0, List) -> List;
create_acc(N, List) -> create_acc(N - 1, [N | List]).

create_even(N) when N >= 0 -> create_even(N, []).

create_even(0, List) -> List;
create_even(N, List) when (N rem 2) == 1 -> create_even(N - 1, List);
create_even(N, List) when (N rem 2) == 0 -> create_even(N - 1, [N | List]).
  
