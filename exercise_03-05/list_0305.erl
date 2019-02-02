-module(list_0305
).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

filter(List, N) -> filter(List, N, []).

filter([], _, Result) -> reverse(Result);
filter([H|T], N, Result) when N >= H -> filter(T, N, [H|Result]);
filter([_|T], N, Result) -> filter(T, N, Result).

reverse(Data) -> reverse(Data, []).
reverse([], List) ->  List;
reverse([H|T], List) ->  reverse(T, [H|List]).

concatenate(List) -> concatenate(List, []).

concatenate([], Result) -> reverse(Result);
concatenate([H|T], Result) -> 
  concatenate(T, add_to_list(H , Result)).


add_to_list([], Result) -> Result;
add_to_list([H|T], Result) -> add_to_list(T, [H | Result]).

flatten(List) -> flatten_help(List).

flatten_help([]) -> [];
flatten_help([H|T]) -> flatten_help(H) ++ flatten_help(T);
flatten_help(H) -> [H].
