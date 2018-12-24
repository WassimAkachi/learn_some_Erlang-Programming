-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Data) -> ok.

write(Key, Element, []) -> [{Key, Element}];
write(Key, Element, Data) -> 
  NewData = delete(Key, Data),
  [{Key, Element} | NewData].
  

delete(_, []) -> [];
delete(Key, Data) -> reverse(delete(Key, Data, [])).

delete(_, [], NewData) -> NewData;
delete(Key, [{Key, _} | RestData], NewData) -> delete(Key, RestData, NewData);
delete(Key, [HeadElement | RestData], NewData) -> delete(Key, RestData, [HeadElement | NewData]).

read(_, []) -> {error, instance};
read(Key, [{Key, Element} | _]) -> {ok, Element};
read(Key, [_ | RestData]) -> read(Key, RestData).

match(Element, Db) -> match(Element, Db, []).

match(_, [], Matches) ->  reverse(Matches);
match(Element, [{Key, Element} | RestData], Matches) -> match(Element, RestData, [Key | Matches]);
match(Element, [ _ | RestData], Matches) -> match(Element, RestData,  Matches).

% Helper function
reverse(Data) -> reverse(Data, []).
reverse([], List) ->  List;
reverse([H|T], List) ->  reverse(T, [H|List]).