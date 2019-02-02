-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).


init(init) -> [];
init([H|T]) -> [H|T].


terminate(Data) -> Data.

handle_event({Action, _Id, Event}, OldData) -> 
  Key = {Action, Event},
  case lists:keytake(Key, 1, OldData) of
    false -> [{Key, 1} | OldData];
    {value, {Key, Count}, ListWithoutFoundedElement} -> [{Key, Count+1} | ListWithoutFoundedElement]
  end.
