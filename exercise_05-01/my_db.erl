-module(my_db).
-import(db, [new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-export([start/0, init/1, stop/0, write/2, delete/1, read/1, match/1]).

start() ->
  case whereis(my_db) of
    undefined -> register(my_db, spawn(my_db, init, [db:new()])), ok;
    _ -> {error, already_started}
  end.

init(Db) -> loop({db, Db}).

loop({db, Db}) ->
  receive
    {req, write, Key, Element} -> NewDb = db:write(Key, Element, Db), loop({db, NewDb});
  
    {req, delete, Key} -> NewDb = db:delete(Key, Db), loop({db, NewDb});

    {req, read, Key, To} -> Result = db:read(Key, Db), send(Result, To), loop({db, Db});

    {req, match, Element, To} -> Result = db:match(Element, Db), send(Result, To), loop({db, Db});

    {req, stop} -> ok
    end.

send(Result, To) ->  To ! {my_db, result, Result}.
send(Message) -> 
  case whereis(my_db) of
    undefined -> {error, already_stopped};
    _ -> my_db ! Message, ok
  end.

stop() -> send({req, stop}).

write(Key, Element) -> send({req, write, Key, Element}), ok.

delete(Key) -> send({req, delete, Key}), ok.

read(Key) -> send({req, read, Key, self()}), receive {my_db, result, Result} -> Result end.

match(Element) -> send({req, match, Element, self()}), receive {my_db, result, Result} -> Result end.
