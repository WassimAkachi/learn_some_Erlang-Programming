-module(echo).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
  case whereis(echo) of
    undefined -> register(echo, spawn(echo, loop, [])), ok;
    _ -> ok
  end.

loop() -> 
  receive
    {request, print, Message} -> io:format("~w~n", [Message]), loop();
    {request, stop} -> ok;
    _ -> loop()
  end.

print(Message) -> 
  echo ! {request, print, Message},
  ok.

stop() ->
  echo ! {request, stop},
  ok.