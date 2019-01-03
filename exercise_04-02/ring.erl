-module(ring).
-export([start/3, process_init/4, shutdown/0]).

start(N, M, Message) when N > 0 andalso M > 0 ->
  start_1(N, M, Message).

start_1(N, M, Message) ->
  register(ring_head_node, spawn(ring, process_init, [head, N, M, Message])),
  ok.

process_init(head, N, M, Message) ->
  spawn(ring, process_init, [self(), N-1, M, Message]),
  loop({self(), M, Message});

process_init(ParentPid, 1, M, Message) -> 
  print({"Last Process started", self()}),
  ring_head_node ! {request_finish_build, {my_pid, self()}},
  loop({ParentPid, M, Message});

process_init(ParentPid, N, M, Message) ->
  print({"New process is starting", ParentPid, N, M, Message}),
  spawn(ring, process_init, [self(), N-1, M, Message]),
  loop({ParentPid, M, Message}).

loop({ParentPid, 0, Message}) -> 
  print({"Process has expecting 0 messages", ParentPid}),
  receive 
    {request, stop} -> 
      print("Process shutding down"),
      ParentPid ! {request, stop};

    Msg -> io:format("Msg: ~w ~n", [Msg]), loop({ParentPid, 0, Message})
  end;

loop({ParentPid, M, Message}) -> 
  print({"Process has expecting messages", ParentPid, M}),
  receive 
    {request, stop} -> ParentPid ! {request, stop};

    {request_finish_build, {my_pid, Pid}} -> 
      print("Last Process is started"),
      Pid ! {request, message, Message},
      loop({Pid, M, Message});

    {request, message, Message} ->
      ParentPid ! {request, message, Message},
      loop({ParentPid, M-1, Message});

    Msg -> io:format("Msg: ~w ~n", [Msg]), loop({ParentPid, M, Message})
  end.

shutdown() -> ring_head_node ! {request, stop}.

print(Object) -> io:format("~p ~n", [{self(), msg, Object}]).