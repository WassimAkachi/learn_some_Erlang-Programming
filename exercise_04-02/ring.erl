-module(ring).
-export([start/3, process_init/4, shutdown/0]).

start(N, M, Message) when N > 0 andalso M > 0 ->
  start_1(N, M, Message).

start_1(N, M, Message) ->
  register(ring_head_node, spawn(ring, process_init, [head, N, M, Message])),
  ok.

process_init(head, N, M, Message) ->
  print("Head node is registered"),
  spawn(ring, process_init, [self(), N-1, M, Message]),
  print("Head node start looping"),
  loop({self(), M, Message});

process_init(ParentPid, 1, M, Message) -> 
  print({"Last Process is started", parrent, ParentPid, proc_number, 1}),
  ring_head_node ! {request_finish_build, {my_pid, self()}},
  print("Last node start looping"),
  loop({ParentPid, M, Message});

process_init(ParentPid, N, M, Message) ->
  print({"New process is starting", parrent, ParentPid, proc_number, N}),
  spawn(ring, process_init, [self(), N-1, M, Message]),
  print({"Node start looping", parrent, ParentPid, proc_number, N}),
  loop({ParentPid, M, Message}).

loop({ParentPid, 0, Message}) -> 
  print({"Process is not expecting messages"}),
  receive 
    {request, stop} -> 
      print("Process shutding down"),
      send(1, {request, stop}, ParentPid);

    {request, message, Message} ->
        print({"Node received last message."}),
        loop({ParentPid, 0, Message});

    Msg -> print({message, Msg}), loop({ParentPid, 0, Message})
  end;

loop({ParentPid, M, Message}) -> 
  print({"Process is expecting messages", expected_messages, M}),
  receive 
    {request, stop} -> send(1, {request, stop}, ParentPid);

    {request_finish_build, {my_pid, Pid}} -> 
      print({"Last Process is started", parrent, Pid}),
      send(M, {request, message, Message}, Pid),
      loop({Pid, 0, Message});

    {request, message, Message} ->
      print({"Node received message. And send to", send_to, ParentPid}),
      send(1, {request, message, Message}, ParentPid),
      loop({ParentPid, M-1, Message});

    Msg -> io:format("Msg: ~w ~n", [Msg]), loop({ParentPid, M, Message})
  end.

send(0, _, _) -> ok;
send(M, Message, Pid) -> 
  print({send_to, Pid, message_to_send, Message}),
  Pid ! Message, send(M-1, Message, Pid).

shutdown() -> ring_head_node ! {request, stop}, ok.

print(Object) -> io:format("~p ~n", [{self(), msg, Object}]).