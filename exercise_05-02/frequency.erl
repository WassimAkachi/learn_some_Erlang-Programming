-module(frequency).
-export([start/0, init/0, stop/0, allocate/0, deallocate/1]).
-include("../defines.hrl").

init() ->
  loop({get_frequencies(), []}).

loop({FreeFrequencies, AllocatedFrequencies}) ->
  receive 
    {?MODULE, {stop, _Pid}} ->
      stop_node({FreeFrequencies, AllocatedFrequencies});

    {?MODULE, Message} ->
      NewState = handle_msg(Message, {FreeFrequencies, AllocatedFrequencies}),
      loop(NewState)
  end.


handle_msg({deallocate, Pid, Freq}, {FreeFrequencies, AllocatedFrequencies}) ->
  {Free, Allocated} = deallocate_frequency_for({Pid, Freq, FreeFrequencies, AllocatedFrequencies}),
  ?DEBUG("~p~n", [{deallocate, Pid, Freq}]),
  {Free, Allocated};


handle_msg({allocate, Pid}, {FreeFrequencies, AllocatedFrequencies}) ->
  case lists:any(fun(Item) -> {APid, _} = Item, APid == Pid end, AllocatedFrequencies) of
    true -> 
      replay(Pid, {error, you_have_already_a_frequence}),
      ?DEBUG("~p~n", [{not_allocated, Pid}]),
      {Free, Allocated} = {FreeFrequencies, AllocatedFrequencies};
    
    false -> 
      {Free, Allocated} = allocate_frequency_for({Pid, FreeFrequencies, AllocatedFrequencies}),
      ?DEBUG("~p~n", [{allocated, Pid}])
  end,
  {Free, Allocated}.


allocate_frequency_for({Pid, [], AllocatedFrequencies}) -> 
  replay({error, no_frequence}, Pid),
  {[], AllocatedFrequencies};

allocate_frequency_for({Pid, [Freq | FreeFrequencies], AllocatedFrequencies}) -> 
  NewAllocated = [{Pid, Freq}|AllocatedFrequencies],
  replay({ok, Freq}, Pid),
  {FreeFrequencies, NewAllocated}.

deallocate_frequency_for({Pid, Freq, FreeFrequencies, AllocatedFrequencies}) -> 
  deallocate_frequency({Pid, Freq, FreeFrequencies, AllocatedFrequencies}, []).

deallocate_frequency({Pid, _, FreeFrequencies, []}, Acc) -> 
  replay({error, no_frequence}, Pid),
  {FreeFrequencies, Acc};

deallocate_frequency({Pid, Freq, FreeFrequencies, [{Pid, Freq}|AllocatedFrequencies]}, Acc) ->
  replay(ok, Pid),
  {[Freq | FreeFrequencies], AllocatedFrequencies ++ Acc};

deallocate_frequency({Pid, Freq, FreeFrequencies, [Item | AllocatedFrequencies]}, Acc) ->
  deallocate_frequency({Pid, Freq, FreeFrequencies, AllocatedFrequencies}, [Item | Acc]).

stop_node({_, []}) -> ok;

stop_node({_, AllocatedFrequencies}) -> 
  receive

    {?MODULE, {allocate, Pid}} -> 
      replay({error, server_is_shutting_down}, Pid),
      stop_node({[], AllocatedFrequencies});

    {?MODULE, {deallocate, Pid, Freq}} ->
      {_, Allocated} = deallocate_frequency_for({Pid, Freq, [], AllocatedFrequencies}),
      ?DEBUG("~p~n", [{deallocate, Pid, Freq}]),
      stop_node({[], Allocated})

  end.

get_frequencies() -> [89,67].

call(Message) -> ?MODULE ! {?MODULE, Message}, ok.
replay(Message, ToPid) -> ToPid ! {?MODULE, replay, Message}, ok.

start() -> 
  ?DEBUG("start ~p~n", [?MODULE]),
  case whereis(?MODULE) of 
    undefined -> register(?MODULE, spawn(?MODULE, init, [])), ok;
    _ -> {error, already_started}
    end.


stop() ->
  ?DEBUG("stop ~p~n", [?MODULE]),
  case whereis(?MODULE) of 
    undefined -> {error, already_stopped};
    _ -> call({stop, self()}), ok
    end.


allocate() ->
  flush(),
  call({allocate, self()}),
  receive 
    {?MODULE, replay, {error, no_frequence}} -> {error, no_frequence};
    {?MODULE, replay, {ok, Freq}} -> Freq;
    {?MODULE, replay, {error, server_is_shutting_down}} -> server_is_shutting_down;
    Msq -> Msq
    end.


deallocate(Freq) ->
  flush(),
  call({deallocate, self(), Freq}),
  receive 
    {?MODULE, replay, ok} -> ok;
    {?MODULE, replay, Message} -> Message;
    Msq -> Msq
  end.


flush() -> 
  receive 
    Msq -> ?DEBUG("Message: ~p~n", [Msq]), flush()
  after 0 ->
    ok
  end.
