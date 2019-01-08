-module(frequency).

-ifdef(debug_print).
  -define(DEBUG(Format, Content), io:format(Format, Content)).
-else.
  -define(DEBUG(Format, Content), ok).
-endif.

-export([start/0, init/0, stop/0, allocate/0, deallocate/1]).

init() ->
  loop({get_frequencies(), []}).

loop({FreeFrequencies, AllocatedFrequencies}) ->
  receive 
    {?MODULE, {allocate, Pid}} ->
      {Free, Allocated} = allocate_frequency_for({Pid, FreeFrequencies, AllocatedFrequencies}),
      ?DEBUG("~p~n", [{allocate, Pid}]),
      loop({Free, Allocated});
    
    {?MODULE, {deallocate, Pid, Freq}} ->
      {Free, Allocated} = deallocate_frequency_for({Pid, Freq, FreeFrequencies, AllocatedFrequencies}),
      ?DEBUG("~p~n", [{deallocate, Pid, Freq}]),
      loop({Free, Allocated});
    
    {?MODULE, {stop, Pid}} ->
      stop_node(Pid, FreeFrequencies, AllocatedFrequencies)
  end.

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

stop_node(_Pid, _FreeFrequencies, []) -> ok;

stop_node(_Pid, _FreeFrequencies, [{Pid, _} | AllocatedFrequencies]) -> 
  exit(Pid, {?MODULE, is_shuting_down}),
  stop_node(_Pid, _FreeFrequencies, AllocatedFrequencies);

stop_node(_Pid, _FreeFrequencies, _AllocatedFrequencies) -> ok.

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
  call({stop, self()}), ok.


allocate() ->
  flush(),
  call({allocate, self()}),
  receive 
    {?MODULE, replay, {error, no_frequence}} -> {error, no_frequence};
    {?MODULE, replay, {ok, Freq}} -> Freq;
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
