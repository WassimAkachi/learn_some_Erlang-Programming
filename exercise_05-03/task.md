# EXERCISE 5-3: SWAPPING HANDLERS

What happens if you want to close and open a new file in the log_handler? You would have to call event_manager:delete_handler/2 immediately followed by event_manager:add_handler/2. The risk with this is that in between these two calls, you might miss an event. Therefore, implement the following function:

```erlang
  event_manager:swap_handlers(Name, OldHandler, NewHandler)
```

which swaps the handlers atomically, ensuring that no events are lost. To ensure that the state of the handlers is maintained, pass the return value of OldHandler:terminate/1 to the NewHandler:init/1 call.