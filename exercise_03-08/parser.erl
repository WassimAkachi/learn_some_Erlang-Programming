-module(parser).
-export([parse/1]).

% 4

parse(Expression)  -> 
  ReversedExpression = lists:reverse(Expression),
  NewExpression = correcte_parants(ReversedExpression),
  Parsed = parse_1(unknown, NewExpression, [], []),
  Postfixed = to_postfix(Parsed),
  Prefixed = lists:reverse(Postfixed),
  format(Prefixed).

correcte_parants(List) -> lists:reverse(correcte_parants(List, [])).

correcte_parants([], Result) -> Result;
correcte_parants([H|T], Result) when H == $( -> correcte_parants(T, [$) | Result]);
correcte_parants([H|T], Result) when H == $) -> correcte_parants(T, [$( | Result]);
correcte_parants([H|T], Result) -> correcte_parants(T, [H | Result]).

parse_1(unknown, [], [], Result) ->
  Result;

parse_1(unknown, [H|T], [], Result) ->
  Type = type_of(H),
  case Type of
    unknown -> {error, lists:flatten(io_lib:format("element '~c' is unkown.", [H]))};
    ignore   -> parse_1(unknown, T, [], Result); 
    _       -> parse_1(Type, [H|T], [], Result)
  end;

parse_1(num, [H|T], Acc, Result) when H >= $0 andalso H =< $9 ->
  parse_1(num, T, Acc ++ [H], Result);

parse_1(num, [], Acc, Result) ->
  N = convert_to_number(Acc),
  parse_1(unknown, [], [], Result ++ [N]);

parse_1(num, [H|T], Acc, Result) when H < $0 orelse H > $9 ->
  N = convert_to_number(Acc),
  parse_1(unknown, [H|T], [], Result ++ [N]);

parse_1(parant_open, [H|T], [], Result) when H == $( ->
  parse_1(unknown, T, [], Result ++ [{parant_open}]);

parse_1(plus, [H|T], [], Result) when H == $+ ->
  parse_1(unknown, T, [], Result ++ [{plus}]);

parse_1(minus, [H|T], [], Result) when H == $- ->
  parse_1(unknown, T, [], Result ++ [{minus}]);

parse_1(multiply, [H|T], [], Result) when H == $* ->
  parse_1(unknown, T, [], Result ++ [{multiply}]);

parse_1(divide, [H|T], [], Result) when H == $/ ->
  parse_1(unknown, T, [], Result ++ [{divide}]);

parse_1(parant_close, [H|T], [], Result) when H == $) ->
  parse_1(unknown, T, [], Result ++ [{parant_close}]).

to_postfix(List) -> to_postfix(List, [], []).

to_postfix([], [], Result) -> Result;

to_postfix([{num, N}|T], Stack, Result) -> 
  to_postfix(T, Stack, Result ++ [{num, N}]);

to_postfix([N|T], Stack, Result) when N == {parant_open} -> 
  to_postfix(T, [N|Stack], Result);

to_postfix([N|T], Stack, Result) when N == {parant_close} -> 
  {NewStack, NewResult} = to_postfix(Stack, Result),
  to_postfix(T, NewStack, NewResult);

to_postfix([N|T], [], Result) when 
  N == {minus}  orelse N == {plus} orelse N == {multiply} orelse N == {divide} ->
  to_postfix(T, [N], Result);

to_postfix([HExp|TExp], [HStack|TStack], Result) when HStack == {parant_open} ->
  to_postfix(TExp, [HExp, HStack | TStack], Result);

to_postfix([], [HStack|TStack], Result) ->
  to_postfix([], TStack, Result ++ [HStack]).


to_postfix([], Result) -> {[], Result};
to_postfix([{parant_open}|T], Result) -> {T, Result};
to_postfix([N|T], Result) -> to_postfix(T, Result ++ [N]).


format(List) -> 
  {Result, _} = format_1(List),
  Result.

format_1([]) -> {ignore, []};
format_1([{num,A}|T]) -> {{num,A}, T};

format_1([{Op}|T]) when Op == plus orelse Op == minus orelse Op == multiply orelse Op == divide -> 
  {A, Rest1} = format_1(T),
  {B, Rest2} = format_1(Rest1),
  {{Op, A, B}, Rest2}.

%% Helper
type_of(N)  when N >= $0 andalso N =< $9 -> num;
type_of(N)  when N == $(        -> parant_open;
type_of(N)  when N == $)       -> parant_close;
type_of(N)  when N == $+        -> plus;
type_of(N)  when N == $-        -> minus;
type_of(N)  when N == $*        -> multiply;
type_of(N)  when N == $/        -> divide;
type_of(N)  when N == 32        -> ignore; % Spacee
type_of(_)  -> unknown.

convert_to_number(List) when is_list(List) -> {num, list_to_integer(lists:reverse(List))}.
