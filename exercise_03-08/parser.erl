-module(parser).
-export([parse/1]).

% 4

parse(Expression)  -> parse_1(unknown, Expression, [], []).

parse_1(unknown, [], [], Result) ->
  Result;

parse_1(unknown, [H|T], [], Result) ->
  Type = type_of(H),
  case Type of
    unknown -> {error, lists:flatten(io_lib:format("element '~c' is unkown.", [H]))};
    space   -> parse_1(unknown, T, [], Result); 
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

parse_1(devide, [H|T], [], Result) when H == $/ ->
  parse_1(unknown, T, [], Result ++ [{devide}]);

parse_1(parant_close, [H|T], [], Result) when H == $) ->
  parse_1(unknown, T, [], Result ++ [{parant_close}]).


%% Helper
type_of(N)  when N >= $0 andalso N =< $9 -> num;
type_of(N)  when N == $(        -> parant_open;
type_of(N)  when N == $)       -> parant_close;
type_of(N)  when N == $+        -> plus;
type_of(N)  when N == $-        -> minus;
type_of(N)  when N == $*        -> multiply;
type_of(N)  when N == $/        -> devide;
type_of(N)  when N == 32        -> space;
type_of(_)  -> unknown.

convert_to_number(List) when is_list(List) -> {num, list_to_integer(List)}.
