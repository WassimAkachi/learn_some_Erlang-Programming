-module(pretty_printer).
-export([print/1]).

%% A pretty printer, which will turn an exp into a string representation
print(Expression) -> lists:flatten(pretty_printer(Expression)).

pretty_printer({num, Num}) -> io_lib:format("~B", [Num]);
pretty_printer({minus, A, B}) -> 
  io_lib:format("(~s - ~s)", [pretty_printer(A), pretty_printer(B)]);
pretty_printer({plus, A, B}) -> 
  io_lib:format("(~s + ~s)", [pretty_printer(A), pretty_printer(B)]).
