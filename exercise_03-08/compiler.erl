-module(parser).
-export([print/1]).

%% A pretty printer, which will turn an exp into a string representation
print(Expression) -> lists:flatten(pretty_printer_1(Expression)).

pretty_printer_1({num, Num}) -> io_lib:format("~B", [Num]);
pretty_printer_1({minus, A, B}) -> 
  io_lib:format("(~s - ~s)", [pretty_printer_1(A), pretty_printer_1(B)]);
pretty_printer_1({plus, A, B}) -> 
  io_lib:format("(~s + ~s)", [pretty_printer_1(A), pretty_printer_1(B)]).
