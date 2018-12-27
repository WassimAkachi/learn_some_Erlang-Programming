-module(compiler).
-export([compile/1]).

%% A compiler, which transforms an exp into a sequence of code for a stack machine to evaluate the exp

compile(Expression) -> compile_1(Expression).

compile_1({num, Num}) -> [{push, Num}];
compile_1({minus, A, B}) ->  compile_1(A) ++ compile_1(B) ++ [{minus}] ;
compile_1({plus, A, B}) -> compile_1(A) ++ compile_1(B) ++ [{'plus'}].
