-module(evaluator).
-export([evaluate/1]).

%% An evaluator, which takes an exp and returns its value

evaluate({num, Num}) -> Num;
evaluate({minus, A, B}) -> evaluate(A) - evaluate(B);
evaluate({plus, A, B}) -> evaluate(A) + evaluate(B).
