-module(sum_1).
-export([sum/1]).

sum(A) when A >= 0 -> sum_acc(A, 0).

sum_acc(0, SumAcc) -> SumAcc;
sum_acc(A, SumAcc) -> sum_acc(A - 1, SumAcc + A). 