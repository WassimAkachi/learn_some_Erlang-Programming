-module(sum_2).
-export([sum/2]).

sum(A, B) when B >= A -> sum_acc(A, B, 0).

sum_acc(B, B, SumAcc) -> SumAcc + B;
sum_acc(A, B, SumAcc) -> sum_acc(A + 1, B, SumAcc + A). 