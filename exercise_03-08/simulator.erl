-module(simulator).
-export([simulate/1]).

%% A simulator which will implement expressions for the stack machine

simulate(Expression) -> simulate_1(Expression, []).

%% [{push,2},{push,3},{plus},{push,4},{minus}]
simulate_1([], [Result]) -> Result;

simulate_1([{push, Num}|Data], Stack) -> simulate_1(Data, Stack ++ [Num]);

simulate_1([{minus}|Data], [A,B|Stack]) -> 
  NewStack = Stack ++ [A - B],
  simulate_1(Data, NewStack);

simulate_1([{plus}|Data], [A,B|Stack]) -> 
  NewStack = Stack ++ [A + B],
  simulate_1(Data, NewStack).

