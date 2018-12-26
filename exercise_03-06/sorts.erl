-module(sorts).
-export([quicksort/1]).

quicksort([]) -> [];
quicksort([OneElement]) -> [OneElement];
quicksort([Pivot|Rest]) ->
  {SmallerThanPivot, GreaterThanPivot} = partition(Pivot, Rest),
  SortedSmallers = quicksort(SmallerThanPivot),
  SortedGreaters = quicksort(GreaterThanPivot),
  SortedSmallers ++ [Pivot] ++ SortedGreaters.

%% Split the List into two partions Smaller/Greater-Collection
partition(Pivot, List) -> 
  AllSmaller = [],
  AllGreater = [],
  partition(Pivot,AllSmaller, AllGreater, List).

%% Base case
%% When the list to sort is empty, than return the Smaller and Greater
partition(_Pivot, AllSmaller, AllGreater, []) -> {AllSmaller, AllGreater};

%% When the element is smaller than the pivot element
%% than add the element to the smaller collection
partition(Pivot, AllSmaller, AllGreater, [First|Rest]) when First < Pivot -> 
  partition(Pivot, [First|AllSmaller], AllGreater, Rest);

%% When the element is greater-equal than the pivot element
%% than add the element to the greater collection
partition(Pivot, AllSmaller, AllGreater, [First|Rest]) when First >= Pivot -> 
  partition(Pivot, AllSmaller, [First|AllGreater], Rest).
