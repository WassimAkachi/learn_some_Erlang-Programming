-module(file_indexer).
-export([index/1]).

index(Filename) -> index_1(Filename).

index_1(Filename) -> 
  AllLines = readlines(Filename),
  FormattedIndexList = format_index(dict:to_list(index_1(AllLines, dict:new()))),
  SortedList = lists:sort(fun ({Wa, _}, {Wb, _}) -> string:to_upper(Wa) < string:to_upper(Wb) end, FormattedIndexList),
  print_formatted_index_list(SortedList).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device, [], 0)
      after file:close(Device)
    end.

get_all_lines(Device, Lines, LineNumber) ->
    case io:get_line(Device, "") of
        eof  -> Lines;
        Line -> CurrentLine = LineNumber + 1, get_all_lines(Device, Lines ++ [{CurrentLine, Line}], CurrentLine)
    end.

index_1([], Index) -> Index;
index_1([Line|Lines], Index) -> 
  NewIndex = index_line(Line, Index),
  index_1(Lines, NewIndex).

index_line({LineNumber, Line}, Index) -> index_line_1(Line,[], Index, LineNumber).

index_line_1([], [], Index, _) -> Index;
index_line_1([Char|Words], Acc, Index, LineNumber) -> 
  case type_of(Char) of 
    word -> index_line_1(Words, Acc ++ [Char], Index, LineNumber);
    _ ->  NewIndex = index_word({LineNumber, Acc}, Index),
          index_line_1(Words, [], NewIndex, LineNumber)
    end.

index_word({_, []}, Index) -> Index;
index_word({LineNumber, Word}, Index) ->
  case dict:take(Word, Index) of
    error -> dict:store(Word, [LineNumber], Index);
    {Value, NewIndex} -> dict:store(Word, Value ++ [LineNumber], NewIndex);
    _ -> Index
  end.

type_of(Char) when Char >= $a andalso Char =< $z -> word;
type_of(Char) when Char >= $A andalso Char =< $Z -> word;
type_of(_) -> not_word.

format_index(List) -> format_index_1(List, []).

format_index_1([], List) -> List;
format_index_1([H|T], NewIndex) -> 
  FormatedEntry = format_index_entry(H),
  format_index_1(T, NewIndex ++ [FormatedEntry]).

 format_index_entry({Key, List}) -> 
   {Key, format_list(List)}.

format_list(List) -> format_list(lists:sort(List), [], []).

format_list([], [], NewList) -> NewList;
format_list([], Acc, NewList) -> format_list([], [], NewList ++ [to_tuple(Acc)]);
format_list([HList|TList], [], NewList) -> format_list(TList, [HList], NewList);
format_list([HList|TList], [HList|TAcc], NewList) -> format_list(TList, [HList|TAcc], NewList);
format_list([HList|TList], [HAcc|TAcc], NewList) when HList == HAcc + 1  -> format_list(TList, [HList, HAcc | TAcc], NewList);
format_list([HList|TList], Acc, NewList) ->  format_list([HList|TList], [], NewList ++ [to_tuple(Acc)]).

to_tuple([OneElement]) -> {OneElement, OneElement};
to_tuple([First, Second]) -> {Second, First};
to_tuple([First|Tail]) -> {lists:last(Tail), First}.

print_formatted_index_list([]) -> ok;
print_formatted_index_list([H|T]) -> print_index_entry(H), print_formatted_index_list(T).

print_index_entry({Word, List}) ->
  io:format("~-20s  ~s~n", [Word, convert_list_to_string(List)]).

convert_list_to_string(List) -> 
  NewList = lists:map(fun convert_to_string/1,List),
  convert_list_to_string_1(NewList, "").

convert_to_string({A, A}) -> io_lib:format("~B", [A]);
convert_to_string({A, B}) -> io_lib:format("~B-~B", [A, B]).

convert_list_to_string_1([], Acc) -> Acc;
convert_list_to_string_1([H|T], Acc) when T == [] ->   convert_list_to_string_1(T, Acc ++ H);
convert_list_to_string_1([H|T], Acc) ->   convert_list_to_string_1(T, Acc ++ H ++ [$,]).
