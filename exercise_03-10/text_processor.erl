-module(text_processor).
-export([filled/1]).


filled(Text) -> print(filled(Text, 40,[])).

filled([], _, _) -> "";
filled(H, LineWidth, Acc) -> fille_line(string:tokens(H, " "), LineWidth, Acc, 0, []).

fille_line([], _, Acc, _, Line) -> Acc ++ [Line];
fille_line(Words, Counter, Acc, Counter, Line) -> 
  fille_line(Words, Counter, Acc ++ [Line], 0, []);
fille_line([H|Words], LineWidth, Acc, Counter, Line) when LineWidth > Counter + 1 + length(H) -> 
  fille_line(Words, LineWidth, Acc, Counter + 1 + length(H), Line ++ [H] ++ " ");
fille_line([H|Words], LineWidth, Acc, _, Line) -> 
  fille_line(Words, LineWidth, Acc ++ [lists:flatten(Line)], length(H), H ++ " ").
  
print([]) -> ok;
print([H|T]) -> io:format("~s~n", [H]), print(T).
