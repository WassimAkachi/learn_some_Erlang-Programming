-module(list_0302).
-export([create/1, reverse_create/1]).

create(N) when N >= 0 -> create_acc(N, []).
reverse_create(N) when N >= 0 -> lists:reverse(create(N)).

create_acc(0, List) -> List;
create_acc(N, List) -> create_acc(N - 1, [N | List]).