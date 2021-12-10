
-module(one).

-define(isopen(C), C == $( orelse C == $[ orelse C == ${ orelse C == $<).
-define(isclose(C), C == $) orelse C == $] orelse C == $} orelse C == $>).

validate([$) | XS], [$( | Stack]) -> validate(XS, Stack);
validate([$] | XS], [$[ | Stack]) -> validate(XS, Stack);
validate([$} | XS], [${ | Stack]) -> validate(XS, Stack);
validate([$> | XS], [$< | Stack]) -> validate(XS, Stack);

validate([C | _], _) when ?isclose(C) -> {corrupted, C};
validate([C | XS], Stack) when ?isopen(C) -> validate(XS, [C | Stack]);

validate([], []) -> {ok, []};
validate([], Stack) -> {unbalanced, Stack}.
validate(Input) -> validate(Input, []).

score($)) -> 3;
score($]) -> 57;
score($}) -> 1197;
score($>) -> 25137;
score(_)  -> 0.


readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Lines = binary:split(Data, [<<"\n">>], [global]),
    [binary_to_list(X) || X <- Lines].

main([FileName]) ->
  Lines = readlines(FileName),
  Scores = [score(Value) || Line <- Lines, {Result, Value} <- [validate(Line)], Result == corrupted],
  io:format("~p~n", [lists:sum(Scores)]);
main(_) ->
    usage().

usage() -> io:format(standard_error, "Usage: escript one.erl <input>~n", []).