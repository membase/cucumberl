-module(sample).

-compile(export_all).
-compile({no_auto_import,[get/0]}).

% Step definitions for the sample calculator Addition feature.

step([given, i, have, entered, N, into, the, calculator], _) ->
    enter(list_to_integer(atom_to_list(N)));

step(['when', i, press, Op], _) ->
    press(Op);

step(['then', the, result, should, be, Result, on, the, screen], _) ->
    [list_to_integer(atom_to_list(Result))] =:= get(calculator);

step(_, _) -> undefined.

% Implementing a simple model here...

enter(N) ->
    State = case get(calculator) of
        undefined ->
            [N|[]];
        Val ->
            [N|Val]
    end,
    put(calculator, State),
    State.

press(Op) ->
    Result = apply(?MODULE, Op, get(calculator)),
    put(calculator, [Result]),
    Result.

add(X, Y) ->
    X + Y.

% A main() to kick it all off...

main() ->
    put(calculator, []),
    cucumberl:run("./features/sample.feature", [?MODULE]).

