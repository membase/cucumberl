-module(simple_sample_table).

-export([setup/0, teardown/1,
         given/3, 'when'/3, then/3, main/0]).

setup() ->
    [].

teardown(_State) ->
    ok.

%% Step definitions for the sample calculator Addition feature.
given([i, have, entered, N, into, the, calculator], State, _) ->
    {ok, simple_sample:enter(State, list_to_integer(atom_to_list(N)))};
given([i, have, cleared, the, calculator], _State, _) ->
    {ok, []}.

'when'([i, press, Op], State, _) ->
    {ok, simple_sample:press(State, Op)}.

then([the, result, should, be, Result, on, the, screen],
     State, _) ->
    list_to_integer(atom_to_list(Result)) =:= State.

%% A main() to kick it all off...

main() ->
    cucumberl:run("./features/simple_sample_table.feature").

