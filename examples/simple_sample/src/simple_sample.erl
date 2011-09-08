-module(simple_sample).

-export([setup/0, teardown/1,
         given/3, 'when'/3, then/3, main/0]).

-export([enter/2, press/2]).

setup() ->
    [].

%% Step definitions for the sample calculator Addition feature.
given([i, have, entered, N, into, the, calculator], State, _) ->
    {ok, enter(State, list_to_integer(atom_to_list(N)))}.

'when'([i, press, Op], State, _) ->
    {ok, press(State, Op)}.

then([the, result, should, be, Result, on, the, screen],
     State, _) ->
    list_to_integer(atom_to_list(Result)) =:=State.

teardown(_State) ->
    ok.

%% Implementing a simple model here...

enter(State, N) ->
    [N|State].

press(State, add) ->
    add(State);
press(State, multiply) ->
    multiply(State).

add([X, Y]) ->
    X + Y.

multiply([X, Y]) ->
    X * Y.



%% A main() to kick it all off...

main() ->
    cucumberl:run("./features/simple_sample.feature").

