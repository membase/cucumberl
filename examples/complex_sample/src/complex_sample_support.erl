-module(complex_sample_support).

-export([given/2, 'when'/2, then/2, enter/2, press/2]).

%% Step definitions for the sample calculator Addition feature.

given([i, have, entered, N, into, the, calculator], State) ->
    {ok, enter(State, list_to_integer(atom_to_list(N)))};
given([i, have, cleared, the, calculator], _) ->
    {ok, []}.

'when'([i, press, Op], State) ->
    {ok, press(State, Op)}.

then([the, result, should, be, Result, on, the, screen], State) ->
    list_to_integer(atom_to_list(Result)) =:= State.

%% Implementing a simple model here...

enter(State, N) ->
    [N|State].

press([X, Y], add) ->
    X + Y;
press([X, Y], multiply) ->
    X * Y.



