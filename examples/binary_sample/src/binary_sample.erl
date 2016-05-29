-module(binary_sample).

-export([setup/0, teardown/1,
         given/3, 'when'/3, then/3, main/0]).

-export([enter/2, press/2]).

setup() ->
    [].

%% Step definitions for the sample calculator Addition feature.
given(<<"I have entered 50 into the calculator">>, State, _) ->
    {ok, enter(State, 50)};
given(<<"I have entered 70 into the calculator">>, State, _) ->
    {ok, enter(State, 70)}.

'when'(<<"I press add">>, State, _) ->
    {ok, press(State, add)}.

then(<<"the result should be 120 on the screen">>, State, _) ->
    120 =:= State.

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
