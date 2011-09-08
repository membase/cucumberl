-module(complex_sample_more).

-export([setup/0,
         given/3, 'when'/3, then/3, main/0]).

setup() ->
    [].

%% Step definitions for the sample calculator Multiplication feature.
given(Step, State, _) ->
    complex_sample_support:given(Step, State).

'when'(Step, State, _) ->
    complex_sample_support:'when'(Step, State).

then(Step, State, _) ->
    complex_sample_support:then(Step, State).

%% A main() to kick it all off...

main() ->
    cucumberl:run("./features/complex_sample_more.feature"),
    cucumberl:run("./features/complex_sample.feature", ?MODULE).

