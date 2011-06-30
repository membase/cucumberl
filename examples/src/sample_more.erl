-module(sample_more).

-compile(export_all).

% A way to reference other step modules when running from the command line.
additional_steps() -> put(calculator, []), [sample].

% Step definitions for the sample calculator Multiplication feature.

step(['when', i, press, multiply], _) ->
    [X, Y] = get(calculator),
    Result = X * Y,
    put(calculator, [Result]),
    Result;

step(_, _) -> undefined.

% A main() to kick it all off...

main() ->
    Modules = [sample_more, sample],
    put(calculator, []),
    cucumberl:run("./features/sample_more.feature", Modules),
    put(calculator, []),
    cucumberl:run("./features/sample.feature", Modules).

