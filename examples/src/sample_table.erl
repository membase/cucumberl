-module(sample_table).

-compile(export_all).

% A way to reference other step modules when running from the command line.
additional_steps() -> put(calculator, []), [sample_more, sample].

% Step definitions for the sample calculator Multiplication feature.

step([given, i, have, cleared, the, calculator], _) ->
    put(calculator, []),
    ok;

step(_, _) -> undefined.

% A main() to kick it all off...

main() ->
    StepDefinitionModules = [sample_table, sample_more, sample],
    put(calculator, []),
    cucumberl:run("./features/sample_table.feature",
                  StepDefinitionModules).

