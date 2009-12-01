# cucumberl

A pure-erlang implementation of Cucumber (http://cukes.info).  This
provides a subset of the Cucumber feature definition language.

## Quick Start

You'll need erlang, of course.

To do a build, do...

    make

To run unit tests, do...

    make test

There's a sample feature file (features/sample.feature) and step
definition (in src/sample.erl).  You can try them out like this:

    make sample

## Slow Start

So you want to write your own steps?  No problem.  Any module that
implements step definitions should export a step/2 function.

  step(TokenList, Info)

The TokenList is a list of either atoms or strings, such as...

  [given, i, have, entered, "Joe Armstrong", into, the, name, field]

The Info parameter is a tuple of helpful debug information, such as
the {LineText, LineNum}, of what cucumberl is currently processing.

Here's how you'd write a few step defintion functions, using erlang's
pattern matching.

  step([given, i, have, entered, N, into, the, calculator], _Info) ->
      % Your step implementation here.
      todo;
  step(['when', i, press, add], _) ->
      % Your step implementation here.
      todo;
  step(['then', the, result, should, be, Result, on, the, screen], _) ->
      % Your step implementation here.
      todo.

Notice that all the tokens have been atomized (and turned lowercase).
The above step definitions will match the parts of a scenario like...

  Scenario: Add two numbers
    Given I have entered 50 into the calculator
    And I have entered 70 into the calculator
    When I press add
    Then the result should be 120 on the screen

  cucumberl:run("./features/sample.feature", StepDefinitionModules).

The StepDefinitionModules is a list of modules that define step/2
callbacks.  For example...

  cucumberl:run("./features/sample.feature", [sample]).

## License

MIT - We made this for you!

