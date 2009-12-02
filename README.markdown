# cucumberl

A pure-erlang, open-soruce, single-file implementation of Cucumber
(http://cukes.info).  This provides a subset of the Cucumber feature
definition language.

## Quick Start

You'll need erlang, of course.

To do a build, do...

    make

To run unit tests, do...

    make test

There's a sample feature file (features/sample.feature) and step
definition (in src/sample.erl).  You can try them out like this:

    make sample

For example, here's a sample run...

    $ make sample
    erl -pa ebin -noshell -s sample main -s init stop
    Feature: Addition                                                :1
      In order to avoid silly mistakes                               :2
      As a math idiot                                                :3
      I want to be told the sum of two numbers                       :4
                                                                     :5
      Scenario: Add two numbers                                      :6
        Given I have entered 50 into the calculator                  :7    ok
        And I have entered 70 into the calculator                    :8    ok
        When I press add                                             :9    ok
        Then the result should be 120 on the screen                  :10   ok

## Slow Start

So you want to write your own step definitions?  No problem.  Any
erlang module that implements step definitions should export a step/2
function, with this kind of call signature...

    step(TokenList, Info)

The TokenList parameter is a list of either atoms or strings, such as...

    [given, i, have, entered, "Joe Armstrong", into, the, authors, field]

The Info parameter is a tuple of helpful debug information, such as
the {LineText, LineNum}, of what cucumberl is currently processing.
The Info parameter is usually ignored unless you're deep into
debugging your scenario/steps.

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
      todo;
    step(_, _) -> undefined.

Notice that all the tokens have been atomized (and turned lowercase).

Also, we must have a last step definition that returns undefined,
so that cucumberl can keep matching against every module that
you provide in your StepDefinitionModules list (see below).

In general, your "real" step definitions return anything but
undefined.

The above step definitions will match a scenario like the following...

    Scenario: Add two numbers
      Given I have entered 50 into the calculator
      And I have entered 70 into the calculator
      When I press add
      Then the result should be 120 on the screen

To run a feature file through cucumberl, do...

    cucumberl:run("./features/sample.feature", StepDefinitionModules).

The StepDefinitionModules parameter is a list of modules that define
step/2 callbacks.  For example...

    cucumberl:run("./features/sample.feature", [sample]).

To use this in your own work, you can just copy src/cucumber.erl to
your own project, as it's fully self-contained.

## License

MIT - We made this for you!


