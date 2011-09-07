# cucumberl

A pure-erlang, open-source, single-file implementation of Cucumber
(http://cukes.info).  This provides a subset of the Cucumber feature
definition language.

## Quick Start

You'll need erlang, of course.

To do a build, do...

    make

To run unit tests, do...

    make test

There's are sample feature files (examples/features) and step definitions (in examples/src). Running `make test` will execute these too.

You can also run them by hand, for example...

    examples $ ../cucumberl
    Feature: Addition                                                :1
      In order to avoid silly mistakes                               :2
      As a math idiot                                                :3
      I want to be told the sum of two numbers                       :4
                                                                     :5
      Scenario: Add two numbers                                      :6
        Given I have entered 50 into the calculator                  :7
        And I have entered 70 into the calculator                    :8
        When I press add                                             :9
        Then the result should be 120 on the screen                  :10   ok
                                                                     :11

    etc.....

## Slow Start

So you want to write your own step definitions?  No problem.  Any
erlang module that implements step definitions should export a step/2
function, with this kind of call signature...

    step(TokenList, Info)

The TokenList parameter is a list of either atoms or strings, such as...

    [given, i, have, entered, "Joe Armstrong", into, the, authors, field]

You can also export functions to explicitly deal with the `given`, `when` and
`then` tokens. So for example, the previous TokenList would also be accepted in
a function definition like this:

    given([i, have, entered, Name, into, the, authors, field], _) -> ok.

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

In general, your "real" step definitions should return anything but
undefined, with the following caveats:

- The atoms `true` and `ok` represent *success* and print *ok* on the console
- A two-tuple of the form `{failed, Reason}` indicates failure
- The atom `undefined` essentially means, "carry on" (i.e., it is ignored)

The above step definitions will match a scenario like the following...

    Scenario: Add two numbers
      Given I have entered 50 into the calculator
      And I have entered 70 into the calculator
      When I press add
      Then the result should be 120 on the screen

## Running cucumberl

Running cucumberl on the command line is very simple. Just execute the cucumberl
self-contained escript.

To run a feature file through cucumberl using the erlang API...

    cucumberl:run(PathToFeatureFile, FeatureDefinitionModule).

For example...

    cucumberl:run("./features/sample.feature", FeatureDefinitionModule).

The FeatureDefinitionModule parameter is an optional module that implements te feature and contains step/2 callbacks.  For example...

    cucumberl:run("./features/auction.feature",
    		   auction_step_definitions).

If it is not provided, then the a module by the same name as the
feature is assumed to implement the feature.

    cucumberl:run("./features/auction.feature").

is equivalent to

    cucumberl:run("./features/auction.feature", auction).

## Scenario Outlines

There's basic support for Scenario Outlines, aka Example Tables, in
cucumberl.  However, placeholders names should be all lowercase, and
there shouldn't be any blank lines before the "Examples:" label.  For
example...

    Scenario Outline:
      Given I have cleared the calculator
      And I have entered <a> into the calculator
      And I have entered <b> into the calculator
      When I press <op>
      Then the result should be <ab> on the screen
      Examples:
        |  a | b | ab | op       |
        |  1 | 1 | 2  | add      |
        |  1 | 3 | 3  | multiply |
        |  2 | 3 | 6  | multiply |
        | 10 | 1 | 11 | add      |

See the files examples/sample/src/sample_table.erl and
examples/sample/features/sample_table.feature for more details.

## It fits in one file!

To use cucumberl in your own work, you can just copy src/cucumber.erl
to your own erlang project, as it's fully self-contained. You can also use the
`cucumberl` escript which is generated when you build the project.

## License

MIT - We made this for you!

## Feedback, or getting in touch

Improvements and patches welcomed -- info@northscale.com

Cheers,
Steve Yen


