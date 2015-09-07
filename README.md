# cucumberl

A pure-erlang, open-source, implementation of Cucumber
(http://cukes.info).  This provides a subset of the Cucumber feature
definition language.

## Quick Start

You'll need erlang, of course.

To do a build, do...

    ./rebar3 compile

To run unit tests, do...

    ./rebar3 eunit

There's are sample feature files (examples/complex_sample/features and
examples/complex_sample/features) and step definitions (in
examples/src). Running `make test` will execute these too.

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

    Action(TokenList, State, Info)

Where Action is:

- given
- 'when'
- then

The TokenList parameter is a list of either atoms or strings, such as...

    [i, have, entered, "Joe Armstrong", into, the, authors, field]

So for example, the previous TokenList would also be accepted in
a function definition like this:

    given([i, have, entered, Name, into, the, authors, field], State, _) ->
        {ok, NewState}.

The State parameter is the state the last step function returned in the state field of the tuple. In the above example, this is NewState.

The Info parameter is a tuple of helpful debug information, such as
the {LineText, LineNum}, of what cucumberl is currently processing.
The Info parameter is usually ignored unless you're deep into
debugging your scenario/steps.

Here's how you'd write a few step definition functions, using erlang's
pattern matching.

    given([i, have, entered, N, into, the, calculator], _State, _Info) ->
      % Your step implementation here.
      todo.

    'when'([, i, press, add], _, _) ->
      % Your step implementation here.
      todo.

    then([the, result, should, be, Result, on, the, screen], _, _) ->
      % Your step implementation here.
      todo.

Notice that all the tokens have been atomized (and turned lowercase).

- The atoms `true` and `ok` in the state tuple represent *success* and
  print *ok* on the console
- A two-tuple of the form `{failed, Reason}` indicates failure

The above step definitions will match a scenario like the following...

    Scenario: Add two numbers
      Given I have entered 50 into the calculator
      And I have entered 70 into the calculator
      When I press add
      Then the result should be 120 on the screen

## Running cucumberl

Running cucumberl on the command line is very simple. Just execute the
cucumberl self-contained escript.

To run a feature file through cucumberl using the erlang API...

    cucumberl:run(PathToFeatureFile).

For example...

    cucumberl:run("./features/sample.feature").

or

    cucumberl:run("./features/sample.feature", FeatureDefinitionModule).

The FeatureDefinitionModule parameter is an optional module that
implements the feature and contains the step callbacks.  However, it is
only needed when the name of the step implementation is different then
the name of the feature. For example...

    cucumberl:run("./features/auction.feature",
                   auction).

is exactly equivalent to

    cucumberl:run("./features/auction.feature").

However, you may want to implement the feature in a different module,
such as ...

    cucumberl:run("./features/auction.feature", some_other_module).

perfectly acceptable but not recommended.

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

See the files examples/simple_sample/src/simple_sample_table.erl and
examples/simple_sample/features/simple_sample_table.feature for more details.

## License

MIT - We made this for you!

## Feedback, or getting in touch

Improvements and patches welcomed -- info@northscale.com

Cheers,
Steve Yen
