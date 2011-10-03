-module(cucumberl).

-include_lib("eunit/include/eunit.hrl").
-include("cucumberl.hrl").

-export([main/1, run/1, run/2]).

main(Args) ->
    cucumberl_cli:main(Args).

%% Cucumber parser & driver in erlang, in a single file,
%% implementing a subset of the cucumber/gherkin DSL.
%%
%% Example step implementation pattern in erlang...
%%
%%   step([given, i, have, entered, N, into, the, calculator], _Info) ->
%%       % Your step implementation here.
%%       anything_but_undefined.
%%
%% The Info is a {Line, LineNum} tuple.
%%
%% Example run...
%%
%%   cucumberl:run("./features/sample.feature").
%%
run(FilePath)
  when is_list(FilePath) ->
    {FeatureModule, Tree} = cucumberl_parser:parse(FilePath),
    run_tree(Tree, FeatureModule).

run(FilePath, FeatureModule)
  when is_list(FilePath), is_atom(FeatureModule) ->
    {_, Tree} = cucumberl_parser:parse(FilePath),
    run_tree(Tree, FeatureModule).

run_tree(Tree, FeatureModule) ->
    case code:ensure_loaded(FeatureModule) of
        {module, FeatureModule} ->
            ok;
        Error ->
            throw(Error)
    end,
    Result =
        try
            State = call_setup(FeatureModule),
            {_, _, Stats} =
                lists:foldl(fun(Entry, Acc) ->
                                    process_line(Entry, Acc, FeatureModule)
                            end,
                            {false, State, #cucumberl_stats{}},
                  Tree),
            call_teardown(FeatureModule, State),
            Stats
        catch
            Err:Reason ->
                %% something else went wrong, which means fail
                io:format("Feature Failed: ~p:~p ~p",
                          [Err, Reason,
                           erlang:get_stacktrace()]),
                failed
        end,
    case Result of
        #cucumberl_stats{scenarios = NScenarios,
                         steps = NSteps,
                         failures = []}  ->
            io:format("~n~p scenarios~n~p steps~n~n",
                      [NScenarios, NSteps]),
            {ok, Result};
        #cucumberl_stats{scenarios = NScenarios,
                         steps = NSteps,
                         failures = Failures}  ->
            io:format("~n~p scenarios~n~p steps~n~p failures ~n~n",
                      [NScenarios, NSteps, erlang:length(Failures)]),
            {failed, Result};
        _ ->
            failed
    end.

process_line({Type, LineNum, Tokens, Line},
             {SkipScenario, State,
              #cucumberl_stats{scenarios = NScenarios,
                               steps = NSteps,
                               failures = FailedSoFar } = Stats},
             FeatureModule) ->
    %% GWT stands for given-when-then.
    %% GWT is the previous line's given-when-then atom.
    io:format("~s:~s ~n",
              [string:left(Line, 65),
               string:left(integer_to_list(LineNum), 4)]),

    %% Run through the FeatureModule steps, only if we are in a scenario
    %% section, otherwise, skip the line.
    {SkipScenario2, Result, Stats2} =
        case {SkipScenario, Type} of
            {_, feature} ->
                {false, {ok, State}, Stats};
            {_, scenario} ->
                {false, {ok, State},
                 Stats#cucumberl_stats{scenarios = NScenarios + 1}};
            {_, scenario_outline} ->
                {false, {ok, State},
                 Stats#cucumberl_stats{scenarios = NScenarios + 1}};
            {false, {action, G}} ->
                R = try
                        apply_step(FeatureModule, G, State, Tokens,
                                   Line, LineNum)
                    catch
                        error:function_clause ->
                            %% we don't have a matching function clause
                            undefined;
                        Err:Reason ->
                            io:format("~nSTEP: ~s FAILED: ~n ~p:~p ~p~n",
                                      [Line, Err, Reason,
                                       erlang:get_stacktrace()]),
                            %% something else went wrong, which means fail
                            {failed, {Err, Reason, erlang:get_stacktrace()}}
                    end,

                {SkipScenario, R,
                 Stats#cucumberl_stats{steps = NSteps + 1}};
            {true, {action, _}} ->
                {SkipScenario, skipped,
                                Stats#cucumberl_stats{steps = NSteps + 1}};
            {_, desc} ->
                {false, {ok, State}, Stats}
        end,

    %% Emit result and our accumulator for our calling foldl.
    case {Type, Result} of
        {{action, G1}, Result} ->
            case check_step(Result) of
                {passed, PossibleState} ->
                    {SkipScenario2, PossibleState, Stats2};
                skipped ->
                    {SkipScenario2, State, Stats2};
                missing ->
                    io:format("---------NO-STEP--------~n~n"),
                    io:format("a step definition snippet...~n"),
                    format_missing_step(G1, Tokens),
                    {true, undefined, undefined, State,
                     Stats2#cucumberl_stats{failures = [{missing, G1}
                                                        | FailedSoFar] }};
                FailedResult ->
                    io:format("-------FAIL------- ~n~n"),
                    {true, State,
                     Stats2#cucumberl_stats{ failures = [{FailedResult, Result}
                                                         |FailedSoFar] }}
            end;
        _ ->
            %% TODO: is this an error case - should it fail when this happens?
            {SkipScenario, State, Stats2}
    end.

apply_step(FeatureModule, G, State, Tokens, Line, LineNum) ->
    case erlang:function_exported(FeatureModule, G, 3) of
        true ->
            apply(FeatureModule, G, [Tokens,
                                     State,
                                     {Line, LineNum}]);
        false ->
            step_undefined
    end.

check_step(true)           -> {passed, undefined};
check_step(ok)             -> {passed, undefined};
check_step({ok, State})    -> {passed, State};
check_step({true, State})  -> {passed, State};
check_step(skipped)        -> skipped;
check_step(step_undefined) -> missing;
check_step(false)          -> failed;
check_step({failed, _})    -> failed;
check_step(_)              -> invalid_result.

format_missing_step('when', Tokens) ->
    io:format("'when'(~p, State, _) ->~n  undefined.~n~n", [Tokens]);
format_missing_step(GWT, Tokens) ->
    io:format("~p(~p, State, _) ->~n  undefined.~n~n", [GWT, Tokens]).

call_setup(FeatureModule) ->
    case erlang:function_exported(FeatureModule, setup, 0) of
        true ->
            FeatureModule:setup();
        false ->
            undefined
    end.

call_teardown(FeatureModule, State) ->
    case erlang:function_exported(FeatureModule, teardown, 1) of
        true ->
            FeatureModule:teardown(State);
        false ->
            undefined
    end.
