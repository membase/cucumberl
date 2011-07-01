-module(cucumberl).

-include_lib("eunit/include/eunit.hrl").
-include("cucumberl.hrl").
-compile(export_all).

main(Args) ->
    cucumberl_cli:main(Args).

% Cucumber parser & driver in erlang, in a single file,
% implementing a subset of the cucumber/gherkin DSL.
%
% Example step implementation pattern in erlang...
%
%   step([given, i, have, entered, N, into, the, calculator], _Info) ->
%       % Your step implementation here.
%       anything_but_undefined.
%
% The Info is a {Line, LineNum} tuple.
%
% Example run...
%
%   cucumberl:run("./features/sample.feature").
%
run(FilePath)              -> run(FilePath, []).
run(FilePath, StepModules) -> run(FilePath, StepModules, 1).
run(FilePath, StepModules, LineNumStart) ->
    run_lines(lines(FilePath), StepModules, LineNumStart).

run_lines(Lines, StepModules, LineNumStart) ->
    StepModulesEx = resolve_modules(StepModules) ++ [?MODULE],
    NumberedLines = numbered_lines(Lines),
    ExpandedLines = expanded_lines(NumberedLines),
    {_, _, #cucumberl_stats{scenarios = NScenarios,
                            steps = NSteps} = Stats} =
        lists:foldl(
          fun ({LineNum, _Line} = LNL,
               {Section, GWT, Stats} = Acc) ->
              case LineNum >= LineNumStart of
                  true  -> process_line(LNL, Acc, StepModulesEx);
                  false -> {Section, GWT, Stats}
              end
          end,
          {undefined, undefined, #cucumberl_stats{}}, ExpandedLines),
    io:format("~n~p scenarios~n~p steps~n~n",
              [NScenarios, NSteps]),
    {ok, Stats}.

resolve_modules([]) -> [];
resolve_modules([Mod|Rest]) ->
    case erlang:function_exported(Mod, additional_steps, 0) of
        true ->
            [Mod|Mod:additional_steps()] ++ resolve_modules(Rest);
        false ->
            [Mod|resolve_modules(Rest)]
    end.

expanded_lines(NumberedLines) ->
    % Expand "Scenario Outlines" or tables.
    {_, _, ExpandedLines} =
        lists:foldl(
          fun({_LineNum, Line} = LNL,
              {LastScenarioOutline, RowHeader, Out}) ->
             case {LastScenarioOutline, RowHeader, string_to_atoms(Line)} of
                 {undefined, _, ['scenario', 'outline:' | _]} ->
                     {[LNL], undefined, Out};
                 {undefined, _, _} ->
                     {undefined, undefined, [LNL | Out]};
                 {LSO, _, ['examples:' | _]} ->
                     {lists:reverse(LSO), undefined, Out};
                 {LSO, undefined, ['|' | _] = Row} ->
                     {LSO, evens(Row), Out};
                 {LSO, _, ['|' | _] = Row} ->
                     ESO = lists:reverse(
                             expand_scenario_outline(LSO, RowHeader,
                                                     evens(Row))),
                     {LSO, RowHeader, ESO ++ Out};
                 {_, _, []} ->
                     {undefined, undefined, [LNL | Out]};
                 {LSO, _, _} ->
                     {[LNL | LSO], RowHeader, Out}
             end
          end,
          {undefined, undefined, []},
          NumberedLines),
    lists:reverse(ExpandedLines).

expand_scenario_outline(ScenarioLines, RowHeader, RowTokens) ->
    KeyValList = lists:zip(RowHeader, RowTokens),
    lists:map(fun ({LineNum, Line}) ->
                  {Strs, Placeholders} =
                      unzip_odd_even(string:tokens(Line, "<>")),
                  Replacements =
                      lists:map(
                        fun (Placeholder) ->
                            K = list_to_atom(Placeholder),
                            case lists:keysearch(K, 1, KeyValList) of
                                {value, {K, Val}} -> atom_to_list(Val)
                            end
                        end,
                        Placeholders),
                  Line2 =
                      lists:foldl(fun (X, Acc) -> Acc ++ X end,
                                  "", zip_odd_even(Strs, Replacements)),
                  {LineNum, Line2}
              end,
              ScenarioLines).

process_line({LineNum, Line},
             {Section, GWT, #cucumberl_stats{scenarios = NScenarios,
                                             steps = NSteps,
                                             failures = FailedSoFar } = Stats},
             StepModules) ->
    % GWT stands for given-when-then.
    % GWT is the previous line's given-when-then atom.
    io:format("~s:~s ",
              [string:left(Line, 65),
               string:left(integer_to_list(LineNum), 4)]),

    % Handle quoted sections by spliting by "\"" first.
    {TokenStrs, QuotedStrs} =
        unzip_odd_even(string:tokens(Line, "\"")),

    % Atomize the unquoted sections.
    TokenAtoms = lists:map(fun string_to_atoms/1, TokenStrs),

    % Zip it back together into a Tokens list that might look like...
    %   [given, i, have, entered, "Joe Armstrong", as, my, name]
    % or
    %   ['when', i, have, installed, erlang]
    % or
    %   ['then', i, should, see, someone, calling, me]
    %
    % Some atoms are reserved words in erlang ('when', 'if', 'then')
    % and need single quoting.
    %
    Tokens = flat_zip_odd_even(TokenAtoms, QuotedStrs),

    % Run through the StepModule steps, only if we are in a scenario
    % section, otherwise, skip the line.
    {Section2, GWT2, Result, Stats2} =
        case {Section, Tokens} of
            {_, ['scenario:' | _]} ->
                {scenario, undefined, ok,
                 Stats#cucumberl_stats{scenarios = NScenarios + 1}};
            {_, ['scenario', 'outline:' | _]} ->
                {scenario, undefined, ok,
                 Stats#cucumberl_stats{scenarios = NScenarios + 1}};
            {_, []} ->
                {undefined, undefined, ok, Stats};
            {undefined, _} ->
                {undefined, undefined, ok, Stats};
            {scenario, ['#' | _]} ->
                {Section, GWT, ok, Stats};
            {scenario, [TokensHead | TokensTail]} ->
                G = case {GWT, TokensHead} of
                        {undefined, _}    -> TokensHead;
                        {_, 'and'}        -> GWT;
                        {GWT, TokensHead} -> TokensHead
                    end,
                R = lists:foldl(
                        fun (StepModule, undefined) ->
                            try
                                case erlang:function_exported(StepModule, G, 2) of
                                    true ->
                                        apply(StepModule, G, [TokensTail,
                                                              {Line, LineNum}]);
                                    false ->
                                        StepModule:step([G | TokensTail],
                                                        {Line, LineNum})
                                end
                            catch
                                error:function_clause -> 
                                    %% we don't have a matching function clause
                                    undefined;
                                Err:Reason -> 
                                    %% something else went wrong, which means fail
                                    {failed, {Err, Reason}}
                            end;
                          (_, Acc) -> Acc
                      end,
                      undefined, StepModules),
                {Section, G, R, Stats#cucumberl_stats{steps = NSteps + 1}}
        end,

    % Emit result and our accumulator for our calling foldl.
    case {Section2, Result} of
        {scenario, Result} ->
            case check_step(Result) of
                passed ->
                    io:format("ok~n"),
                    {Section2, GWT2, Stats2};
                missing ->
                    io:format("NO-STEP~n~n"),
                    io:format("a step definition snippet...~n"),
                    io:format("step(~p, _) ->~n  undefined.~n~n", [Tokens]),
                    {undefined, undefined, Stats2};
                failed ->
                    io:format("FAIL ~n"),
                    {Section2, GWT2,
                     Stats2#cucumberl_stats{ failures = [Result|FailedSoFar] }};
                ignored ->
                    io:format("~n"),
                    {Section2, GWT2, Stats2}
            end;
        _ ->
            %% TODO: is this an error case - should it fail when this happens?
            io:format("~n"),
            {Section2, GWT2, Stats2}
    end.

check_step(true)        -> passed;
check_step(ok)          -> passed;
check_step(undefined)   -> missing;
check_step(false)       -> failed;
check_step({failed, _}) -> failed;
check_step(_)           -> ignored.

step(['feature:' | _], _Line)  -> true;
step(['scenario:' | _], _Line) -> true;
step([], _) -> true;
step(_, _)  -> undefined.

numbered_lines(Lines) ->
    NLines = length(Lines),
    lists:zip(lists:seq(1, NLines, 1), Lines).

lines(FilePath) ->
    case file:read_file(FilePath) of
        {ok, FB} -> lines(binary_to_list(FB), [], []);
        Err -> io:format("error: could not open file ~p~n", [FilePath]),
               exit(Err)
    end.

lines([], CurrLine, Lines) ->
    lists:reverse([lists:reverse(CurrLine) | Lines]);
lines([$\n | Rest], CurrLine, Lines) ->
    lines(Rest, [], [lists:reverse(CurrLine) | Lines]);
lines([X | Rest], CurrLine, Lines) ->
    lines(Rest, [X | CurrLine], Lines).

% This flat_zip_odd_even() also does flattening of Odds,
% since each Odd might be a list of atoms.

flat_zip_odd_even(Odds, Evens) ->
    zip_odd_even(flat, Odds, Evens, 1, []).

zip_odd_even(Odds, Evens) ->
    zip_odd_even(reg, Odds, Evens, 1, []).

zip_odd_even(_, [], [], _F, Acc) ->
    lists:reverse(Acc);
zip_odd_even(K, [], [Even | Evens], F, Acc) ->
    zip_odd_even(K, [], Evens, F, [Even | Acc]);

zip_odd_even(reg, [Odd | Odds], [], F, Acc) ->
    zip_odd_even(reg, Odds, [], F, [Odd | Acc]);
zip_odd_even(flat, [Odd | Odds], [], F, Acc) ->
    zip_odd_even(flat, Odds, [], F, lists:reverse(Odd) ++ Acc);

zip_odd_even(reg, [Odd | Odds], Evens, 1, Acc) ->
    zip_odd_even(reg, Odds, Evens, 0, [Odd | Acc]);
zip_odd_even(flat, [Odd | Odds], Evens, 1, Acc) ->
    zip_odd_even(flat, Odds, Evens, 0, lists:reverse(Odd) ++ Acc);

zip_odd_even(K, Odds, [Even | Evens], 0, Acc) ->
    zip_odd_even(K, Odds, Evens, 1, [Even | Acc]).

unzip_odd_even(Tokens) ->
    {Odds, Evens, _F} =
        lists:foldl(fun (X, {Odds, Evens, F}) ->
                        case F of
                            1 -> {[X | Odds], Evens, 0};
                            0 -> {Odds, [X | Evens], 1}
                        end
                    end,
                    {[], [], 1}, Tokens),
    {lists:reverse(Odds), lists:reverse(Evens)}.

evens(L) ->
    {_Odds, Evens} = unzip_odd_even(L),
    Evens.

string_to_atoms(StrWords) ->
    lists:map(fun (Y) -> list_to_atom(string:to_lower(Y)) end,
              string:tokens(StrWords, " ")).

% ------------------------------------

unzip_test() ->
    ?assertMatch({[], []}, unzip_odd_even([])),
    ?assertMatch({[1], []}, unzip_odd_even([1])),
    ?assertMatch({[1], [2]}, unzip_odd_even([1, 2])),
    ?assertMatch({[1, 3], [2]}, unzip_odd_even([1, 2, 3])),
    ?assertMatch({[1, 3, 5], [2, 4, 6]},
                 unzip_odd_even([1, 2, 3, 4, 5, 6])).

zip_test() ->
    ?assertMatch([1, 2, 3, 4, 5, 6],
                 zip_odd_even([1, 3, 5], [2, 4, 6])),
    ?assertMatch([1, 2, 3, 4, 5, 6],
                 flat_zip_odd_even([[1], [3], [5]], [2, 4, 6])).

string_to_atoms_test() ->
    ?assertMatch([], string_to_atoms("")),
    ?assertMatch([a, bb, ccc],
                 string_to_atoms("a bb ccc")),
    ?assertMatch([a, bb, ccc],
                 string_to_atoms("  a  bb   ccc  ")).
