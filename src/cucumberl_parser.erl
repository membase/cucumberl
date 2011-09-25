-module(cucumberl_parser).

-include_lib("eunit/include/eunit.hrl").
-include("cucumberl.hrl").

-export([parse/1]).

parse(FilePath) ->
    StepMod = list_to_atom(filename:basename(FilePath, ".feature")),
    {StepMod, process_lines(lines(FilePath))}.

process_lines(Lines) ->
    NumberedLines = numbered_lines(Lines),
    {Tree, _} =
        lists:foldl(fun process_line/2,
                    {[], {undefined, undefined}},
                    expanded_lines(NumberedLines)),
    lists:reverse(Tree).


expanded_lines(NumberedLines) ->
    %% Expand "Scenario Outlines" or tables.
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

process_line({LineNum, Line}, {Acc,  {Section0, GWT0}}) ->
    %% GWT stands for given-when-then.
    %% GWT is the previous line's given-when-then atom.

    %% Handle quoted sections by spliting by "\"" first.
    {TokenStrs, QuotedStrs} =
        unzip_odd_even(string:tokens(Line, "\"")),

    %% Atomize the unquoted sections.
    TokenAtoms = lists:map(fun string_to_atoms/1, TokenStrs),

    %% Zip it back together into a Tokens list that might look like...
    %%   [given, i, have, entered, "Joe Armstrong", as, my, name]
    %% or
    %%   ['when', i, have, installed, erlang]
    %% or
    %%   ['then', i, should, see, someone, calling, me]
    %%
    %% Some atoms are reserved words in erlang ('when', 'if', 'then')
    %% and need single quoting.
    %%
    Tokens = flat_zip_odd_even(TokenAtoms, QuotedStrs),

    %% Run through the FeatureModule steps, only if we are in a scenario
    %% section, otherwise, skip the line.
    {Parsed, Section1, GWT1} =
        case {Section0, Tokens} of
            {_, ['feature:' | _]} ->
                {{feature, LineNum, Tokens, Line}, undefined, GWT0};
            {_, ['scenario:' | _]} ->
                {{scenario, LineNum, Tokens, Line}, scenario, GWT0};
            {_, ['scenario', 'outline:' | _]} ->
                {{scenario_outline, LineNum, Tokens, Line},
                 scenario, GWT0};
            {_, []} ->
                {{desc, LineNum, Tokens, Line}, undefined, GWT0};
            {undefined, _} ->
                {{desc, LineNum, Tokens, Line}, undefined, GWT0};
            {scenario, ['#' | _]} ->
                {{desc, LineNum, Tokens, Line}, Section0, GWT0};
            {scenario, [TokensHead | TokensTail]} ->
                G = case {GWT0, TokensHead} of
                        {undefined, _}    -> TokensHead;
                        {_, 'and'}        -> GWT0;
                        {GWT0, TokensHead} -> TokensHead
                    end,
                {{{action, G}, LineNum, TokensTail, Line}, Section0, G}
        end,
    {[Parsed | Acc], {Section1, GWT1}}.


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

%% This flat_zip_odd_even() also does flattening of Odds,
%% since each Odd might be a list of atoms.

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

%% ------------------------------------

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
