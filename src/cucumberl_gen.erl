-module(cucumberl_gen).

-include_lib("eunit/include/eunit.hrl").
-include("cucumberl.hrl").

-export([gen/2, gen/3]).

gen(FeaturePath, OutputPath)
  when is_list(FeaturePath), is_list(OutputPath) ->
    {FeatureModule, Tree} = cucumberl_parser:parse(FeaturePath),
    gen_tree(OutputPath, Tree, FeatureModule).

gen(FeaturePath, OutputPath, ImplementationModule)
  when is_list(FeaturePath), is_list(OutputPath) ->
    {_FeatureModule, Tree} = cucumberl_parser:parse(FeaturePath),
    gen_tree(OutputPath, Tree, ImplementationModule).

gen_tree(OutputPath, Tree, FeatureModule) ->
    TargetPath = filename:join([OutputPath,
                                atom_to_list(FeatureModule) ++ ".erl"]),
    Dict = lists:foldl(fun(Entry, Acc) ->
                               process_line(Entry, Acc)
                       end,
                       dict:new(),
                       Tree),
    Given = process_clauses(given,
                            lists:reverse(sets:to_list(dict:fetch(given, Dict)))),
    When = process_clauses('when',
                           lists:reverse(sets:to_list(dict:fetch('when', Dict)))),
    Then = process_clauses(then,
                           lists:reverse(sets:to_list(dict:fetch(then, Dict)))),

    file:write_file(TargetPath,
                    io_lib:format("-module(~s).~n~n"
                                  "-export([given/3, 'when'/3, then/3]).~n~n"
                                  "~s~n~n"
                                  "~s~n~n"
                                  "~s~n~n",
                    [atom_to_list(FeatureModule), Given,
                     When, Then])).

process_line({Type, _, Tokens, _}, Dict0) ->

    %% Run through the FeatureModule steps, only if we are in a scenario
    %% section, otherwise, skip the line.
    case Type of
        {action, G} ->
            add_to_dict(G, Tokens, Dict0);
        _ ->
            Dict0
    end.

format_step('when', Tokens) ->
    io_lib:format("'when'(~p, State, _) ->~n  undefined", [Tokens]);
format_step(GWT, Tokens) ->
    io_lib:format("~p(~p, State, _) ->~n  undefined", [GWT, Tokens]).


add_to_dict(Key, Value, Dict) ->
    case dict:find(Key, Dict) of
        {ok, OldValue} ->
            dict:store(Key, sets:add_element(Value, OldValue), Dict);
        error ->
            dict:store(Key, sets:from_list([Value]), Dict)
    end.

process_clauses(G, [Clause | Rest]) ->
        process_clauses(G, Rest, [format_step(G, Clause)]).

process_clauses(G, [Clause | Rest], Acc) ->
    process_clauses(G, Rest, [format_step(G, Clause), ";\n" | Acc]);
process_clauses(_G, [], Acc) ->
    lists:reverse(["." | Acc]).


