-module(cucumberl_cli).

-export([main/1]).

%% TODO: introduce command line arguments to control things like 
%%      (a) features directory
%%      (b) extra sources/directories to add to code:path
%%      (c) choice of feature(s) to be run
%%      (d) support for code coverage....

main(_) ->
    code:add_pathz("ebin"),
    Features = find_files("features", ".*\\.feature\$"),
    [ run_feature(F) || F <- Features ].

run_feature(FeatureFile) ->
    %% is there a mod *named* for this feature?
    StepMod = list_to_atom(filename:basename(FeatureFile, ".feature")),
    AllStepMods = ensure_loaded([StepMod|step_helpers()]),
    cucumberl:run(FeatureFile, AllStepMods).

ensure_loaded([]) -> [];
ensure_loaded([Mod|Rest]) ->
    [ensure_loaded(Mod)|ensure_loaded(Rest)];
ensure_loaded(Mod) when is_atom(Mod) ->
    case code:ensure_loaded(Mod) of
        {error, _}=E ->
            throw(E);
        _ -> 
            Mod
    end.

step_helpers() ->
    %% TODO: this is a very crude way of loading multiple step modules...
    [ M || {M, P} <- code:all_loaded(), 
            P /= preloaded andalso string:str(P, "step_helper") > 0 ].

find_files(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F | Acc] end, []).
