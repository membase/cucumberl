-module(fail).

-compile(export_all).

given([a, step, that, works], _) -> ok.

'when'([i, come, across, a, failing, step], _) -> ok.

then([cucumberl, should, show, this, as, a, failure], _) ->
    {failed, "Some reason or other...."}.

step(_, _) -> undefined.
