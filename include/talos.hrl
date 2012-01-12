
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(ABORT(Str, Args), lager:error(Str, Args), halt(1)).
