%%%----------------------------------------------------------------------
%%%
%%% @copyright @COPYRIGHT@
%%%
%%% @author @AUTHOR@ <@MAIL@>
%%% @doc the control module
%%% @end
%%%
%%%----------------------------------------------------------------------

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

