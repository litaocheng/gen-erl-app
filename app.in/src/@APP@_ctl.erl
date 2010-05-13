%%%----------------------------------------------------------------------
%%%
%%% @copyright @COPYRIGHT@
%%%
%%% @author @AUTHOR@ <@MAIL@>
%%% @doc the @APP@ ctl module
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(@APP@_ctl).
-author('litaocheng@gmail.com').
-vsn('0.2').

-export([start/0, init/0, process/1]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

start() ->
    case init:get_plain_arguments() of
    [SNode | Args]->
        %io:format("plain arguments is:~n~p", [AArgs]),
        SNode1 = case string:tokens(SNode, "@") of
        [_Node, _Server] ->
            SNode;
        _ ->
            case net_kernel:longnames() of
             true ->
                 SNode ++ "@" ++ inet_db:gethostname() ++
                      "." ++ inet_db:res_option(domain);
             false ->
                 SNode ++ "@" ++ inet_db:gethostname();
             _ ->
                 SNode
             end
        end,
        Node = list_to_atom(SNode1),
        Status = case rpc:call(Node, ?MODULE, process, [Args]) of
             {badrpc, _Reason} ->
                 %?PRINT("RPC failed on the node ~p: ~p~n", [Node, Reason]),
                 ?STATUS_BADRPC;
             S ->
                 S
             end,
        halt(Status);
    _ ->
        print_usage(),
        halt(?STATUS_USAGE)
    end.

init() ->
    ets:new(node_ctl_cmds, [named_table, set, public]),
    ets:new(node_ctl_host_cmds, [named_table, set, public]).


process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    %?PRINT("Node ~p is ~p. Status: ~p~n",
    %          [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(@APP@, 1, application:which_applications()) of
        {value,_Version} when InternalStatus =:= started,
                            ProvidedStatus =:= started ->
            %?PRINT("node is running~n", []),
            ?STATUS_SUCCESS;
        _ ->
            %?PRINT("node is not running~n", []),
            ?STATUS_ERROR
    end;

process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;
process(["usage"]) ->
    print_usage(),
    ?STATUS_SUCCESS;
process([_|_]) ->
    ?STATUS_USAGE.

print_usage() ->
    "".
