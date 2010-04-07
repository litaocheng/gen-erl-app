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
-vsn('0.1').

-export([start/0,
	 init/0,
	 process/1,
	 register_commands/3,
	 register_commands/4,
	 unregister_commands/3,
	 unregister_commands/4]).

-include("@APP@_ctl.hrl").

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
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
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
    ?PRINT("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(@APP@, 1, application:which_applications()) of
        false ->
            ?PRINT("node is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            ?PRINT("node is running~n", []),
            ?STATUS_SUCCESS
    end;

process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["mnesia"]) ->
    ?PRINT("~p~n", [mnesia:system_info(all)]),
    ?STATUS_SUCCESS;

process(["mnesia", "info"]) ->
    mnesia:info(),
    ?STATUS_SUCCESS;

process(["mnesia", Arg]) when is_list(Arg) ->
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
	Return -> ?PRINT("~p~n", [Return])
    end,
    ?STATUS_SUCCESS;

process(Args) ->
    case node_hooks:run_fold(node_ctl_process, false, [Args]) of
	false ->
	    print_usage(),
	    ?STATUS_USAGE;
	Status ->
	    Status
    end.


print_usage() ->
    CmdDescs =
	[{"status", "get node status"},
	 {"stop", "stop node"},
	 {"restart", "restart node"},
	 {"mnesia [info]", "show information of Mnesia system"}
	 ] ++
	ets:tab2list(node_ctl_cmds),
    MaxCmdLen =
	lists:max(lists:map(
		    fun({Cmd, _Desc}) ->
			    length(Cmd)
		    end, CmdDescs)),
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: @APP@ctl [--node nodename] command [options]~n"
      "~n"
      "Available commands in this node node:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  @APP@ctl start~n"
      "  @APP@ctl debug~n"
      "  @APP@ctl stop~n"
      "  @APP@ctl restart~n"
      "  @APP@ctl --node node@host restart~n",
     []).

register_commands(CmdDescs, Module, Function) ->
    ets:insert(node_ctl_cmds, CmdDescs),
    node_hooks:add(node_ctl_process,
		       Module, Function, 50),
    ok.

register_commands(Host, CmdDescs, Module, Function) ->
    ets:insert(node_ctl_host_cmds,
	       [{{Host, Cmd}, Desc} || {Cmd, Desc} <- CmdDescs]),
    node_hooks:add(node_ctl_process, Host,
		       Module, Function, 50),
    ok.

unregister_commands(CmdDescs, Module, Function) ->
    lists:foreach(fun(CmdDesc) ->
			  ets:delete_object(node_ctl_cmds, CmdDesc)
		  end, CmdDescs),
    node_hooks:delete(node_ctl_process,
			  Module, Function, 50),
    ok.

unregister_commands(Host, CmdDescs, Module, Function) ->
    lists:foreach(fun({Cmd, Desc}) ->
			  ets:delete_object(node_ctl_host_cmds,
					    {{Host, Cmd}, Desc})
		  end, CmdDescs),
    node_hooks:delete(node_ctl_process,
			  Module, Function, 50),
    ok.
