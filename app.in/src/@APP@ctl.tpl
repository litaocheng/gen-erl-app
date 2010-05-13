#! /bin/bash
ROOTDIR=`cd $(dirname $0); pwd`
CTLFILE=$(basename $0)

# define default vm configuration
POLL=true
ASYNC=8
SMP=auto
ERL_PROCESSES=500000
CONNECT_ALL=true
DATETIME=`date "+%Y%m%d-%H%M%S"`
export ERL_CRASH_DUMP=$ROOTDIR/erl_crash_$DATETIME.dump
export ERL_MAX_PORTS=32000
export ERL_MAX_ETS_TABLES=1400
export HOME=$ROOTDIR
LOGS_DIR=$HOME/log

# define default environment variables
ERL=erl
NODE=@APP@
HOST=localhost
ERLANG_NODE=$NODE@$HOST
COOKIE="node-cookie"

RUNAPP=@APP@_app
CTLAPP=@APP@_ctl
ERROR_LOG=$LOGS_DIR/@APP@.log
SASL_LOG=$LOGS_DIR/sasl.log

# define additional environment variables
ROOT_EBIN=$ROOTDIR/ebin 
EBINS="$ROOT_EBIN"
#echo "ebins is " $EBINS

# makesure the logs dir exists
if [ ! -d $LOGS_DIR ]; then
    mkdir -p $LOGS_DIR || echo "make $LOGS_DIR error!"; exit 1
fi

STATUS_SUCCESS=0
STATUS_ERROR=1
STATUS_USAGE=2
STATUS_BADRPC=3

# display usage
usage ()
{
    shell_usage; erlang_usage
}

shell_usage ()
{
    echo ""
    echo "Usage:"
    echo "@APP@ctl ACTION [OPTION]"
    echo "ACTION:"
    echo "  live    Start an node in live (interactive) mode"
    echo "  start   Start an node in daemon mode"
    echo "  status  Get the status of the deamon node"
    echo "  attach  Attach an interactive Erlang shell to a running node"
    echo "  stop    Stop a deamon running node"
    echo "  restart Restart a deamon running node"
    echo ""
    echo "OPTION:"
    echo "  -h, --help             Show this info"
    echo "  -n, --node=Nodename    Node name:$ERLANG_NODE(default)"
    echo "  -c, --cookie=Cookie    Cookie for node communication(default \"\")"
}

erlang_usage()
{
    echo ""
    echo "Commands processed by Erlang"
    echo ""
}

rpc() 
{
    $ERL \
      $NAME @APP@_ctl \
      -noinput \
      -pa $EBINS \
      -s ${CTLAPP}  -extra $ERLANG_NODE $@
}

print_rpc_msg ()
{
    case $1 in
    $STATUS_SUCCESS) 
        echo ""
        ;;
    $STATUS_ERROR) 
        echo "Warning: Process commands error!"
        ;;
    $STATUS_USAGE) 
        echo "Warning: Command NOT Supported!"
        usage
        ;;
    $STATUS_BADRPC) 
        echo "Warning: $ERLANG_NODE is not running"
        echo "Use '$CTLFILE start' to start node" 
        ;;
    *)
        echo "Warning: Unknown command!"
    esac
    return $result
}

is_started () 
{
    #echo "check if node started"
    rpc status
    result=$?
    if [  "$result" = "$STATUS_SUCCESS" ]; then
        return 0
    fi
    return 1
}

# start interactive server
live ()
{
    echo "--------------------------------------------------------------------"
    echo ""
    echo "IMPORTANT: node is going to start in LIVE (interactive) mode."
    echo "All log messages will be shown in the command shell."
    echo "You can interact with the node if you know how to use it."
    echo "Please be extremely cautious with your actions,"
    echo "and exit immediately if you are not completely sure."
    echo ""
    echo "To exit this LIVE mode and stop node, press:"
    echo "  q().  and press the Enter key"
    echo ""
    echo "--------------------------------------------------------------------"
    echo "Press any key to continue"
    read foo
    echo ""
    $ERL \
      $NAME $ERLANG_NODE \
      -pa $EBINS \
      -s ${RUNAPP} start \
      $ERLANG_OPTS $ARGS "$@"
}

# start server
start ()
{
    if is_started; then
        echo "WARNING: $ERLANG_NODE already started"
        exit 0
    fi

    $ERL \
      $NAME $ERLANG_NODE \
      -noinput -detached \
      -pa $EBINS \
      -kernel error_logger \{file,\"$ERROR_LOG\"\} \
      -sasl sasl_error_logger \{file,\"$SASL_LOG\"\} \
      -s ${RUNAPP} start\
      $ERLANG_OPTS $ARGS "$@"
    
    echo -n "Node $ERLANG_NODE start: "
    if [ $? -eq 0 ]; then
        echo "OK"
    else
        echo "Failed"
    fi
}

# get node status
status ()
{
    if rpc status; then
        echo "Node $ERLANG_NODE status: Running"
    else
        print_rpc_msg $?
    fi  
}

# attach to server
attach ()
{
    echo "--------------------------------------------------------------------"
    echo ""
    echo "IMPORTANT: we will attempt to attach an INTERACTIVE shell"
    echo "to an already running node."
    echo "If an ERROR is printed, it means the connection was not succesfull."
    echo "You can interact with the running node if you know how to use it."
    echo "Please be extremely cautious with your actions,"
    echo "and exit immediately if you are not completely sure."
    echo ""
    echo "To detach this shell from node, press:"
    echo "  control+c, control+c"
    echo ""
    echo "--------------------------------------------------------------------"
    echo "Press any key to continue"
    read foo
    echo ""
    $ERL \
      $NAME ${NODE}debug \
      -remsh $ERLANG_NODE \
      $ERLANG_OPTS $ARGS "$@"
}

# stop the node
stop ()
{
    if rpc stop; then
        echo "Node $ERLANG_NODE stop: OK"
    else
        print_rpc_msg $?
    fi  
}

# restart the node
restart ()
{
    
    if rpc restart; then
        echo "Node $ERLANG_NODE restart: OK"
    else
        print_rpc_msg $?
    fi  
}

# parse command line parameters
while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
        --) break ;;
        --node|-n) ERLANG_NODE=$1; shift ;;
        --cookie|-c) COOKIE=$1 ; shift ;;
        --help|-h) usage; exit 0;;
        *) ARGS="$ARGS $PARAM" ;;
    esac
done

NAME=-name
[ "$ERLANG_NODE" = "${ERLANG_NODE%.*}" ] && NAME=-sname

ERLANG_OPTS="-connect_all $CONNECT_ALL +K $POLL +A $ASYNC -smp $SMP +P $ERL_PROCESSES"

# Compatibility in ZSH
#setopt shwordsplit 2>/dev/null

case $ARGS in
    '') usage;;
    ' live') live;;
    ' start') start;;
    ' status') status;;
    ' attach') attach;;
    ' stop') stop;;
    ' restart') restart;;
    *) rpc $ARGS;;
esac
