#!/bin/sh

# define default configuration
POLL=true
ASYNC=8
SMP=auto
ERL_MAX_PORTS=32000
ERL_PROCESSES=500000
ERL_MAX_ETS_TABLES=1400

# define default environment variables
NODE=@APP@
HOST=localhost
ERLANG_NODE=$NODE@$HOST
ERL=erl
ROOTDIR=`cd $(dirname $0); pwd`
CH_CONFIG_PATH=$ROOTDIR/etc/
LOGS_DIR=$ROOTDIR/log
MNESIA_DIR=$ROOTDIR/var/db/$NODE
RUNAPP=@APP@_app
CTLAPP=@APP@_ctl

#echo "root is $ROOTDIR \n"

# display usage
usage ()
{
    echo ""
    echo "Usage:"
    echo "@APP@ctl ACTION [OPTION]"
    echo "ACTION:"
    echo "  live   Start an node in live (interactive) mode"
    echo "  start  Start an node in daemon mode"
    echo "  status Get the status of the deamon node"
    echo "  debug  Attach an interactive Erlang shell to a running node"
    echo "  stop   Stop a deamon running node"
    echo ""
    echo "OPTION:"
    echo "  -n, --node=Nodename    Node name:$ERLANG_NODE(default)"
    echo "  -d, --logs=Dir         Memp log directory(default $LOGS_DIR)"
    echo ""
}

# parse command line parameters
while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
        --) break ;;
        --node) ERLANG_NODE=$1; shift ;;
        --config) CH_CONFIG_PATH=$1 ; shift ;;
        --logs) LOGS_DIR=$1 ; shift ;;
        --help|-h) usage; exit 0;;
        *) ARGS="$ARGS $PARAM" ;;
    esac
done

NAME=-name
[ "$ERLANG_NODE" = "${ERLANG_NODE%.*}" ] && NAME=-sname
RUNAPP="$RUNAPP start"

ERLANG_OPTS="-connect_all false +K $POLL +A $ASYNC -smp $SMP +P $ERL_PROCESSES"

# define additional environment variables
MOCHI_EBIN=`dirname $ROOTDIR`/mochiweb/ebin
ROOT_EBIN=$ROOTDIR/ebin 

EBINS="$MOCHI_EBIN $ROOT_EBIN"
#echo "ebins is " $EBINS

@APP@_SO_PATH=$ROOTDIR/priv/lib
@APP@_BIN_PATH=$ROOTDIR/priv/bin
@APP@_LOG_PATH=$LOGS_DIR/@APP@.log
SASL_LOG_PATH=$LOGS_DIR/sasl.log
DATETIME=`date "+%Y%m%d-%H%M%S"`
ERL_CRASH_DUMP=$LOGS_DIR/erl_crash_$DATETIME.dump
#ERL_INETRC=$ROOTDIR/etc/inetrc
HOME=$ROOTDIR

# export global variables
export @APP@_CONFIG_PATH
export @APP@_LOG_PATH
export @APP@_SO_PATH
export @APP@_BIN_PATH
export ERL_CRASH_DUMP
#export ERL_INETRC
export ERL_MAX_PORTS
export ERL_MAX_ETS_TABLES
export HOME

[ -d $LOGS_DIR ] || mkdir -p $LOGS_DIR

# Compatibility in ZSH
#setopt shwordsplit 2>/dev/null

# start server
start ()
{
    $ERL \
      $NAME $ERLANG_NODE \
      -noinput -detached \
      -pa $EBINS \
      -mnesia dir "\"$MNESIA_DIR\"" \
      -kernel error_logger \{file,\"$@APP@_LOG_PATH\"\} \
      -sasl sasl_error_logger \{file,\"$SASL_LOG_PATH\"\} \
      -s ${RUNAPP}\
      $ERLANG_OPTS $ARGS "$@"
}

# attach to server
debug ()
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
      -mnesia dir "\"$MNESIA_DIR\"" \
      -s ${RUNAPP} \
      $ERLANG_OPTS $ARGS "$@"
}

# common control function
ctl ()
{
    $ERL \
      $NAME @APP@_ctl \
      -noinput \
      -pa $EBINS \
      -s ${CTLAPP}  -extra $ERLANG_NODE $@
    result=$?
    case $result in
    0) :;;
    *)
        echo ""
        echo "Commands to start an node:"
        echo "  start  Start an node in daemon mode"
        echo "  debug  Attach an interactive Erlang shell to a running node"
        echo "  live   Start an node in live (interactive) mode"
        echo ""
        echo "Optional parameters when starting an node:"
        echo "  --config file      Config file:             $CH_CONFIG_PATH"
        echo "  --node nodename    node name:               $ERLANG_NODE"
        echo "";;
    esac
    return $result
}


case $ARGS in
    ' start') start;;
    ' debug') debug;;
    ' live') live;;
    *) ctl $ARGS;;
esac
