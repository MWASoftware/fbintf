#!/bin/sh
usage()
{
  echo "Run UDR tests"
  echo "runtests.sh  [-3] [-4] "
}

BUILD=
#Parse Parameters
TEMP=`getopt h34db:f: "$@"`
FB="4.0.1"
if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

while true ; do
        case "$1" in
        -h)     usage; exit 1;;

        \-3) 	FB="3.0.5"; shift 1;;

        \-4) 	FB="4.0.1"; shift 1;;

	-d)	FB="master"; shift 1;;

	-f)	FB=$2; shift 2;;
        
        --)    shift; break;; 

        *)      echo "Unrecognised argument $1"; usage; exit 1;;
        
        esac
done

export FIREBIRD=/opt/firebird/$FB
echo "FIREBIRD=$FIREBIRD"
export LD_LIBRARY_PATH=$FIREBIRD/lib
ISQL=$FIREBIRD/bin/isql
if [ ! -x $ISQL ]; then
  echo "Unable to find isql utility"
  exit 1
fi

RUNISQL="$ISQL -user SYSDBA -pass masterkey localhost:employee"
TESTISQL="$ISQL -user TESTER -pass testing localhost:employee"
rm libfbudrtests.so

if [ ! -d "$FIREBIRD" ]; then
  echo "$FIREBIRD not found"
else
  rm -r testunits
  fpcmake
  make
  if [ ! -f libfbudrtests.so ]; then
    echo "Compilation failed"	  
    exit 1
  fi
  cp libfbudrtests.so $FIREBIRD/plugins/udr
  PID=`ps ax|grep $FB/bin/fbguard|grep -v grep|awk '{print $1;}'`
  if [ -n "$PID" ]; then
    sudo kill -TERM $PID
  fi
  sleep 1
  sudo echo "starting $FB/bin/fbguard"
  sudo $FIREBIRD/bin/fbguard&
  sleep 2

  echo "Running UDR Lib Testsuite" >testout.log
  
  echo "Adding SQL Definitions"
  $RUNISQL < AddDefs.sql
  for FN in `ls Test*.sql`; do
    echo "Running `basename -s .sql $FN`"
    echo "------------------------" >>testout.log
    echo "Running `basename -s .sql $FN`" >>testout.log
    echo "------------------------" >>testout.log
    sed "s|\`pwd\`|`pwd`|" $FN| $RUNISQL  2>&1 |tee -a testout.log
    echo "Repeat with USER=TESTER" >>testout.log
    sed "s|\`pwd\`|`pwd`|" $FN| $TESTISQL  2>&1 |tee -a testout.log
    done
  echo "Dropping definitions"
  $RUNISQL < dropdefs.sql  
  
  
  echo "Tests Completed"
  echo "UDR Log Contents"  >>testout.log
  echo "----------------"  >>testout.log
  cat $FIREBIRD/fbudrtests.log |sed 's|[0-9]\+-[0-9]\+-[0-9]\+ [0-9]\+:[0-9]\+:[0-9]\+\.[0-9]\+|dd-mm-yy hh:mm:ss.zzzz|' >>testout.log
  if [ -f Reference.log ] ; then
    echo "Diff output is"
    diff Reference.log testout.log
  fi
fi
