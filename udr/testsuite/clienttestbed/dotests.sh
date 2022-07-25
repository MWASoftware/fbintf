#!/bin/sh
usage()
{
  echo "Run UDR tests"
  echo "dotest.sh  [-3] [-4] "
}

BUILD=
#Parse Parameters
TEMP=`getopt h34db:f: "$@"`
if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

while true ; do
        case "$1" in
        -h)     usage; exit 1;;

        \-3) 	FB="3.0.5"; shift 1;;

        \-4) 	FB="4.0.1"; shift 1;;

	-d)	FB="dev"; shift 1;;

	-f)	FB=$2; shift 2;;
        
        --)    shift; break;; 

        *)      echo "Unrecognised argument $1"; usage; exit 1;;
        
        esac
done

export FIREBIRD=/opt/firebird/$FB
echo "FIREBIRD=$FIREBIRD"
export LD_LIBRARY_PATH=$FIREBIRD/lib
export FBINTFROOT=../../..

if [ ! -d "$FIREBIRD" ]; then
  echo "$FIREBIRD not found"
else
  fpcmake
  make clean
  make
  if [ ! -x testsuite ]; then
    echo "Compilation failed"	  
    exit 1
  fi
  ./testsuite -o testout.log
  echo 'Server Side Log' >>testout.log
  echo '---------------' >>testout.log
  cat serverside.log |sed 's|[0-9]\+-[0-9]\+-[0-9]\+ [0-9]\+:[0-9]\+:[0-9]\+\.[0-9]\+|dd-mm-yy hh:mm:ss.zzzz|' >>testout.log
  if [ -f Reference.log ] ; then
    echo "Diff output is"
    diff Reference.log testout.log
  fi
fi
