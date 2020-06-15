#!/bin/sh

usage()
{
  echo "dotest.sh [-2] [-3] [-4 b1|b2]"
}

if [ $# -eq 0 ]; then
  FB=4
  BUILD=b2
else
#Parse Parameters
TEMP=`getopt h234: "$@"`
if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

while true ; do
        case "$1" in
        -h)     usage; exit 1;;

        \-2)     FB="2.5.9"; break; shift 1;;

        \-3) 	FB="3.0.5"; break; shift 1;;

        \-4) 	FB=4; BUILD="$2"; break; shift 2;;

        *)      echo "Unrecognised argument $FB"; usage; exit 1;;
        esac
done
fi

export FIREBIRD=/opt/firebird$FB$BUILD
export LD_LIBRARY_PATH=$FIREBIRD/lib

if [ ! -d "$FIREBIRD" ]; then
  echo "$FIREBIRD not found"
else
  ./runtest.sh
fi

