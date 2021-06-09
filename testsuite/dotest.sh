#!/bin/sh

usage()
{
  echo "dotest.sh [-2] [-3] [-4 b1|b2] [-t <testid>]"
}

BUILD=
#Parse Parameters
TEMP=`getopt h234b:t: "$@"`
if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

while true ; do
        case "$1" in
        -h)     usage; exit 1;;

        \-2)     FB="2.5.9";  shift 1;;

        \-3) 	FB="3.0.5"; shift 1;;

        \-4) 	FB="4.0.0"; shift 1;;
        
        -t)    TEST="-t $2"; shift 2;;

	-b)	BUILD="$2"; shift 2;;

        --)    shift; break;; 

        *)      echo "Unrecognised argument $1"; usage; exit 1;;
        
        esac
done

export FIREBIRD=/opt/firebird$FB$BUILD
echo "FIREBIRD=$FIREBIRD"

if [ ! -d "$FIREBIRD" ]; then
  echo "$FIREBIRD not found"
else
  ./runtest.sh $TEST
fi

