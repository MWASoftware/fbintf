#!/bin/sh


usage()
{
  echo "dotest.sh [-2] [-3] [-4 b1|b2] [-5] [-t <testid>] [-p <fpc version>]"
}

BUILD=
#Parse Parameters
TEMP=`getopt h2345db:t:f:p: "$@"`
if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

while true ; do
        case "$1" in
        -h)     usage; exit 1;;

        \-2)     FB="2.5.9";  shift 1;;

        \-3) 	FB="3.0.10"; shift 1;;

        \-4) 	FB="4.0.4"; shift 1;;

	\-5)	FB="5.0.0"; shift 1;;

	-d)	FB="master"; shift 1;;

	-f)	FB=$2; shift 2;;
        
        -t)    TEST="-t $2"; shift 2;;

	-b)	BUILD="$2"; shift 2;;
	
	-p) export FPCDIR=/usr/lib/fpc/$2; shift 2;;

        --)    shift; break;; 

        *)      echo "Unrecognised argument $1"; usage; exit 1;;
        
        esac
done
if [ -n "$BUILD" ]; then
  FB="4"
fi

export FIREBIRD=/opt/firebirdVersions/$FB$BUILD
echo "FIREBIRD=$FIREBIRD"

if [ -n "$FPCDIR" ]  && [ -d "$FPCDIR" ]; then
  export FPC=$FPCDIR/ppcx64
fi

if [ ! -d "$FIREBIRD" ]; then
  echo "$FIREBIRD not found"
else
  ./runtest.sh  $TEST
fi

