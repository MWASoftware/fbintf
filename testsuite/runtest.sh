#!/bin/sh

#Test suite Configuration parameters
#These may be modified if needed to suite local requirements

TESTOUTDIR=/tmp/fbintf-testsuite
USERNAME=SYSDBA
PASSWORD=masterkey
EMPLOYEEDB=employee
NEWDBNAME=$TESTOUTDIR/testsuite1.fdb
NEWDBNAME2=$TESTOUTDIR/testsuite2.fdb
BAKFILE=$TESTOUTDIR/testsuite.gbk
if [ -z "$FPC" ]; then
  export FPC=fpc
fi

cd `dirname $0`
mkdir -p $TESTOUTDIR
chmod 777 $TESTOUTDIR
export FPCDIR=/usr/lib/fpc/`$FPC -iV`
fpcmake
make clean
make
if [ -x testsuite ]; then
  if [ -n "$FIREBIRD" ]; then
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$FIREBIRD/lib"
  fi
  echo ""
  echo "Starting Testsuite"
  echo ""
  ./testsuite -u $USERNAME -p $PASSWORD -e $EMPLOYEEDB -n $NEWDBNAME -s $NEWDBNAME2 -b $BAKFILE -o testout.log $@
  #normalise data/time
  sed -i 's|Timestamp = [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9][0-9]|Timestamp = yyyy/mm/dd hh:mm:ss.zzzz|' testout.log
  echo "Comparing results with reference log"
  echo ""
  if grep 'ODS Major Version = 11' testout.log >/dev/null; then
    diff FB2reference.log testout.log >diff.log
  elif grep 'ODS Major Version = 12' testout.log >/dev/null; then
    diff FB3reference.log testout.log >diff.log
  else
    diff FB4reference.log testout.log >diff.log
  fi
  cat diff.log 
else
  echo "Unable to run test suite"
fi
rm -r testunits
rm testsuite
exit 0

