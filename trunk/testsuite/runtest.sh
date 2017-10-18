#!/bin/sh

#Test suite Configuration parameters
#These may be modified if needed to suite local requirements

TESTOUTDIR=/tmp/fbintf-testsuite
USERNAME=SYSDBA
PASSWORD=masterkey
EMPLOYEEDB=localhost:employee
NEWDBNAME=localhost:$TESTOUTDIR/testsuite1.fdb
NEWDBNAME2=localhost:$TESTOUTDIR/testsuite2.fdb
BAKFILE=$TESTOUTDIR/testsuite.gbk

cd `dirname $0`
mkdir -p $TESTOUTDIR
chmod 777 $TESTOUTDIR
export FPCDIR=/usr/lib/fpc/`fpc -iV`
fpcmake
make clean
make
if [ -x testsuite ]; then
  echo ""
  echo "Starting Testsuite"
  echo ""
  ./testsuite -u $USERNAME -p $PASSWORD -e $EMPLOYEEDB -n $NEWDBNAME -s $NEWDBNAME2 -b $BAKFILE -o testout.log $1
  echo "Comparing results with reference log"
  echo ""
  diff reference.log testout.log >diff.log
  cat diff.log 
else
  echo "Unable to run test suite"
fi
rm -r testunits
rm testsuite
exit 0

