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
  #normalise the run dependent values on both sides of the comparison:
  #transaction ids depend on the server's history and IBX$CREATED is the
  #journal's own timestamp
  normalise_log()
  {
    sed -e 's|Timestamp = [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9][0-9]|Timestamp = yyyy/mm/dd hh:mm:ss.zzzz|' \
        -e 's|Transaction ID = [0-9][0-9]*|Transaction ID = nnnn|' \
        -e 's|IBX$CREATED = [0-9][0-9]*/[0-9][0-9]*/[0-9][0-9]* [0-9:.]*|IBX$CREATED = yyyy/mm/dd hh:mm:ss.zzzz|' \
        -e 's|Database ID = [0-9][0-9]* FB = .* SN = .*|Database ID = n FB = dbpath SN = hostname|' \
        -e 's|^Pages Used = [0-9][0-9]*|Pages Used = nnnn|' \
        -e 's|^Pages Free = [0-9][0-9]*|Pages Free = nnnn|' \
        -e 's|^Fetches  = [0-9][0-9]*|Fetches  = nnnn|' \
        -e 's|^Reads  = [0-9][0-9]*|Reads  = nnnn|' \
        -e 's|^Writes  = [0-9][0-9]*|Writes  = nnnn|' \
        -e 's|^Page Writes  = [0-9][0-9]*|Page Writes  = nnnn|' \
        -e 's|^Count = [0-9][0-9]*|Count = nnnn|' \
        -e 's|^Pages =[0-9][0-9]*|Pages =nnnn|' \
        -e 's|^Server Memory = [0-9][0-9]*|Server Memory = nnnn|' \
        -e 's|^Max Memory  = [0-9][0-9]*|Max Memory  = nnnn|' \
        -e 's|^Database Created: .*|Database Created: yyyy/mm/dd hh:mm:ss|' \
        -e 's|^Version = 1: .*|Version = 1: server version string|' \
        -e 's|^Implementation = .*|Implementation = implementation codes|' \
        -e 's|^Server Version = .*|Server Version = server version string|' \
        -e 's|^RDB$SECURITY_CLASS = .*|RDB$SECURITY_CLASS = SQL$nnn|' \
        "$1"
  }
  if grep 'Provider = pure Pascal wire protocol' testout.log >/dev/null; then
    REFLOG=FBWirereference.log
  elif grep 'ODS Major Version = 11' testout.log >/dev/null; then
    REFLOG=FB2reference.log
  elif grep 'ODS Major Version = 12' testout.log >/dev/null; then
    REFLOG=FB3reference.log
  elif grep 'ODS Major Version = 13' testout.log >/dev/null && grep 'ODS Minor Version = 0' testout.log >/dev/null; then
    REFLOG=FB4reference.log
  else
    REFLOG=FB5reference.log
  fi
  normalise_log $REFLOG >reference.tmp
  normalise_log testout.log >testout.tmp
  diff reference.tmp testout.tmp >diff.log
  rm -f reference.tmp testout.tmp
  cat diff.log
else
  echo "Unable to run test suite"
fi
rm -r testunits
rm testsuite
exit 0

