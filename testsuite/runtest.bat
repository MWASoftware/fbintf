
#Test suite Configuration parameters
#These may be modified if needed to suite local requirements

set FPCDIR=C:\lazarus-win32\fpc\3.0.0
set FPCBIN=%FPCDIR%\bin\i386-win32
set TESTOUTDIR=%TEMP%\fbintf-testsuite
set USERNAME=SYSDBA
set PASSWORD=masterkey
set EMPLOYEEDB=localhost:employee
set NEWDBNAME=localhost:%TESTOUTDIR%\testsuite1.fdb
set NEWDBNAME2=localhost:%TESTOUTDIR%\testsuite2.fdb
set BAKFILE=%TESTOUTDIR%\testsuite.gbk

cd `dirname $0`
mkdir %TESTOUTDIR%
%FPCBIN%\fpcmake
%FPCBIN%\make clean
%FPCBIN%\make
echo ""
echo "Starting Testsuite"
echo ""
IF EXIST "testsuite.exe" (
testsuite.exe -u %USERNAME% -p %PASSWORD% -e %EMPLOYEEDB% -n %NEWDBNAME% -s %NEWDBNAME2% -b %BAKFILE%>testout.log
echo "Comparing results with reference log"
echo ""
%FPCBIN%\diff reference.log testout.log >diff.log
#type diff.log 
)
