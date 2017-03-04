@echo off
REM Test suite Configuration parameters
REM These may be modified if needed to suite local requirements

set DELPHIBIN=C:\Program Files\Embarcadero\RAD Studio\7.0\bin
set DIFF=C:\Program Files\GnuWin32\bin\diff.exe

set TESTOUTDIR=%TEMP%\fbintf-testsuite
set USERNAME=SYSDBA
set PASSWORD=masterkey
set EMPLOYEEDB=localhost:employee
set NEWDBNAME=localhost:%TESTOUTDIR%\testsuite1.fdb
set NEWDBNAME2=localhost:%TESTOUTDIR%\testsuite2.fdb
set BAKFILE=%TESTOUTDIR%\testsuite.gbk

rd /s /q testunits
mkdir testunits
mkdir %TESTOUTDIR%
del testsuite.exe
"%DELPHIBIN%\dcc32" -B -E. -N0testunits -I..\include;..\client\include -U..;..\client;..\client\2.5\;..\client\3.0;..\client\3.0\firebird testsuite.dpr

echo( 
echo Starting Testsuite
echo( 
IF EXIST "testsuite.exe" (
testsuite.exe -u %USERNAME% -p %PASSWORD% -e %EMPLOYEEDB% -n %NEWDBNAME% -s %NEWDBNAME2% -b %BAKFILE% -o testout.log %1
echo Comparing results with reference log
echo( 
"%DIFF%" reference.log testout.log >diff.log
type diff.log 
rd /s /q testunits
del testsuite.exe
)
