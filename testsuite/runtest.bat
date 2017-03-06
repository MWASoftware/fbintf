@echo off
REM Test suite Configuration parameters
REM These may be modified if needed to suite local requirements


if EXIST C:\lazarus\fpc\3.0.2\bin\i386-win32\fpc.exe (
set FPCDIR=C:\lazarus\fpc\3.0.2
set FPCBIN=C:\lazarus\fpc\3.0.2\bin\i386-win32
Goto COMPILE
)

if EXIST C:\lazarus\fpc\3.0.2\bin\x86_64-win64\fpc.exe (
set FPCDIR=C:\lazarus\fpc\3.0.2
set FPCBIN=C:\lazarus\fpc\3.0.2\bin\x86_64-win64
Goto COMPILE
)

if EXIST C:\lazarus\fpc\3.0.0\bin\i386-win32\fpc.exe (
set FPCDIR=C:\lazarus\fpc\3.0.0
set FPCBIN=C:\lazarus\fpc\3.0.0\bin\i386-win32
Goto COMPILE
)

if EXIST C:\lazarus\fpc\3.0.0\bin\x86_64-win64\fpc.exe (
set FPCDIR=C:\lazarus\fpc\3.0.0
set FPCBIN=C:\lazarus\fpc\3.0.0\bin\x86_64-win64
Goto COMPILE
)

if not EXIST %FPCBIN%\fpc.exe (
  echo "Unable to find fpc.exe"
  goto :EOF
)

:COMPILE
set TESTOUTDIR=%TEMP%\fbintf-testsuite
set USERNAME=SYSDBA
set PASSWORD=masterkey
set EMPLOYEEDB=localhost:employee
set NEWDBNAME=localhost:%TESTOUTDIR%\testsuite1.fdb
set NEWDBNAME2=localhost:%TESTOUTDIR%\testsuite2.fdb
set BAKFILE=%TESTOUTDIR%\testsuite.gbk

rd /s /q testunits
mkdir %TESTOUTDIR%
%FPCBIN%\fpcmake
%FPCBIN%\make clean
%FPCBIN%\make
echo( 
echo Starting Testsuite
echo( 
IF EXIST "testsuite.exe" (
testsuite.exe -u %USERNAME% -p %PASSWORD% -e %EMPLOYEEDB% -n %NEWDBNAME% -s %NEWDBNAME2% -b %BAKFILE% -o testout.log %1
echo Comparing results with reference log
echo( 
%FPCBIN%\diff reference.log testout.log >diff.log
type diff.log 
rd /s /q testunits
del testsuite.exe
)
