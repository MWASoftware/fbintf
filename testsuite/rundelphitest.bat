@echo off
REM Test suite Configuration parameters
REM These may be modified if needed to suite local requirements

REM Test suite Configuration parameters (FPCDIR and FPCBIN)
REM These may be modified if needed to suite local requirements


FOR %%V in (3.2.0 3.0.4 3.0.2 3.0.0) do (
  if EXIST C:\lazarus\fpc\%%V\bin\i386-win32\fpc.exe (
    set FPCDIR=C:\lazarus\fpc\%%V
    set FPCBIN=C:\lazarus\fpc\%%V\bin\i386-win32
    Goto COMPILE
  )
  if EXIST C:\lazarus\fpc\%%V\bin\x86_64-win32\fpc.exe (
    set FPCDIR=C:\lazarus\fpc\%%V
    set FPCBIN=C:\lazarus\fpc\%%V\bin\x86_64-win32
    Goto COMPILE
  )
  if EXIST C:\lazarus\fpc\%%V\bin\x86_64-win64\fpc.exe (
    set FPCDIR=C:\lazarus\fpc\%%V
    set FPCBIN=C:\lazarus\fpc\%%V\bin\x86_64-win64
    Goto COMPILE
  )
)


:COMPILE
set DELPHIBIN=C:\Program Files\Embarcadero\RAD Studio\7.0\bin
if EXIST "%FPCBIN%\diff.exe" (
  set DIFF=%FPCBIN%\diff.exe
) ELSE (
  set DIFF=C:\Program Files\GnuWin32\bin\diff.exe
)
echo DIFF is %DIFF%

set TESTOUTDIR=%TEMP%\fbintf-testsuite
set USERNAME=SYSDBA
set PASSWORD=masterkey
set EMPLOYEEDB=employee
set NEWDBNAME=%TESTOUTDIR%\testsuite1.fdb
set NEWDBNAME2=%TESTOUTDIR%\testsuite2.fdb
set BAKFILE=%TESTOUTDIR%\testsuite.gbk

rd /s /q testunits
mkdir testunits
mkdir %TESTOUTDIR%
IF EXIST "%DELPHIBIN%\dcc32.exe" (
del testsuite.exe
"%DELPHIBIN%\dcc32" -B -E. -N0testunits -I..\include;..\client\include -U..;..\client;..\client\2.5\;..\client\3.0;..\client\3.0\firebird testsuite.dpr
)
echo( 
echo Starting Testsuite
echo( 
IF EXIST "testsuite.exe" (
testsuite.exe -u %USERNAME% -p %PASSWORD% -e %EMPLOYEEDB% -n %NEWDBNAME% -s %NEWDBNAME2% -b %BAKFILE% -o testout.log %1
if not EXIST "%DIFF%" (
  echo Unable to compare results - diff not found
  goto :EOF
  )

echo Comparing results with reference log
echo(
findstr /C:"ODS Major Version = 11" testout.log
IF ERRORLEVEL 1 (
  findstr /C:"ODS Major Version = 12" testout.log
  IF ERRORLEVEL 1 (
    %DIFF% FB4reference.log testout.log >diff.log
  ) ELSE (
    %DIFF% FB3reference.log testout.log >diff.log
  )
) ELSE (
  %DIFF% FB2reference.log testout.log >diff.log
)
type diff.log

IF EXIST "%DELPHIBIN%\dcc32" (
rd /s /q testunits
rem del testsuite.exe
)
)
