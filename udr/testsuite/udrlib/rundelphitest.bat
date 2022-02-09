@echo off
REM Test suite Configuration parameters
REM These may be modified if needed to suite local requirements

REM Test suite Configuration parameters (FPCDIR and FPCBIN)
REM These may be modified if needed to suite local requirements

set DELPHIBIN=C:\Program Files\Embarcadero\RAD Studio\7.0\bin
set FIREBIRD=C:\Program Files (x86)\Firebird\Firebird_4_0

FOR %%V in (3.2.2 3.2.0 3.0.4 3.0.2 3.0.0) do (
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

if EXIST "%FPCBIN%\diff.exe" (
  set DIFF=%FPCBIN%\diff.exe
) ELSE (
  set DIFF=C:\Program Files\GnuWin32\bin\diff.exe
)
echo DIFF is %DIFF%


echo Firebird set to %FIREBIRD%

rem if not Exist ""%FIREBIRD%\firebird.exe"" (
rem  echo Unable to locate Firebird at %FIREBIRD%
rem  goto :EOF
rem
)


set ISQL=%FIREBIRD%\isql.exe

rem if not Exist "%ISQL%" 
rem  echo Unable to locate isql at %ISQL%"
REM   goto :EOF
REM
)
echo ISQL is %ISQL%

set RUNISQL="%ISQL%" -user SYSDBA -pass masterkey localhost:employee
set RUNTESTERISQL="%ISQL%" -user TESTER -pass testing localhost:employee
echo Run command is %RUNISQL%


IF EXIST "%DELPHIBIN%\dcc32.exe" (
rd /s /q testunits
mkdir testunits
del fbudrtests.dll
"%DELPHIBIN%\dcc32" -B -E. -N0testunits -I../../../client/3.0/firebird ../../../client/include ;..\client\include -U../../../client ../../../client/3.0/firebird  . ../../../client/3.0 ../../source ../../.. fbudrtests.dpr
)

if not Exist fbudrtests.dll (
  echo Unable to find fbudrtests.dll
  goto :EOF
)

net stop "Firebird Server - DefaultInstance"
copy fbudrtests.dll "%FIREBIRD%\plugins\udr"
IF ERRORLEVEL 1 (
  echo Copy to udr directory failed
  goto :EOF
)
echo dll copied to Firebird udr directory

net start "Firebird Server - DefaultInstance"
IF ERRORLEVEL 1 (
  echo Unable to restart Firebird server
  goto :EOF
)
echo Server Restarted

echo( 
echo Running Testsuite
echo( 
echo Running UDR Lib Testsuite >testout.log

echo Adding SQL Definitions
%RUNISQL% < AddDefs.sql

echo Running Tests
for %%f in (Test*.sql) do (
   echo Running %%f 
   echo Running %%f >>testout.log
   echo ------------------------ >>testout.log
   %RUNISQL%  >>testout.log 2>&1 < %%f
   echo(
   echo Running with User TESTER
   %RUNTESTERISQL%  >>testout.log 2>&1 < %%f
)
echo Dropping definitions
%RUNISQL% <dropdefs.sql

echo Tests Completed
echo UDR Log Contents  >>testout.log
echo ----------------  >>testout.log
copy /b testout.log+"%FIREBIRD%\fbudrtests.log" testout2.log
  

echo Comparing results with reference log

%DIFF% Reference.log testout2.log >diff.log

type diff.log

IF EXIST "%DELPHIBIN%\dcc32" (
rd /s /q testunits

)

