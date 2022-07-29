@echo off
REM Test suite Configuration parameters
REM These may be modified if needed to suite local requirements

REM Test suite Configuration parameters (FPCDIR and FPCBIN)
REM These may be modified if needed to suite local requirements

REM This script will typically need to be run as "administrator"

set FBINTFROOT=..\..\..
set FIREBIRD=
FOR %%G in (4_0 3_0) do (
  if EXIST "%ProgramFiles%\Firebird\Firebird_%%G\firebird.exe" (
    set FIREBIRD=%ProgramFiles%\Firebird\Firebird_%%G
    goto FBFOUND
  )
  if EXIST "%ProgramFiles(x86)%\Firebird\Firebird_%%G\firebird.exe" (
    set FIREBIRD=%ProgramFiles(x86)%\Firebird\Firebird_%%G
    goto FBFOUND
 )
)

echo Unable to locate Firebird
goto :EOF

:FBFOUND
echo Firebird set to %FIREBIRD%


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
echo FPC not found
goto :EOF

:COMPILE

if EXIST %FPCBIN%\diff.exe (
  set DIFF=%FPCBIN%\diff.exe
) ELSE (
  set DIFF=C:\Program Files\GnuWin32\bin\diff.exe
)
echo DIFF is %DIFF%



set ISQL="%FIREBIRD%\isql.exe"

if not Exist %ISQL% (
  echo Unable to locate isql at %ISQL%
  goto :EOF
)
echo ISQL is %ISQL%

set RUNISQL=%ISQL% -user SYSDBA -pass masterkey localhost:employee
set RUNTESTERISQL=%ISQL% -user TESTER -pass testing localhost:employee
echo Run command is %RUNISQL%

rd /s /q testunits
mkdir testunits
del fbudrtests.dll
%FPCBIN%\fpcmake
%FPCBIN%\make clean
%FPCBIN%\make

if not Exist fbudrtests.dll (
  echo Unable to find fbudrtests.dll
  goto :EOF
)

net stop "Firebird Server - DefaultInstance"
copy /y fbudrtests.dll testtext.txt test.empty "%FIREBIRD%\plugins\udr"
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
   echo Running with User TESTER  >>testout.log
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

rd /s /q testunits
del "%FIREBIRD%\plugins\udr"\testtext.txt "%FIREBIRD%\plugins\udr"\test.empty


