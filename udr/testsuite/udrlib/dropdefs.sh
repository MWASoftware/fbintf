#!/bin/sh

if [ -z "$FIREBIRD" ]; then
  echo "FIREBIRD not defined"
  exit
fi

$FIREBIRD/bin/isql -user SYSDBA -pass masterkey employee <<EOT
drop function MyRowCount;
drop function BadRowCount;
drop function UDRInfo;
drop procedure MyTestProc;
drop procedure MySelectProc;
drop procedure MyReadText;
Alter Table EMPLOYEE drop PREVIOUS_PHONE_EXT;
drop trigger MyEmployeeUpdate;
EOT


