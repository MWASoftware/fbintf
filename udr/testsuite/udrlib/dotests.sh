#!/bin/sh

if [ -z "$FIREBIRD" ]; then
  echo "FIREBIRD not defined"
  exit
fi

$FIREBIRD/bin/isql -user SYSDBA -pass masterkey employee <<EOT
Select MyRowCount('EMPLOYEE') from RDB\$Database;
Select MyRowCount('DEPARTMENT') from RDB\$Database;
select MyRowCount('BAD') from RDB\$Database;
Select UDRINFO() from RDB\$Database;
Execute Procedure MYTESTPROC(24);
Select * From MySelectProc;
Update EMPLOYEE Set PHONE_EXT = '999' Where EMP_NO = 2;
Select PHONE_EXT,PREVIOUS_PHONE_EXT FROM EMPLOYEE Where EMP_NO = 2;
Update EMPLOYEE Set PHONE_EXT = '250' Where EMP_NO = 2;
Select PHONE_EXT,PREVIOUS_PHONE_EXT FROM EMPLOYEE Where EMP_NO = 2;
EOT
