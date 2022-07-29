 create or alter function MyRowCount (
      table_name varchar(31)
    ) returns integer
    external name 'fbudrtests!row_count'
    engine udr;    
create or alter function BadRowCount (
      table_name varchar(31)
    ) returns integer
    external name 'fbudrtests!bad_row_count'
    engine udr;  
create or alter function UDRInfo (
    ) returns VarChar(31)
    external name 'fbudrtests!return_info!Hello World'
    engine udr;
create or alter function GetCurDir (
    ) returns VarChar(64)
    external name 'fbudrtests!return_info'
    engine udr;
create or alter procedure MyTestProc (
      EMP_NO SMALLINT
    ) returns (Salary Numeric(10,2), FullName VarChar(36))
    external name 'fbudrtests!test_proc'
    engine udr;
Create or Alter procedure MyErrorProc (
      ErrorCase Smallint)
    external name 'fbudrtests!error_proc'
    engine udr;
create or alter procedure MySelectProc ()
    returns (FullName VarChar(36), Salary Numeric(10,2), AccSalary Numeric(10,2) )
    external name 'fbudrtests!select_proc'
    engine udr;
create or alter procedure MyReadText (
        path varchar(200) not null /*relative to udr directory */
        ) returns (
       text varchar(100) not null
   )
   external name 'fbudrtests!read_txt'
   engine udr;
Alter Table EMPLOYEE Add PREVIOUS_PHONE_EXT VarChar(4);
Create or Alter Trigger MyEmployeeUpdate Active Before Update On  EMPLOYEE
   external name 'fbudrtests!my_employee_update'
    engine udr;       
create or alter procedure ShowAttachments (
     user_name VarChar(32))
    returns (
    MON$ATTACHMENT_ID bigint ,
    MON$ATTACHMENT_NAME varchar(255) ,
    MON$USER char(63) ,
    MON$ROLE char(63) ,
    MON$REMOTE_HOST varchar(255) 
    )
    external name 'fbudrtests!show_att'
    engine udr;
create or alter procedure GetServerInfo
   returns (ServerInfo VarChar(256))
   external name 'fbudrtests!server_info'
   engine udr;
create or alter user TESTER password 'testing';
grant Execute on function MyRowCount to TESTER;
grant Execute on function BadRowCount to TESTER;
grant Execute on function UDRInfo to TESTER;
grant Execute on function GetCurDir to TESTER;
grant Execute on Procedure MyTestProc to TESTER;
grant Execute on Procedure MyErrorProc to TESTER;
grant Execute on Procedure MySelectProc to TESTER;
grant Execute on Procedure MyReadText to TESTER;
grant Execute on Procedure ShowAttachments to TESTER;
grant Execute on Procedure GetServerInfo to TESTER;



