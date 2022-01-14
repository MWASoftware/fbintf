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
create or alter procedure MyTestProc (
      EMP_NO SMALLINT
    ) returns (Salary Numeric(10,2), FullName VarChar(36))
    external name 'fbudrtests!test_proc'
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

