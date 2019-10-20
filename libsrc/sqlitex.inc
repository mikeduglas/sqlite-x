  INCLUDE('abutil.inc'), ONCE
  INCLUDE('entity.inc'), ONCE

!- data types
sqlite3                       EQUATE(LONG)      !- Database handle
sqlite3_stmt                  EQUATE(LONG)      !- Statement handle
sqlite3_context               EQUATE(LONG)
sqlite3_value                 EQUATE(LONG)

SQLITE_DB_TEMPORARY           EQUATE('')
SQLITE_DB_MEMORY              EQUATE(':memory:')

!- These constants are codes for each of those types. 
SQLITE_DATATYPE               EQUATE(LONG)
SQLITE_INTEGER                EQUATE(1)
SQLITE_FLOAT                  EQUATE(2)
SQLITE_TEXT                   EQUATE(3)
SQLITE_BLOB                   EQUATE(4)
SQLITE_NULL                   EQUATE(5)

!- Result Codes
SQLITE_Result_Code            EQUATE(LONG)
SQLITE_OK                     EQUATE(0)
!- beginning-of-error-codes
SQLITE_ERROR                  EQUATE(1)    !- Generic error
SQLITE_INTERNAL               EQUATE(2)    !- Internal logic error in SQLite
SQLITE_PERM                   EQUATE(3)    !- Access permission denied
SQLITE_ABORT                  EQUATE(4)    !- Callback routine requested an abort
SQLITE_BUSY                   EQUATE(5)    !- The database file is locked
SQLITE_LOCKED                 EQUATE(6)    !- A table in the database is locked
SQLITE_NOMEM                  EQUATE(7)    !- A malloc() failed
SQLITE_READONLY               EQUATE(8)    !- Attempt to write a readonly database
SQLITE_INTERRUPT              EQUATE(9)    !- Operation terminated by sqlite3_interrupt()*/
SQLITE_IOERR                  EQUATE(10)   !- Some kind of disk I/O error occurred
SQLITE_CORRUPT                EQUATE(11)   !- The database disk image is malformed
SQLITE_NOTFOUND               EQUATE(12)   !- Unknown opcode in sqlite3_file_control()
SQLITE_FULL                   EQUATE(13)   !- Insertion failed because database is full
SQLITE_CANTOPEN               EQUATE(14)   !- Unable to open the database file
SQLITE_PROTOCOL               EQUATE(15)   !- Database lock protocol error
SQLITE_EMPTY                  EQUATE(16)   !- Internal use only
SQLITE_SCHEMA                 EQUATE(17)   !- The database schema changed
SQLITE_TOOBIG                 EQUATE(18)   !- String or BLOB exceeds size limit
SQLITE_CONSTRAINT             EQUATE(19)   !- Abort due to constraint violation
SQLITE_MISMATCH               EQUATE(20)   !- Data type mismatch
SQLITE_MISUSE                 EQUATE(21)   !- Library used incorrectly
SQLITE_NOLFS                  EQUATE(22)   !- Uses OS features not supported on host
SQLITE_AUTH                   EQUATE(23)   !- Authorization denied
SQLITE_FORMAT                 EQUATE(24)   !- Not used
SQLITE_RANGE                  EQUATE(25)   !- 2nd parameter to sqlite3_bind out of range
SQLITE_NOTADB                 EQUATE(26)   !- File opened that is not a database file
SQLITE_NOTICE                 EQUATE(27)   !- Notifications from sqlite3_log()
SQLITE_WARNING                EQUATE(28)   !- Warnings from sqlite3_log()
SQLITE_ROW                    EQUATE(100)  !- sqlite3_step() has another row ready
SQLITE_DONE                   EQUATE(101)  !- sqlite3_step() has finished executing
!- end-of-error-codes

  MAP
    !- sqlite3_exec callback prototype
    SQLITE_EXECCB_PROTO(LONG pObject, LONG argc, LONG argv, LONG argn), LONG, C, TYPE
    !- user-defined function prototype
    SQLITE_XFUNC_PROTO(sqlite3_context ctx, LONG argc, *sqlite3_value argv), C, TYPE
    !- the callback prototype to be invoked periodically during long running calls to sqlite3_exec().
    !- If the progress callback returns non-zero, the operation is interrupted.
    SQLITE_PROGRESS_PROTO(LONG pArg), LONG, C, TYPE

    MODULE('sqlitex')
      !- this set of interface routines to access the parameter values on the function or aggregate. 
      sqlite::value_int(LONG argnum, sqlite3_value argv), LONG
      sqlite::value_double(LONG argnum, sqlite3_value argv), REAL
      sqlite::value_text(LONG argnum, sqlite3_value argv), STRING
      sqlite::value_blob(LONG argnum, sqlite3_value argv), *STRING
      sqlite::value_type(LONG argnum, sqlite3_value argv), SQLITE_DATATYPE


      !- These routines are used by the xFunc or xFinal callbacks that implement SQL functions and aggregates.
      sqlite::result_double(sqlite3_context ctx, REAL value)
      sqlite::result_int(sqlite3_context ctx, LONG value)
      sqlite::result_null(sqlite3_context ctx)
      sqlite::result_text(sqlite3_context ctx, STRING value)
   
      !- converts to utf-8 and binds
      sqlite::bind_text(sqlite3_stmt pStmt, LONG pIndex, STRING pText), SQLITE_Result_Code

      !- use in ExecCB callback to extract column name and value
      sqlite::array_element(LONG pArray, LONG pIndex), *CSTRING
    END
  END


TCallbackObject               GROUP, TYPE
ThisSqlite                      &TSqliteBase !- SELF
V                               ANY         !- result of sql query
G                               &GROUP      !- result of sql query
Q                               &QUEUE      !- result of sql query
F                               &FILE       !- result of sql query
                              END

!!!<summary>
!!!Base sqlite class
!!!</summary>
TSqliteBase                   CLASS, TYPE, MODULE('sqlitex.clw'), LINK('sqlitex.clw')
db                              sqlite3, PROTECTED  !- SQLite db handle

ProgressAddress                 LONG  !- the address of callback function (see SQLITE_PROGRESS_PROTO)
ProgressInterval                LONG  !- the approximate number of virtual machine instructions that are evaluated between successive invocations of the callback

Construct                       PROCEDURE()
Destruct                        PROCEDURE(), VIRTUAL

ExecQuery                       PROCEDURE(STRING pSql, LONG lpObject), BOOL, VIRTUAL, PROTECTED

!!!<summary>Opens an SQLite database file as specified by the filename argument.</summary>
!!!<param name="filename">If the filename is ":memory:", then a private, temporary in-memory database is created for the connection. 
!!!This in-memory database will vanish when the database connection is closed. 
!!!Future versions of SQLite might make use of additional special filenames that begin with the ":" character. 
!!!It is recommended that when a database filename actually does begin with a ":" character 
!!!you should prefix the filename with a pathname such as "./" to avoid ambiguity.
!!!If the filename is an empty string, then a private, temporary on-disk database will be created. 
!!!This private database will be automatically deleted as soon as the database connection is closed.</param>
Open                            PROCEDURE(STRING filename), BOOL, VIRTUAL
!!!<summary>Closes an SQLite database connection.</summary>
Close                           PROCEDURE()

!!!<summary>Returns numeric result code.</summary>
ErrCode                         PROCEDURE(), SQLITE_Result_Code
!!!<summary>Returns English-language text that describes the error.</summary>
ErrMsg                          PROCEDURE(), STRING

!!!<summary>Executes sql script and optionally stores result set in passed scalar/group/queue/file.</summary>
Exec                            PROCEDURE(STRING pSql), BOOL, PROC
!!!<summary>Executes sql script and optionally stores result set in passed scalar/group/queue/file.</summary>
Exec                            PROCEDURE(STRING pSql, *? pScalar), BOOL, PROC
!!!<summary>Executes sql script and optionally stores result set in passed scalar/group/queue/file.</summary>
Exec                            PROCEDURE(STRING pSql, *GROUP pGrp), BOOL, PROC
!!!<summary>Executes sql script and optionally stores result set in passed scalar/group/queue/file.</summary>
Exec                            PROCEDURE(STRING pSql, *QUEUE pQue), BOOL, PROC
!!!<summary>Executes sql script and optionally stores result set in passed scalar/group/queue/file.</summary>
Exec                            PROCEDURE(STRING pSql, *FILE pFile), BOOL, PROC

!!!<summary>Callback method.</summary>
ExecCB                          PROCEDURE(LONG nColumns, LONG argc, LONG argv, *TCallbackObject pObject), SQLITE_Result_Code, VIRTUAL, PROTECTED !- executes for each result row

!!!<summary>Progress method.</summary>
Progress                        PROCEDURE(), SQLITE_Result_Code, VIRTUAL, PROTECTED !- executes for each result row

!The life-cycle of a prepared statement object usually goes like this:
!1. Create the prepared statement object using Prepare().
!2. Bind values to parameters using the BindValue()/BindGroup().
!3. Run the SQL by calling Step() one or more times.
!4. Reset the prepared statement using BindReset() then go back to step 2. Do this zero or more times.
!5. Destroy the object using Finalize().

!!!<summary>To execute an SQL statement, it must first be compiled into a byte-code program using this routine.</summary>
Prepare                         PROCEDURE(STRING pSql), sqlite3_stmt, VIRTUAL
Finalize                        PROCEDURE(sqlite3_stmt pStmt), SQLITE_Result_Code, PROC
!!!<summary>After Prepare, this function must be called one or more times to evaluate the statement.</summary>
Step                            PROCEDURE(sqlite3_stmt pStmt), SQLITE_Result_Code

BindValue                       PROCEDURE(sqlite3_stmt pStmt, <? pValue>, LONG pIndex, BOOL pAsBlob = FALSE), SQLITE_Result_Code, PROC
BindBlob                        PROCEDURE(sqlite3_stmt pStmt, *BLOB pBlob, LONG pIndex), SQLITE_Result_Code, PROC
BindEntity                      PROCEDURE(sqlite3_stmt stmt, *TEntity entity), BOOL, PROC

BindReset                       PROCEDURE(sqlite3_stmt pStmt), SQLITE_Result_Code, PROC

!!!<summary>Defines a user-defined function.</summary>
UDF                             PROCEDURE(STRING funcName, LONG nArgs, SQLITE_XFUNC_PROTO xFunc), BOOL, PROC

!!!<summary>Executes "CREATE TABLE".</summary>
CreateTable                     PROCEDURE(*FILE f, BOOL pWithBlobs = FALSE), BOOL, PROC
!!!<summary>Executes "CREATE TABLE".</summary>
CreateTable                     PROCEDURE(*QUEUE q, STRING pTblName), BOOL, PROC
!!!<summary>Copies all data.</summary>
CloneTable                      PROCEDURE(*FILE f, BOOL pWithBlobs = FALSE), BOOL, PROC
!!!<summary>Copies all data.</summary>
CloneTable                      PROCEDURE(*QUEUE q, STRING pTblName), BOOL, PROC
!!!<summary>Executes "DROP TABLE".</summary>
DropTable                       PROCEDURE(*FILE f), BOOL, PROC
!!!<summary>Executes "DROP TABLE".</summary>
DropTable                       PROCEDURE(STRING pTblName), BOOL, PROC
!!!<summary>Copies data from source file/queue.</summary>
InsertIntoTable                 PROCEDURE(*FILE f, BOOL pWithBlobs = FALSE), BOOL, PROC
!!!<summary>Copies data from source file/queue.</summary>
InsertIntoTable                 PROCEDURE(*QUEUE q, STRING pTblName), BOOL, PROC

!!!<summary>Inserts current record buffer.</summary>
InsertRecord                    PROCEDURE(*FILE f), BOOL, PROC
!!!<summary>Inserts current record buffer.</summary>
InsertRecord                    PROCEDURE(*GROUP g, STRING pTblName), BOOL, PROC
!!!<summary>Updates a table record(s) with current record buffer.</summary>
!!!<param name="pWhere">'name=value,...' list, without WHERE keyword.</param>
UpdateRecord                    PROCEDURE(*FILE f, STRING pWhere), BOOL, PROC
!!!<summary>Updates a table record(s) with current record buffer.</summary>
!!!<param name="pWhere">'name=value,...' list, without WHERE keyword.</param>
UpdateRecord                    PROCEDURE(*GROUP g, STRING pTblName, STRING pWhere), BOOL, PROC
!!!<summary>Deletes a table record(s).</summary>
!!!<param name="pWhere">'name=value,...' list, without WHERE keyword.</param>
DeleteRecord                    PROCEDURE(*FILE f, STRING pWhere), BOOL, PROC
!!!<summary>Deletes a table record(s).</summary>
!!!<param name="pWhere">'name=value,...' list, without WHERE keyword.</param>
DeleteRecord                    PROCEDURE(STRING pTblName, STRING pWhere), BOOL, PROC
                              END

!!!<summary>
!!!This class allows to display result set returned from Exec(sql) method in a virtual listbox (number of rows and columns and column headers come from result set).
!!!</summary>
TSqliteViewer                 CLASS(TSqliteBase), TYPE, MODULE('sqlitex.clw'), LINK('sqlitex.clw')
listFeq                         SIGNED, PROTECTED
ColNames                        &TCBNames, PROTECTED
ColValues                       &TCBValues, PROTECTED
nChanges                        LONG, PROTECTED

columnFormat                    STRING(255) !- format string for each column, by default '42L(2)|M~%s~L(1)@s255@'; %s will be changed by column header.

Construct                       PROCEDURE()
Destruct                        PROCEDURE(), DERIVED

SetListbox                      PROCEDURE(SIGNED pFeq)

ExecQuery                       PROCEDURE(STRING pSql, LONG lpObject), BOOL, DERIVED, PROTECTED
ExecCB                          PROCEDURE(LONG argc, LONG argv, LONG argn, *TCallbackObject pObject), LONG, DERIVED, PROTECTED
VLBproc                         PROCEDURE(LONG row, SHORT column), STRING, VIRTUAL, PROTECTED  !- data source for virtual list box
 
PrintIt                         PROCEDURE()
                              END

!!!<summary>
!!!This class applies SQL filter to standatd browse.
!!!</summary>
TBRWSQLFilter                 CLASS(TSqliteBase), TYPE, MODULE('sqlitex.clw'), LINK('sqlitex.clw')
FieldPairs                      &FieldPairsClass, PROTECTED
FilterQ                         &QUEUE, PROTECTED
SelectStatement                 ANY, PROTECTED
IsActive                        BOOL

Construct                       PROCEDURE()
Destruct                        PROCEDURE(), DERIVED

Init                            PROCEDURE(FILE pFile, KEY pKey, QUEUE pFilterQ, STRING pSelectStmt), VIRTUAL

ValidateRecord                  PROCEDURE(), BOOL, VIRTUAL
ApplyFilter                     PROCEDURE(STRING pWhere), BOOL, VIRTUAL
ResetFilter                     PROCEDURE(), VIRTUAL, VIRTUAL
                              END
