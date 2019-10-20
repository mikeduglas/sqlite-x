  MEMBER

  PRAGMA('compile(CWUTIL.CLW)')
  PRAGMA('link(sqlite3.LIB)')

  INCLUDE('svapi.inc'), ONCE
  INCLUDE('sqlitex.inc'), ONCE

!struct Mem {
!  sqlite3 *db;        /* The associated database connection */
!  char *z;            /* String or BLOB value */
!  double r;           /* Real value */
!  union {
!    i64 i;              /* Integer value used when MEM_Int is set in flags */
!    int nZero;          /* Used when bit MEM_Zero is set in flags */
!    FuncDef *pDef;      /* Used only when flags==MEM_Agg */
!    RowSet *pRowSet;    /* Used only when flags==MEM_RowSet */
!    VdbeFrame *pFrame;  /* Used when flags==MEM_Frame */
!  } u;
!  int n;              /* Number of characters in string value, excluding '\0' */
!  u16 flags;          /* Some combination of MEM_Null, MEM_Str, MEM_Dyn, etc. */
!  u8  type;           /* One of SQLITE_NULL, SQLITE_TEXT, SQLITE_INTEGER, etc */
!  u8  enc;            /* SQLITE_UTF8, SQLITE_UTF16BE, SQLITE_UTF16LE */
!#ifdef SQLITE_DEBUG
!  Mem *pScopyFrom;    /* This Mem is a shallow copy of pScopyFrom */
!  void *pFiller;      /* So that sizeof(Mem) is a multiple of 8 */
!#endif
!  void (*xDel)(void *);  /* If not null, call this function to delete Mem.z */
!  char *zMalloc;      /* Dynamic buffer allocated by sqlite3_malloc() */
!}; 
  
!sqlite3 *db    4 bytes
!char *z        4 bytes
!double r       8 bytes
!union(i64)     8 bytes
!int n          4 bytes
!u16 flags      2 bytes
!u8  type       1 byte
!u8  enc        1 byte
!void (*xDel)   4 bytes
!char *zMalloc  4 bytes
!   total=40
sqlite3_mem::size             EQUATE(40)
sqlite3_context::size         EQUATE(sqlite3_mem::size)
sqlite3_value::size           EQUATE(sqlite3_mem::size)

!- sqlite3_destructor_types
SQLITE_STATIC                 EQUATE(0)
SQLITE_TRANSIENT              EQUATE(-1)

!- encodings
SQLITE_UTF8                   EQUATE(1)

!- VLB support
TCBNames                      QUEUE, TYPE
ColName                         STRING(32)
                              END
TCBValues                     QUEUE, TYPE
ColValue                        STRING(256)
                              END

  MAP
    MODULE('win api')
      winapi::memcpy(LONG lpDest,LONG lpSource,LONG nCount),LONG,PROC,NAME('_memcpy')
   
      winapi::MultiByteToWideChar(UNSIGNED Codepage, ULONG dwFlags, ULONG LpMultuByteStr, |
        LONG cbMultiByte, ULONG LpWideCharStr, LONG cchWideChar), RAW, ULONG, PASCAL, PROC, NAME('MultiByteToWideChar')

      winapi::WideCharToMultiByte(UNSIGNED Codepage, ULONG dwFlags, ULONG LpWideCharStr, LONG cchWideChar, |
        ULONG lpMultuByteStr, LONG cbMultiByte, ULONG LpDefalutChar, ULONG lpUsedDefalutChar), RAW, ULONG, PASCAL, PROC, NAME('WideCharToMultiByte')
 
      winapi::GetLastError(),lONG,PASCAL,NAME('GetLastError')
    END

    MODULE('sqlite3 api')
      sqlite3_open(*CSTRING filename, *sqlite3 ppDb), LONG, RAW, C
      sqlite3_close(sqlite3 pDb), LONG, C, PROC
      sqlite3_errcode(sqlite3 pDb), LONG, C
      sqlite3_errmsg(sqlite3 pDb), *CSTRING, C
      sqlite3_create_function(sqlite3 pDb, *CSTRING zFunctionName, LONG nArg, LONG eTextRep, LONG pApp, SQLITE_XFUNC_PROTO xFunc, LONG xStep, LONG xFinal), LONG, RAW, C
 
      sqlite3_value_int(sqlite3_value value), LONG, C
      sqlite3_value_double(sqlite3_value value), REAL, C
      sqlite3_value_text(sqlite3_value value), *CSTRING, C
      sqlite3_value_blob(sqlite3_value value), LONG, C
      sqlite3_value_bytes(sqlite3_value value), LONG, C   !- Size of a BLOB or a UTF-8 TEXT result in bytes
      sqlite3_value_type(sqlite3_value value), SQLITE_DATATYPE, C

      sqlite3_result_double(sqlite3_context ctx, REAL value), C
      sqlite3_result_int(sqlite3_context ctx, LONG value), C
      sqlite3_result_null(sqlite3_context ctx), C
      sqlite3_result_text(sqlite3_context ctx, *CSTRING value, LONG length, LONG xDel), C

      sqlite3_exec(sqlite3 pDb, *CSTRING pSql, LONG pCallback, LONG pObject, LONG pErrMsg), LONG, RAW, C
      sqlite3_progress_handler(sqlite3 pDb, LONG nOps, LONG xProgress, LONG pArg), C

      sqlite3_prepare_v2(sqlite3 pDb, *CSTRING pSql, Long nByte, *sqlite3_stmt pStmt, LONG pTail), LONG, RAW, C
      sqlite3_step(sqlite3_stmt pStmt), LONG, C
      sqlite3_stmt_busy(sqlite3_stmt pStmt), LONG, C
      sqlite3_reset(sqlite3_stmt pStmt), LONG, C
      sqlite3_finalize(sqlite3_stmt pStmt), LONG, C, PROC

      !- sqlite3_bind_ functions
      !- https://www.sqlite.org/c3ref/bind_blob.html
      sqlite3_bind_blob(sqlite3_stmt pStmt, LONG pIndex, LONG zData, LONG nData, LONG xDel), LONG, C
      sqlite3_bind_double(sqlite3_stmt pStmt, LONG pIndex, REAL rValue), LONG, C
      sqlite3_bind_int(sqlite3_stmt pStmt, LONG pIndex, LONG iValue), LONG, C
      sqlite3_bind_null(sqlite3_stmt pStmt, LONG pIndex), LONG, C
      sqlite3_bind_text(sqlite3_stmt pStmt, LONG pIndex, *CSTRING zData, LONG nData, LONG xDel), LONG, RAW, C
      sqlite3_bind_text(sqlite3_stmt pStmt, LONG pIndex, LONG zData, LONG nData, LONG xDel), LONG, RAW, C
      !!!

      sqlite3_free(LONG ptr), C
    END

    !- CWUTIL
    INCLUDE('CWUTIL.INC'),ONCE

    sqlite::ConvertEncoding(STRING pInput, UNSIGNED pInputCodepage, UNSIGNED pOutputCodepage), STRING, PRIVATE
    sqlite::FromUtf8(STRING pInput, UNSIGNED pCodepage = CP_ACP), STRING, PRIVATE
    sqlite::ToUtf8(STRING pInput, UNSIGNED pCodepage = CP_ACP), STRING, PRIVATE

    sqlite::GetValueItem(LONG argnum, sqlite3_value argv), LONG, PRIVATE

    sqlite::ExecCB_Default(LONG pObject, LONG argc, LONG argv, LONG argn), LONG, C, PRIVATE
    sqlite::Progress_Default(LONG pObject), LONG, C, PRIVATE

    sqlite::AddToResultSet(LONG argv, LONG argn, LONG pIndex, *GROUP pGrp), PRIVATE

    INCLUDE('printf.inc'),ONCE
  END

!!!region Encodings
sqlite::ConvertEncoding       PROCEDURE(STRING pInput, UNSIGNED pInputCodepage, UNSIGNED pOutputCodepage)
szInput                         CSTRING(LEN(pInput) + 1)
UnicodeText                     CSTRING(LEN(pInput)*2+2)
DecodedText                     CSTRING(LEN(pInput)*2+2)
Len                             LONG, AUTO
  CODE
  IF NOT pInput
    RETURN ''
  END
  
  szInput = pInput
  !- get length of UnicodeText in characters
  Len = winapi::MultiByteToWideChar(pInputCodePage, 0, ADDRESS(szInput), -1, 0, 0)
  IF Len = 0
    printd('MultiByteToWideChar failed, error %i', winapi::GetLastError())
    RETURN ''
  END
  !- get UnicodeText terminated by <0,0>
  winapi::MultiByteToWideChar(pInputCodePage, 0, ADDRESS(szInput), -1, ADDRESS(UnicodeText), Len)
  
  !- get length of DecodedText in bytes
  Len = winapi::WideCharToMultiByte(pOutputCodePage, 0, ADDRESS(UnicodeText), -1, 0, 0, 0, 0)
  IF Len = 0
    printd('WideCharToMultiByte failed, error %i', winapi::GetLastError())
    RETURN ''
  END
  winapi::WideCharToMultiByte(pOutputCodePage, 0, ADDRESS(UnicodeText), -1, ADDRESS(DecodedText), Len, 0, 0)
  RETURN DecodedText

sqlite::FromUtf8              PROCEDURE(STRING pInput, UNSIGNED pCodepage = CP_ACP)
  CODE
  RETURN sqlite::ConvertEncoding(pInput, CP_UTF8, pCodepage)
  
sqlite::ToUtf8                PROCEDURE(STRING pInput, UNSIGNED pCodepage = CP_ACP)
  CODE
  RETURN sqlite::ConvertEncoding(pInput, pCodepage, CP_UTF8)
!!!endregion

!!!region SQLITE C INTERFACE
sqlite::GetValueItem          PROCEDURE(LONG argnum, sqlite3_value argv)
  CODE
  RETURN argv + (argnum - 1) * sqlite3_value::size
  
sqlite::value_int             PROCEDURE(LONG argnum, sqlite3_value argv)
valadr                          LONG, AUTO
  CODE
  valadr = sqlite::GetValueItem(argnum, argv)
  RETURN sqlite3_value_int(valadr)
  
sqlite::value_double          PROCEDURE(LONG argnum, sqlite3_value argv)
valadr                          LONG, AUTO
  CODE
  valadr = sqlite::GetValueItem(argnum, argv)
  RETURN sqlite3_value_double(valadr)
  
sqlite::value_text            PROCEDURE(LONG argnum, sqlite3_value argv)
valadr                          LONG, AUTO
  CODE
  valadr = sqlite::GetValueItem(argnum, argv)
  RETURN sqlite3_value_text(valadr)
  
sqlite::value_blob            PROCEDURE(LONG argnum, sqlite3_value argv)
valadr                          LONG, AUTO
bytes                           LONG, AUTO
blobref                         &STRING
  CODE
  valadr = sqlite::GetValueItem(argnum, argv)
  bytes = sqlite3_value_bytes(valadr)
  IF bytes > 0
    blobref &= NEW STRING(bytes)
    winapi::memcpy(ADDRESS(blobref), sqlite3_value_blob(valadr), bytes)
  END
  
  RETURN blobref

sqlite::value_type            PROCEDURE(LONG argnum, sqlite3_value argv)
valadr                          LONG, AUTO
  CODE
  valadr = sqlite::GetValueItem(argnum, argv)
  RETURN sqlite3_value_type(valadr)
  
sqlite::result_double         PROCEDURE(sqlite3_context ctx, REAL value)
  CODE
  sqlite3_result_double(ctx, value)
  
sqlite::result_int            PROCEDURE(sqlite3_context ctx, LONG value)
  CODE
  sqlite3_result_int(ctx, value)

sqlite::result_null           PROCEDURE(sqlite3_context ctx)
  CODE
  sqlite3_result_null(ctx)

sqlite::result_text           PROCEDURE(sqlite3_context ctx, STRING value)
szvalue                         CSTRING(LEN(value)*2)
  CODE
  szvalue = CLIP(sqlite::ToUtf8(value))
  sqlite3_result_text(ctx, szvalue, -1, SQLITE_STATIC)
  
sqlite::bind_text             PROCEDURE(sqlite3_stmt stmt, LONG pIndex, STRING pText)
szvalue                         CSTRING(LEN(pText)*2)
  CODE
  szvalue = CLIP(sqlite::ToUtf8(pText))
  RETURN sqlite3_bind_text(stmt, pIndex, szvalue, -1, SQLITE_TRANSIENT)
  
sqlite::ExecCB_Default        PROCEDURE(LONG pObject, LONG argc, LONG argv, LONG argn)
object                          &TCallbackObject
  CODE
  IF pObject AND argc > 0
    object &= (pObject)
    IF NOT object &= NULL AND NOT object.ThisSqlite &= NULL
      RETURN object.ThisSqlite.ExecCB(argc, argv, argn, object)
    END
  END
  
  RETURN SQLITE_OK
  
sqlite::Progress_Default      PROCEDURE(LONG pObject)
this                            &TSqliteBase
  CODE
  IF pObject
    this &= (pObject)
    IF NOT this &= NULL
      RETURN this.Progress()
    END
  END
  
  RETURN SQLITE_OK
  
sqlite::array_element         PROCEDURE(LONG pArray, LONG pIndex)
itemAddress                     &LONG
itemValue                       &CSTRING
  CODE
  itemAddress &= (pArray + (pIndex-1)*4)
  itemValue &= (itemAddress)
  RETURN itemValue

sqlite::AddToResultSet        PROCEDURE(LONG argv, LONG argn, LONG pIndex, *GROUP pGrp)
szColumnName                    &CSTRING
szColumnText                    &CSTRING
fldRef                          ANY
fldNdx                          LONG, AUTO
  CODE
  !- column name
  szColumnName &= sqlite::array_element(argn, pIndex)
  !- column value (text)
  szColumnText &= sqlite::array_element(argv, pIndex)

  !- find a group field with a name = szColumnName and assign a value
  LOOP fldNdx = 1 TO 999
    fldRef &= WHAT(pGrp, fldNdx)
    IF fldRef &= NULL
      !end of field list
      RETURN
    END
 
    IF UPPER(sqlite::FromUtf8(szColumnName)) = WHO(pGrp, fldNdx)
      IF NOT szColumnText &= NULL
        fldRef = sqlite::FromUtf8(szColumnText)
      ELSE
        fldRef = '' !- NULL
      END
      
      RETURN
    END
  END
!!!endregion

!!!region TSqliteBase
TSqliteBase.Construct         PROCEDURE()
  CODE
  SELF.ProgressAddress = ADDRESS(sqlite::Progress_Default)
  SELF.ProgressInterval = 1000
  
TSqliteBase.Destruct          PROCEDURE()
  CODE
  IF SELF.db
    SELF.Close()
  END
  
TSqliteBase.Open              PROCEDURE(STRING filename)
szfilename                      CSTRING(256)
rc                              SQLITE_Result_Code, AUTO
  CODE
  IF filename = SQLITE_DB_TEMPORARY OR filename = SQLITE_DB_MEMORY
    !- temp db or in-memory
    szfilename = filename
  ELSE
    szfilename = sqlite::ToUtf8(LONGPATH(filename))
  END
  
  rc = sqlite3_open(szfilename, SELF.db)
!  IF rc = SQLITE_OK
!    SELF.Exec('PRAGMA encoding = "UTF-8";') 
!  END
  
  RETURN CHOOSE(rc = SQLITE_OK)

TSqliteBase.Close             PROCEDURE()
  CODE
  sqlite3_close(SELF.db)

TSqliteBase.ErrCode           PROCEDURE()
  CODE
  RETURN sqlite3_errcode(SELF.db)

TSqliteBase.ErrMsg            PROCEDURE()
  CODE
  RETURN sqlite3_errmsg(SELF.db)

TSqliteBase.ExecQuery         PROCEDURE(STRING pSql, LONG lpObject)
sql                             CSTRING(LEN(pSql) * 2)  !- sql query in UTF-8
cbAddress                       LONG, AUTO              !- callback address
rc                              SQLITE_Result_Code, AUTO
  CODE
  sql = CLIP(sqlite::ToUtf8(pSql))
  
  IF lpObject
    cbAddress = ADDRESS(sqlite::ExecCB_Default)
  ELSE
    !- Not interested in result set
    cbAddress = 0
  END

  sqlite3_progress_handler(SELF.db, SELF.ProgressInterval, SELF.ProgressAddress, ADDRESS(SELF))
  rc = sqlite3_exec(SELF.db, sql, cbAddress, lpObject, 0)
  sqlite3_progress_handler(SELF.db, 0, 0, 0)
  
  IF rc = SQLITE_OK OR rc = SQLITE_ABORT
    !- SQLITE_ABORT actually is not an error, we just read only first row and abort the callback
    RETURN TRUE
  END
  
  RETURN FALSE
  
TSqliteBase.Exec              PROCEDURE(STRING pSql)
cbObject                        LIKE(TCallbackObject)
  CODE
  cbObject.ThisSqlite &= SELF
  RETURN SELF.ExecQuery(pSql, ADDRESS(cbObject))

TSqliteBase.Exec              PROCEDURE(STRING pSql, *? pScalar)
cbObject                        LIKE(TCallbackObject)
  CODE
  cbObject.ThisSqlite &= SELF
  cbObject.V &= pScalar
  RETURN SELF.ExecQuery(pSql, ADDRESS(cbObject))

TSqliteBase.Exec              PROCEDURE(STRING pSql, *GROUP pGrp)
cbObject                        LIKE(TCallbackObject)
  CODE
  cbObject.ThisSqlite &= SELF
  cbObject.G &= pGrp
  RETURN SELF.ExecQuery(pSql, ADDRESS(cbObject))

TSqliteBase.Exec              PROCEDURE(STRING pSql, *QUEUE pQue)
cbObject                        LIKE(TCallbackObject)
  CODE
  cbObject.ThisSqlite &= SELF
  cbObject.Q &= pQue
  RETURN SELF.ExecQuery(pSql, ADDRESS(cbObject))

TSqliteBase.Exec              PROCEDURE(STRING pSql, *FILE pFile)
cbObject                        LIKE(TCallbackObject)
  CODE
  cbObject.ThisSqlite &= SELF
  cbObject.F &= pFile
  RETURN SELF.ExecQuery(pSql, ADDRESS(cbObject))

TSqliteBase.ExecCB            PROCEDURE(LONG argc, LONG argv, LONG argn, *TCallbackObject pObject)
i                               LONG, AUTO
aRecord                         &GROUP
szScalar                        &CSTRING
  CODE
  IF NOT (argc = 0 OR (pObject.V &= NULL AND pObject.G &= NULL AND pObject.Q &= NULL AND pObject.F &= NULL))
    IF NOT pObject.V &= NULL
      !- scalar value as result set
      szScalar &= sqlite::array_element(argv, 1)
      pObject.V = szScalar
      RETURN SQLITE_ABORT
    ELSIF NOT pObject.G &= NULL
      !- group as result set
      CLEAR(pObject.G)
      aRecord &= pObject.G
    ELSIF NOT pObject.Q &= NULL
      !- queue as result set
      CLEAR(pObject.Q)
      aRecord &= pObject.Q
    ELSIF NOT pObject.F &= NULL
      !- file as result set
      CLEAR(pObject.F)
      aRecord &= pObject.F{PROP:Record}
    END
  
    LOOP i = 1 TO argc
      sqlite::AddToResultSet(argv, argn, i, aRecord)
    END
    
    IF NOT pObject.G &= NULL
      !- group as result set
      !- stop reading result set
      RETURN SQLITE_ABORT
    ELSIF NOT pObject.Q &= NULL
      !- queue as result set
      ADD(pObject.Q)
    ELSIF NOT pObject.F &= NULL
      !- file as result set
      ADD(pObject.F)
    END
  END

  RETURN SQLITE_OK
  
TSqliteBase.Progress          PROCEDURE()
  CODE
  RETURN SQLITE_OK
  
TSqliteBase.Prepare           PROCEDURE(STRING pSql)
sql                             CSTRING(LEN(pSql) * 2)  !- sql query in UTF-8
stmt                            sqlite3_stmt, AUTO
  CODE
  sql = CLIP(sqlite::ToUtf8(pSql))
  IF sqlite3_prepare_v2(SELF.db, sql, -1, stmt, 0) = SQLITE_OK
    RETURN stmt
  ELSE
    RETURN 0
  END
  
TSqliteBase.Step              PROCEDURE(sqlite3_stmt stmt)
  CODE
  RETURN sqlite3_step(stmt)
  
TSqliteBase.BindValue         PROCEDURE(sqlite3_stmt stmt, <? pValue>, LONG pIndex, BOOL pAsBlob = FALSE)
sBlobVal                        &STRING
nBlobSize                       LONG, AUTO
rc                              SQLITE_Result_Code, AUTO
  CODE
  IF OMITTED(pValue) OR CLIP(pValue) = ''
    RETURN sqlite3_bind_null(stmt, pIndex)
  END
    
  IF NOT pAsBlob
    IF ISSTRING(pValue)
      rc = sqlite::bind_text(stmt, pIndex, CLIP(pValue))
    ELSIF NUMERIC(pValue)
      IF pValue = INT(pValue)
        rc = sqlite3_bind_int(stmt, pIndex, pValue)
      ELSE
        rc = sqlite3_bind_double(stmt, pIndex, pValue)
      END
    ELSE
      rc = sqlite::bind_text(stmt, pIndex, CLIP(pValue))
    END
  ELSE
    nBlobSize = LEN(CLIP(pValue))
    sBlobVal &= NEW STRING(nBlobSize)
    sBlobVal = CLIP(pValue)
    rc = sqlite3_bind_blob(stmt, pIndex, ADDRESS(sBlobVal), nBlobSize, SQLITE_TRANSIENT)
    DISPOSE(sBlobVal)
  END
  
  RETURN rc
  
TSqliteBase.BindBlob          PROCEDURE(sqlite3_stmt stmt, *BLOB pBlob, LONG pIndex)
blobSize                        LONG, AUTO
blobData                        &STRING, AUTO
rc                              SQLITE_Result_Code, AUTO
  CODE
  IF NOT pBlob &= NULL AND pBlob{PROP:Size} > 0
    blobSize = pBlob{PROP:Size}
    blobData &= NEW STRING(blobSize)
    blobData = pBlob[0 : blobSize - 1]
    rc = sqlite3_bind_blob(stmt, pIndex, ADDRESS(blobData), blobSize, SQLITE_TRANSIENT)
    DISPOSE(blobData)
  ELSE
    rc = sqlite3_bind_null(stmt, pIndex)
  END
  
  RETURN rc
  
TSqliteBase.BindReset         PROCEDURE(sqlite3_stmt stmt)
  CODE
  RETURN sqlite3_reset(stmt)
  
TSqliteBase.Finalize          PROCEDURE(sqlite3_stmt stmt)
  CODE
  RETURN sqlite3_finalize(stmt)
  
TSqliteBase.BindEntity        PROCEDURE(sqlite3_stmt stmt, *TEntity entity)
fldNdx                          LONG, AUTO
fldValue                        ANY
rescode                         SQLITE_Result_Code, AUTO
  CODE
  !- for each field in entity
  LOOP fldNdx = 1 TO entity.FieldCount()
    !- locate a field
    entity.GetByIndex(fldNdx)
    !- read field value
    fldValue = entity.FieldValue()
    !- bind a value
    rescode = SELF.BindValue(stmt, fldValue, fldNdx, entity.IsBlob())
    !- check result
    IF rescode <> SQLITE_OK
      printd('BindValue(%i) failed: %i', fldNdx, rescode)
      RETURN FALSE
    END
  END

  RETURN TRUE

TSqliteBase.UDF               PROCEDURE(STRING funcName, LONG nArgs, SQLITE_XFUNC_PROTO xFunc)
szFuncname                      CSTRING(LEN(CLIP(funcName)) + 1)
rc                              SQLITE_Result_Code, AUTO
  CODE
  szFuncname = CLIP(funcName)
  rc = sqlite3_create_function(SELF.db, szFuncname, nArgs, SQLITE_UTF8, ADDRESS(SELF), xFunc, 0, 0)
  RETURN CHOOSE(rc = SQLITE_OK)

TSqliteBase.CreateTable       PROCEDURE(*FILE f, BOOL pWithBlobs = FALSE)
entity                          TFileEntity
  CODE
  entity.Init(f,,pWithBlobs)  
  SELF.Exec(entity.DropQuery())
  RETURN SELF.Exec(entity.CreateQuery())
  
TSqliteBase.CreateTable       PROCEDURE(*QUEUE q, STRING pTblName)
entity                          TQueueEntity
  CODE
  entity.Init(q, pTblName)  
  SELF.Exec(entity.DropQuery())
  RETURN SELF.Exec(entity.CreateQuery())

TSqliteBase.CloneTable        PROCEDURE(*FILE f, BOOL pWithBlobs = FALSE)
entity                          TFileEntity
stmt                            sqlite3_stmt, AUTO
doCloseFile                     BOOL(FALSE)
rescode                         SQLITE_Result_Code, AUTO
  CODE
  entity.Init(f,,pWithBlobs)  
  
  !- create table if not exist
  SELF.Exec(entity.CreateQuery(TRUE))
  
  !- begin transaction
  SELF.Exec('BEGIN TRANSACTION')
  
  stmt = SELF.Prepare(entity.PrepareInsertQuery())
  IF stmt = 0
    printd('Prepare failed:')
    printd(entity.PrepareInsertQuery())
    RETURN FALSE
  END
  
  IF STATUS(f) = 0
    OPEN(f, 40h)  !- Read Only/Deny None
    IF ERRORCODE()
      RETURN ''
    END

    doCloseFile = TRUE
  END

  SET(f)  !- sort by physical order
  LOOP
    NEXT(f)
    IF ERRORCODE()
      BREAK
    END

    IF SELF.BindEntity(stmt, entity)
      rescode = SELF.Step(stmt)
      IF rescode <> SQLITE_DONE
        printd('Step failed: %i', rescode)
      END
    END
    
    SELF.BindReset(stmt)
  END
  
  IF doCloseFile
    CLOSE(f)
  END
  
  SELF.Finalize(stmt)
  
  !- commit transaction
  SELF.Exec('COMMIT TRANSACTION')
  
  RETURN TRUE
  
TSqliteBase.CloneTable        PROCEDURE(*QUEUE q, STRING pTblName)
entity                          TQueueEntity
stmt                            sqlite3_stmt, AUTO
i                               LONG, AUTO
rescode                         SQLITE_Result_Code, AUTO
  CODE
  entity.Init(q, pTblName)
  
  !- create table if not exist
  SELF.Exec(entity.CreateQuery(TRUE))

  !- begin transaction
  SELF.Exec('BEGIN TRANSACTION')
  
  stmt = SELF.Prepare(entity.PrepareInsertQuery())
  IF stmt = 0
    printd('Prepare failed:')
    printd(entity.PrepareInsertQuery())
    RETURN FALSE
  END

  LOOP i = 1 TO RECORDS(q)
    GET(q, i)

    IF SELF.BindEntity(stmt, entity)
      rescode = SELF.Step(stmt)
      IF rescode <> SQLITE_DONE
        printd('Step failed: %i', rescode)
      END
    END
    
    SELF.BindReset(stmt)
  END
  
  SELF.Finalize(stmt)
  
  !- commit transaction
  SELF.Exec('COMMIT TRANSACTION')
  
  RETURN TRUE

TSqliteBase.DropTable         PROCEDURE(*FILE f)
entity                          TFileEntity
  CODE
  entity.Init(f)
  RETURN SELF.Exec(entity.DropQuery())

TSqliteBase.DropTable         PROCEDURE(STRING pTblName)
  CODE
  RETURN SELF.Exec(printf('DROP TABLE IF EXISTS %s;', pTblName))

TSqliteBase.InsertIntoTable   PROCEDURE(*FILE f, BOOL pWithBlobs = FALSE)
entity                          TFileEntity
  CODE
  entity.Init(f,,pWithBlobs)
  RETURN SELF.Exec(entity.InsertQuery())
  
TSqliteBase.InsertIntoTable   PROCEDURE(*QUEUE q, STRING pTblName)
entity                          TQueueEntity
  CODE
  entity.Init(q, pTblName)
  RETURN SELF.Exec(entity.InsertQuery())

TSqliteBase.InsertRecord      PROCEDURE(*FILE f)
entity                          TFileEntity
  CODE
  entity.Init(f,,TRUE)
  RETURN SELF.Exec(entity.InsertRecordQuery())

TSqliteBase.InsertRecord      PROCEDURE(*GROUP g, STRING pTblName)
entity                          TGroupEntity
  CODE
  entity.Init(g, pTblName)
  RETURN SELF.Exec(entity.InsertRecordQuery())

TSqliteBase.UpdateRecord      PROCEDURE(*FILE f, STRING pWhere)
entity                          TFileEntity
  CODE
  entity.Init(f,,TRUE)
  RETURN SELF.Exec(entity.UpdateRecordQuery(pWhere))

TSqliteBase.UpdateRecord      PROCEDURE(*GROUP g, STRING pTblName, STRING pWhere)
entity                          TGroupEntity
  CODE
  entity.Init(g, pTblName)
  RETURN SELF.Exec(entity.UpdateRecordQuery(pWhere))

TSqliteBase.DeleteRecord      PROCEDURE(*FILE f, STRING pWhere)
entity                          TFileEntity
  CODE
  entity.Init(f)
  RETURN SELF.Exec(entity.DelereRecordQuery(pWhere))

TSqliteBase.DeleteRecord      PROCEDURE(STRING pTblName, STRING pWhere)
  CODE
  RETURN SELF.Exec(printf('DELETE FROM %s WHERE %s;', pTblName, pWhere))
!!!endregion
  
!!!region TSqliteViewer
TSqliteViewer.Construct       PROCEDURE()
  CODE
  SELF.ColNames &= NEW TCBNames
  SELF.ColValues &= NEW TCBValues
  SELF.columnFormat = '42L(2)|M~%s~L(1)@s255@'
  
TSqliteViewer.Destruct        PROCEDURE()
  CODE
  FREE(SELF.ColNames)
  DISPOSE(SELF.ColNames)
  FREE(SELF.ColValues)
  DISPOSE(SELF.ColValues)
  
TSqliteViewer.SetListbox      PROCEDURE(SIGNED pFeq)
  CODE
  ASSERT(pFeq{PROP:Type} = CREATE:list)
  IF pFeq{PROP:Type} = CREATE:list
    SELF.nChanges = CHANGES(SELF.ColValues)

    SELF.listFeq = pFeq
    SELF.listFeq{PROP:VLBVal} = ADDRESS(SELF)
    SELF.listFeq{PROP:VLBProc} = ADDRESS(SELF.VLBproc)
  END
  
TSqliteViewer.ExecQuery       PROCEDURE(STRING pSql, LONG lpObject)
rc                              BOOL, AUTO
  CODE
  SETCURSOR(CURSOR:Wait)
  FREE(SELF.ColNames)
  FREE(SELF.ColValues)
  SELF.listFeq{PROP:Format} = ''
  rc = PARENT.ExecQuery(pSql, lpObject)
  SETCURSOR()
  RETURN rc

TSqliteViewer.ExecCB          PROCEDURE(LONG argc, LONG argv, LONG argn, *TCallbackObject pObject)
i                               LONG, AUTO
szColumnName                    &CSTRING
szColumnText                    &CSTRING
  CODE
  IF argc > 0
    IF RECORDS(SELF.ColNames) = 0
      !- first time save column names
      LOOP i = 1 TO argc
        szColumnName &= sqlite::array_element(argn, i)
        SELF.ColNames.ColName = sqlite::FromUtf8(szColumnName)
        ADD(SELF.ColNames)
      END
    END

    LOOP i = 1 TO argc
      !- save column values
      szColumnText &= sqlite::array_element(argv, i)
      SELF.ColValues.ColValue = sqlite::FromUtf8(szColumnText)
!      printd('szColumnText=%s', szColumnText)
!      printd('ColValue    =%s', SELF.ColValues.ColValue)
      ADD(SELF.ColValues)
    END
  END
  
  RETURN SQLITE_OK

TSqliteViewer.VLBproc         PROCEDURE(LONG row, SHORT column)
nChanges                        LONG, AUTO
nColumns                        LONG, AUTO
qIndex                          LONG, AUTO
lstFormat                       ANY
  CODE
  nColumns = RECORDS(SELF.ColNames)
  IF nColumns AND SELF.listFeq{PROP:Format} = ''
    LOOP qIndex = 1 TO nColumns
      GET(SELF.ColNames, qIndex)
      lstFormat = CLIP(lstFormat) & printf(SELF.columnFormat, SELF.ColNames.ColName)
    END
!    printd(lstFormat)
    SELF.listFeq{PROP:Format} = CLIP(lstFormat)
  END
  
  CASE row
  OF -1                    ! How many rows?
    RETURN RECORDS(SELF.ColValues) / nColumns
  OF -2                    ! How many columns?
    IF nColumns
      RETURN nColumns
    ELSE
      RETURN 1
    END
  OF -3                    ! Has it changed
    nChanges = CHANGES(SELF.ColValues)
    IF nChanges <> SELF.nChanges
      SELF.nChanges = nChanges
      RETURN 1
    ELSE
      RETURN 0
    END
  ELSE
    GET(SELF.ColValues, (row-1) * nColumns + column)
    IF NOT ERRORCODE()
      RETURN CLIP(SELF.ColValues.ColValue)
    ELSE
      RETURN ''
    END
  END
  
TSqliteViewer.PrintIt         PROCEDURE()
VLBReport                       REPORT,AT(229,531,11240,7531),FONT('Arial',10,,FONT:regular),PRE(VLBReport), |
                                  LANDSCAPE,PAPER(PAPER:A4),THOUS
                                  HEADER,AT(229,208,11240,344),FONT(,12),USE(?Header)
                                    LINE,AT(52,240,11156,0),USE(?LINE1)
                                  END
Detail                            DETAIL,AT(0,0,11240,427),USE(?Detail)
                                  END
Separator                         DETAIL,AT(0,0,11240,100),USE(?Separator)
                                    LINE,AT(42,42,11156,0),USE(?LINE2)
                                  END
                                END
nRows                           LONG, AUTO
nColumns                        LONG, AUTO
startFeq                        EQUATE(501)
feq                             SIGNED, AUTO
xpos                            UNSIGNED, AUTO
w                               UNSIGNED, AUTO
cIndex                          LONG, AUTO
rIndex                          LONG, AUTO
  CODE
  nColumns = RECORDS(SELF.ColNames)
  IF nColumns = 0
    RETURN
  END
  nRows = RECORDS(SELF.ColValues) / nColumns
  IF nRows = 0
    RETURN
  END

  OPEN(VLBReport)
  
  !- set target to the report
  SETTARGET(VLBReport)

  !- width of each column
  w = VLBReport{PROP:Width} / nColumns
  
  !- create header controls
  xpos = 0
  LOOP cIndex = 1 TO nColumns
    GET(SELF.ColNames, cIndex)
    feq = CREATE(0, CREATE:string, ?Header)
    feq{PROP:Text} = SELF.ColNames.ColName
    feq{PROP:FontStyle} = FONT:bold
    SETPOSITION(feq, xpos, , w)
    UNHIDE(feq)
    xpos += w
  END
  
  !- create detail
  SETTARGET(VLBReport, ?Detail)

  !- detail controls get feq numbers from "startFeq" value
  xpos = 0
  feq = startFeq
  LOOP cIndex = 1 TO nColumns
    GET(SELF.ColNames, cIndex)
    feq = CREATE(feq, CREATE:text, ?Detail)
    SETPOSITION(feq, xpos, , w)
    UNHIDE(feq)
    xpos += w
    feq += 1
  END

  !- print
  LOOP rIndex = 1 TO nRows
    feq = startFeq
    LOOP cIndex = 1 TO nColumns
      GET(SELF.ColValues, (rIndex-1) * nColumns + cIndex)
      feq{PROP:Text} = SELF.ColValues.ColValue
      feq += 1
    END
    PRINT(VLBReport:Detail)
    PRINT(VLBReport:Separator)
  END
  
  !- reset target
  SETTARGET()
  
  CLOSE(VLBReport)

!!!endregion
  
!!!region TBRWSQLFilter
TBRWSQLFilter.Construct       PROCEDURE()
  CODE
  SELF.FieldPairs &= NEW FieldPairsClass
  SELF.FieldPairs.Init()
  
TBRWSQLFilter.Destruct        PROCEDURE()
  CODE
  SELF.FieldPairs.Kill()
  DISPOSE(SELF.FieldPairs)

TBRWSQLFilter.Init            PROCEDURE(FILE pFile, KEY pKey, QUEUE pFilterQ, STRING pSelectStmt)
Rec                             &GROUP
nComps                          BYTE, AUTO
kIndex                          BYTE, AUTO  !- field index in KEY
fIndex                          BYTE, AUTO  !- field index in RECORD
fRef                            ANY
qRef                            ANY
  CODE
  SELF.FilterQ &= pFilterQ
  SELF.SelectStatement = CLIP(pSelectStmt)
  
  Rec &= pFile{PROP:Record}
  nComps = pKey{PROP:Components}
  LOOP kIndex = 1 TO nComps
    fIndex = pKey{PROP:Field, kIndex}
    fRef &= WHAT(Rec, fIndex)
    qRef &= WHAT(SELF.FilterQ, kIndex)
    SELF.FieldPairs.AddPair(fRef, qRef)
  END

TBRWSQLFilter.ValidateRecord  PROCEDURE()
entity                          TQueueEntity
  CODE
  IF RECORDS(SELF.FilterQ)
    !- copy file fields to queue fields
    SELF.FieldPairs.AssignLeftToRight()
    !- check if queue contains file record
    entity.Init(SELF.FilterQ)
    GET(SELF.FilterQ, entity.FieldNames())
    RETURN CHOOSE(NOT ERRORCODE())
  ELSE
    !- empty FilterQ means either sql query returned empty result set, or no query was executed yet.
    RETURN CHOOSE(NOT SELF.IsActive)
  END

TBRWSQLFilter.ApplyFilter     PROCEDURE(STRING pWhere)
  CODE
  SELF.IsActive = TRUE
  FREE(SELF.FilterQ)
  IF NOT pWhere
    !- filter not defined
    RETURN TRUE
  END
  
  IF SELF.Exec(printf(CLIP(SELF.SelectStatement) &' WHERE %s;', pWhere), SELF.FilterQ)
    RETURN TRUE
  END
  
  RETURN FALSE
  
TBRWSQLFilter.ResetFilter     PROCEDURE()
  CODE
  FREE(SELF.FilterQ)
  SELF.IsActive = FALSE
!!!endregion
  
