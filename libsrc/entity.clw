!** sqlite for Clarion
!** 20.10.2019
!** mikeduglas66@gmail.com

  MEMBER

  INCLUDE('entity.inc')

  MAP
    
    GetFileName(*FILE pFile), STRING, PRIVATE       !- returns file name w/o extension (from {PROP:Name})
    RemovePrefix(*STRING fldName), PRIVATE          !- removes first ':' from field name
    RemoveAttributes(*STRING fldName), PRIVATE      !- removes all attributes from name (WHO() may return field name with attributes like "|READONLY", "|BINARY") 
    NormalizeName(*STRING fldName), PRIVATE         !- removes prefix and attributes from field name
    FieldCount(*GROUP pGrp), LONG, PRIVATE          !- returns a number of fields in the group
    FieldPrintableValue(*? fldRef), STRING, PRIVATE !- returns printable field value (strings are in quotes)
    StringToHex(STRING pData), STRING, PRIVATE      !- converts each byte to hex value
    IsTextData(STRING pData), BOOL, PRIVATE         !- returns true if data contains only text characters

    INCLUDE('CWUTIL.INC'),ONCE
    INCLUDE('printf.inc'),ONCE
  END

TEntityFieldQ                 QUEUE, TYPE
Name                            STRING(256)
Pos                             LONG          !- position in entity, negative for blob/memo
ArrayNdx                        LONG          !- position in array, 0 if not in array
                              END

!!!region static functions
GetFileName                   PROCEDURE(*FILE f)
szPath                          CSTRING(256)
szName                          CSTRING(256)
  CODE
  szPath = f{PROP:Name}
  PathSplit(szPath, , , szName)
  RETURN szName

RemovePrefix                  PROCEDURE(*STRING fldName)
first_colon_pos                 LONG, AUTO
  CODE
  first_colon_pos = INSTRING(':', fldName, 1, 1)
  IF first_colon_pos
    fldName = SUB(fldName, first_colon_pos + 1, LEN(fldName))
  END

RemoveAttributes              PROCEDURE(*STRING fldName)
first_pipe_pos                  LONG, AUTO
  CODE
  first_pipe_pos = INSTRING('|', fldName, 1, 1)
  IF first_pipe_pos > 1
    fldName = SUB(fldName, 1, first_pipe_pos - 1)
  END

NormalizeName                 PROCEDURE(*STRING fldName)
  CODE
  RemovePrefix(fldName)
  RemoveAttributes(fldName)
  
FieldCount                    PROCEDURE(*GROUP pGrp)
fldNdx                          LONG, AUTO
fldRef                          ANY
nestedGrp                       &GROUP
nFields                         LONG(0)
  CODE
  LOOP fldNdx = 1 TO 9999
    fldRef &= WHAT(pGrp, fldNdx)
    IF fldRef &= NULL
      !end of group
      BREAK
    END
  
    IF ISGROUP(pGrp, fldNdx)
      !- recursively get number of fields from nested group
      nestedGrp &= GETGROUP(pGrp, fldNdx)
      nFields += FieldCount(nestedGrp)
    ELSE
      nFields += HOWMANY(pGrp, fldNdx)
    END
  END
  
  RETURN nFields

FieldPrintableValue           PROCEDURE(*? fldRef)
  CODE
  IF CLIP(fldRef) = ''
    RETURN 'NULL'
  END
  
  IF ISSTRING(fldRef)
    RETURN printf('%S', fldRef)
  ELSIF NUMERIC(fldRef)
    RETURN fldRef
  ELSE
    !neither STRING nor NUMERIC
    RETURN printf('%S', fldRef)
  END

StringToHex                   PROCEDURE(STRING pData)
i                               UNSIGNED, AUTO
sHex                            ANY
  CODE
  LOOP i = 1 TO LEN(CLIP(pData))
    sHex = CLIP(sHex) & printf('%x', VAL(pData[i]))
  END
    
  RETURN CLIP(sHex)
  
IsTextData                    PROCEDURE(STRING pData)
i                               UNSIGNED, AUTO
  CODE
  LOOP i = 1 TO LEN(CLIP(pData))
    CASE VAL(pData[i])
    OF 0 TO 7
      RETURN FALSE
    OF 11 TO 12
      RETURN FALSE
    OF 14 TO 31
      RETURN FALSE
    END
    
    IF i > 1000   !- if first 1000 chars are text symbols, assume entire data is text
      BREAK
    END
  END

  RETURN TRUE
!!!endregion
  
!!!region TEntity
TEntity.Construct             PROCEDURE()
  CODE
  SELF.fields &= NEW TEntityFieldQ
  
TEntity.Destruct              PROCEDURE()
  CODE
  FREE(SELF.fields)
  DISPOSE(SELF.fields)
  
TEntity.From                  PROCEDURE(*GROUP pGrp)
fldNdx                          LONG, AUTO
fldRef                          ANY
fldName                         STRING(256), AUTO
fldDim                          LONG, AUTO
dimNdx                          LONG, AUTO
nestedGrp                       &GROUP
  CODE
  LOOP fldNdx = 1 TO 9999
    fldRef &= WHAT(pGrp, fldNdx)
    IF fldRef &= NULL
      !end of group
      BREAK
    END
  
    fldName = WHO(pGrp, fldNdx)
    IF NOT fldName
      !- if noname field, assign something
      fldName = '_noname_'& FORMAT(fldNdx, @n02)
    END
 
    IF ISGROUP(pGrp, fldNdx)
      !- recursively add fields from nested group
      nestedGrp &= GETGROUP(pGrp, fldNdx)
      SELF.From(nestedGrp)
      fldNdx += FieldCount(nestedGrp)  !- skip fields from nested groups
    ELSE
      NormalizeName(fldName)

      fldDim = HOWMANY(pGrp, fldNdx)
      IF fldDim = 1
        !- add a field name to the list
        SELF.AddField(fldName, fldNdx)
      ELSE
        !- DIMmed field --> fieldname_1, fieldname_2...
        LOOP dimNdx = 1 TO fldDim
          SELF.AddField(fldName &'_'& dimNdx, fldNdx, dimNdx)
        END
      END
    END
  END

TEntity.From                  PROCEDURE(*FILE pFile)
Rec                             &GROUP
fldNdx                          LONG, AUTO
fldName                         STRING(256), AUTO
  CODE
  Rec &= pFile{PROP:Record}
  SELF.From(Rec)
  
  IF SELF.withBlobs
    LOOP fldNdx = 1 TO pFile{PROP:Memos} + pFile{PROP:Blobs}
      fldName = pFile{PROP:Label, -fldNdx}
      IF NOT fldName
        !- if noname field, assign something
        fldName = '_blob_'& FORMAT(fldNdx, @n02)
      END
    
      NormalizeName(fldName)

      SELF.AddField(fldName, -fldNdx, 0)
    END
  END

TEntity.AddField              PROCEDURE(STRING pName, LONG pPos, LONG pArrayNdx = 0)
  CODE
  CLEAR(SELF.fields)
  SELF.fields.Name = pName
  SELF.fields.Pos = pPos
  SELF.fields.ArrayNdx = pArrayNdx
  ADD(SELF.fields)

TEntity.SetEntity             PROCEDURE(*GROUP pGrp, <STRING pName>)
  CODE
  SELF.thisGroup &= pGrp
  SELF.entityType = Entity:GROUP
  IF pName
    SELF.tableName = pName
  ELSE
    SELF.tableName = '_group_'& CLOCK()
  END
  
  FREE(SELF.fields)
  SELF.From(pGrp)

TEntity.SetEntity             PROCEDURE(*QUEUE pQue, <STRING pName>)
  CODE
  SELF.thisQueue &= pQue
  SELF.entityType = Entity:QUEUE
  IF pName
    SELF.tableName = pName
  ELSE
    SELF.tableName = '_queue_'& CLOCK()
  END
  
  FREE(SELF.fields)
  SELF.From(pQue)

TEntity.SetEntity             PROCEDURE(*FILE pFile,  <STRING pName>)
  CODE
  SELF.thisFile &= pFile
  SELF.entityType = Entity:FILE
  IF pName
    SELF.tableName = pName
  ELSE
    SELF.tableName = GetFileName(pFile)
  END

  FREE(SELF.fields)
  SELF.From(pFile)
  
TEntity.EntityName            PROCEDURE()
  CODE
  RETURN SELF.tableName

TEntity.FieldCount            PROCEDURE()
  CODE
  RETURN RECORDS(SELF.fields)

TEntity.GetByIndex            PROCEDURE(LONG pIndex)
  CODE
  GET(SELF.fields, pIndex)
  RETURN CHOOSE(ERRORCODE() = 0)
    
TEntity.GetByName             PROCEDURE(STRING pName)
  CODE
  SELF.fields.Name = pName
  NormalizeName(SELF.fields.Name)
  GET(SELF.fields, SELF.fields.Name)
  RETURN CHOOSE(ERRORCODE() = 0)
    
TEntity.GetByPos              PROCEDURE(LONG pPos)
  CODE
  SELF.fields.Pos = pPos
  GET(SELF.fields, SELF.fields.Pos)
  RETURN CHOOSE(ERRORCODE() = 0)

TEntity.FieldNames            PROCEDURE()
aList                           ANY
fldNdx                          LONG, AUTO
  CODE
  LOOP fldNdx = 1 TO RECORDS(SELF.fields)
    GET(SELF.fields, fldNdx)
    IF fldNdx > 1
      aList = CLIP(aList) &','
    END
    aList = CLIP(aList) & CLIP(SELF.fields.Name)
  END
  
  RETURN CLIP(aList)

TEntity.Placeholders          PROCEDURE()
aList                           ANY
fldNdx                          LONG, AUTO
  CODE
  LOOP fldNdx = 1 TO RECORDS(SELF.fields)
    GET(SELF.fields, fldNdx)
    IF fldNdx > 1
      aList = CLIP(aList) &','
    END
    aList = CLIP(aList) & '?'
  END
  
  RETURN CLIP(aList)

TEntity.GetRawValue           PROCEDURE(*GROUP pGrp)
fldRef                          ANY
  CODE
  IF SELF.fields.Pos > 0
    IF SELF.fields.ArrayNdx = 0
      fldRef &= WHAT(pGrp, SELF.fields.Pos)
    ELSE
      fldRef &= WHAT(pGrp, SELF.fields.Pos, SELF.fields.ArrayNdx)
    END
  END
  
  RETURN fldRef
  
TEntity.GetRawValue           PROCEDURE(*FILE pFile)
rec                             &GROUP
fldRef                          ANY
  CODE
  IF SELF.fields.Pos > 0
    rec &= pFile{PROP:Record}
    fldRef = WHAT(rec, SELF.fields.Pos)
  ELSE
    IF SELF.withBlobs
      !- blob
      fldRef = pFile{PROP:Value, SELF.fields.Pos}
    END
  END
  
  RETURN fldRef

TEntity.GetPrintableValue     PROCEDURE(*GROUP pGrp)
fldRef                          ANY
  CODE
  fldRef = SELF.GetRawValue(pGrp)
  RETURN FieldPrintableValue(fldRef)
  
TEntity.GetPrintableValue     PROCEDURE(*FILE pFile)
rec                             &GROUP
fldRef                          ANY
  CODE
  fldRef = SELF.GetRawValue(pFile)
  RETURN FieldPrintableValue(fldRef)
  
TEntity.KeyValues             PROCEDURE(*GROUP pGrp)
i                               LONG, AUTO
fldRef                          ANY
fldList                         ANY
  CODE  
  LOOP i = 1 TO RECORDS(SELF.fields)
    GET(SELF.fields, i)
    IF SELF.fields.Pos > 0
      !- not a BLOB
      
      fldRef &= WHAT(pGrp, SELF.fields.Pos)
 
      IF CLIP(fldList) <> ''
        fldList = CLIP(fldList) &','
      END
      fldList = CLIP(fldList) & printf('%s=%s', SELF.fields.Name, FieldPrintableValue(fldRef))
    END
  END
  
  RETURN CLIP(fldList)

TEntity.KeyValues             PROCEDURE(*FILE pFile)
rec                             &GROUP
i                               LONG, AUTO
blobVal                         ANY
fldList                         ANY
  CODE
  rec &= pFile{PROP:Record}
  fldList = SELF.KeyValues(rec)
  
  IF SELF.withBlobs
    LOOP i = 1 TO RECORDS(SELF.fields)
      GET(SELF.fields, i)
      IF SELF.fields.Pos < 0
        blobVal = pFile{PROP:Value, SELF.fields.Pos}

        IF CLIP(fldList) <> ''
          fldList = CLIP(fldList) &','
        END
        fldList = CLIP(fldList) & printf('%s=x%S', SELF.fields.Name, StringToHex(blobVal))
      END
    END
  END
  
  RETURN CLIP(fldList)
  
TEntity.Values                PROCEDURE(*GROUP pGrp)
i                               LONG, AUTO
vlist                           ANY
  CODE
  LOOP i = 1 TO SELF.FieldCount()
    SELF.GetByIndex(i)
    IF SELF.fields.Pos > 0
      !- not a BLOB
      IF CLIP(vlist) <> ''
        vlist = CLIP(vlist) &','
      END
      vlist = CLIP(vlist) & SELF.GetPrintableValue(pGrp)
    END
  END

  RETURN '('& CLIP(vlist) &')'
  
TEntity.Values                PROCEDURE(*QUEUE pQue, BOOL pAllRecs)
grp                             &GROUP
qIndex                          LONG, AUTO
vlist                           ANY
  CODE
  IF pAllRecs
    LOOP qIndex = 1 TO RECORDS(pQue)
      GET(pQue, qIndex)
      grp = pQue
    
      IF CLIP(vlist) <> ''
        vlist = CLIP(vlist) &','
      END
      vlist = CLIP(vlist) & SELF.Values(grp)
    END
  ELSE
    !- current record
    grp = pQue
    vlist = SELF.Values(grp)
  END
  
  RETURN CLIP(vlist)

TEntity.Values                PROCEDURE(*FILE pFile, BOOL pAllRecs)
doCloseFile                     BOOL(FALSE)
i                               LONG, AUTO
rlist                           ANY
vlist                           ANY
  CODE
  IF pAllRecs
    IF STATUS(pFile) = 0
      OPEN(pFile, 40h)  !- Read Only/Deny None
      IF ERRORCODE()
        RETURN ''
      END
    
      doCloseFile = TRUE
    END
  
    !- a list of values: (value1, value2...),(valuex, valuey...)...
    SET(pFile)  !- sort by physical order
    LOOP
      NEXT(pFile)
      IF ERRORCODE()
        BREAK
      END
    
      rlist = ''
    
      LOOP i = 1 TO SELF.FieldCount()
        SELF.GetByIndex(i)
        IF CLIP(rlist) <> ''
          rlist = CLIP(rlist) &','
        END
        rlist = CLIP(rlist) & SELF.GetPrintableValue(pFile)
      END
    
      rlist = '('& CLIP(rlist) &')'
  
      IF CLIP(vlist) <> ''
        vlist = CLIP(vlist) &','
      END
      vlist = CLIP(vlist) & rlist
    END
  
    IF doCloseFile
      CLOSE(pFile)
    END
  ELSE
    !- current record
    LOOP i = 1 TO SELF.FieldCount()
      SELF.GetByIndex(i)
      IF CLIP(vlist) <> ''
        vlist = CLIP(vlist) &','
      END
      vlist = CLIP(vlist) & SELF.GetPrintableValue(pFile)
    END
    
    vlist = '('& CLIP(vlist) &')'
  END
  
  RETURN CLIP(vlist)

TEntity.CreateQuery           PROCEDURE(BOOL pIfNotExist = FALSE)
  CODE
  RETURN printf('CREATE TABLE %s %s (%s);', CHOOSE(pIfNotExist = TRUE, 'IF NOT EXISTS', ''), SELF.tableName, SELF.FieldNames())

TEntity.DropQuery             PROCEDURE()
  CODE
  RETURN printf('DROP TABLE IF EXISTS %s;', SELF.tableName)
  
TEntity.InsertQuery           PROCEDURE(BOOL pAllRecs = TRUE)
  CODE
  CASE SELF.entityType
  OF Entity:GROUP
    RETURN printf('INSERT INTO %s (%s) VALUES %s;', SELF.tableName, SELF.FieldNames(), SELF.Values(SELF.thisGroup))
  OF Entity:QUEUE
    RETURN printf('INSERT INTO %s (%s) VALUES %s;', SELF.tableName, SELF.FieldNames(), SELF.Values(SELF.thisQueue, pAllRecs))
  OF Entity:FILE
    RETURN printf('INSERT INTO %s (%s) VALUES %s;', SELF.tableName, SELF.FieldNames(), SELF.Values(SELF.thisFile, pAllRecs))
  END
  
  RETURN ''
  
TEntity.PrepareInsertQuery    PROCEDURE()
  CODE
  RETURN printf('INSERT INTO %s (%s) VALUES (%s);', SELF.tableName, SELF.FieldNames(), SELF.Placeholders())
  
TEntity.InsertRecordQuery     PROCEDURE()
  CODE
  RETURN SELF.InsertQuery(FALSE)
  
TEntity.UpdateRecordQuery     PROCEDURE(STRING pWhere)
  CODE
  CASE SELF.entityType
  OF Entity:GROUP
    RETURN printf('UPDATE %s SET %s WHERE %s;', SELF.tableName, SELF.KeyValues(SELF.thisGroup), pWhere)
  OF Entity:QUEUE
    RETURN printf('UPDATE %s SET %s WHERE %s;', SELF.tableName, SELF.KeyValues(SELF.thisQueue), pWhere)
  OF Entity:FILE
    RETURN printf('UPDATE %s SET %s WHERE %s;', SELF.tableName, SELF.KeyValues(SELF.thisFile), pWhere)
  END
  
  RETURN ''
  
TEntity.DelereRecordQuery     PROCEDURE(STRING pWhere)
  CODE
  RETURN printf('DELETE FROM %s WHERE %s;', SELF.tableName, pWhere)
  
TEntity.FieldName             PROCEDURE()
  CODE
  RETURN SELF.fields.Name
  
TEntity.FieldValue            PROCEDURE()
v                               ANY
  CODE
  CASE SELF.entityType
  OF Entity:GROUP
    v = SELF.GetRawValue(SELF.thisGroup)
  OF Entity:QUEUE
    v = SELF.GetRawValue(SELF.thisQueue)
  OF Entity:FILE
    v = SELF.GetRawValue(SELF.thisFile)
  END
  
  RETURN v

TEntity.IsBlob                PROCEDURE()
  CODE
  RETURN CHOOSE(SELF.fields.Pos < 0)
!!!endregion
  
!!!region TGroupEntity
TGroupEntity.Init             PROCEDURE(*GROUP pGrp, <STRING pName>)
  CODE
  SELF.SetEntity(pGrp, pName)
!!!endregion
  
!!!region TQueueEntity
TQueueEntity.Init             PROCEDURE(*QUEUE pQue, <STRING pName>)
  CODE
  SELF.SetEntity(pQue, pName)
!!!endregion
    
!!!region TFileEntity
TFileEntity.Init              PROCEDURE(*FILE pFile, <STRING pName>, BOOL pWithBlobs = FALSE)
  CODE
  SELF.withBlobs = pWithBlobs
  SELF.SetEntity(pFile, pName)
!!!endregion