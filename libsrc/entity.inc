!** sqlite for Clarion
!** 20.10.2019
!** mikeduglas66@gmail.com

                              ITEMIZE
Entity:GROUP                    EQUATE(1)
Entity:QUEUE                    EQUATE(2)
Entity:FILE                     EQUATE(3)
                              END

TEntity                       CLASS, TYPE, MODULE('entity.clw'), LINK('entity.clw')
thisGroup                       &GROUP, PROTECTED
thisQueue                       &QUEUE, PROTECTED
thisFile                        &FILE, PROTECTED
tableName                       STRING(256), PROTECTED
fields                          &TEntityFieldQ, PROTECTED
entityType                      BYTE, PROTECTED               !- 1=GROUP, 2=QUEUE, 3=FILE
withBlobs                       BOOL, PROTECTED

Construct                       PROCEDURE()
Destruct                        PROCEDURE(), VIRTUAL

From                            PROCEDURE(*GROUP pGrp), PRIVATE
From                            PROCEDURE(*FILE pFile), PRIVATE

AddField                        PROCEDURE(STRING pName, LONG pPos, LONG pArrayNdx = 0), PRIVATE

SetEntity                       PROCEDURE(*GROUP pGrp, <STRING pName>), PROTECTED
SetEntity                       PROCEDURE(*QUEUE pQue, <STRING pName>), PROTECTED
SetEntity                       PROCEDURE(*FILE pFile, <STRING pName>), PROTECTED

!!!<summary>returns entity name</summary>
EntityName                      PROCEDURE(), STRING

!!!<summary>GET(fields, index)</summary>
!!!<returns>false if index is out of range</returns>
GetByIndex                      PROCEDURE(LONG pIndex), BOOL, PROC
GetByName                       PROCEDURE(STRING pName), BOOL, PROC
GetByPos                        PROCEDURE(LONG pOse), BOOL, PROC

!!!<summary>returns field name. You must call GetByXXX method before this one.</summary>
FieldName                       PROCEDURE(), STRING

!!!<summary>returns field value. You must call GetByXXX method before this one.</summary>
FieldValue                      PROCEDURE(), ?

!!!<summary>returns true if a field is MEMO or BLOB. You must call GetByXXX method before this one.</summary>
IsBlob                          PROCEDURE(), BOOL

!!!<summary>returns a value of a field</summary>
GetRawValue                     PROCEDURE(*GROUP pGrp), ?, PROTECTED
!!!<summary>returns a value of a field</summary>
GetRawValue                     PROCEDURE(*FILE pFile), ?, PROTECTED
!!!<summary>returns a printable value of a field</summary>
GetPrintableValue               PROCEDURE(*GROUP pGrp), STRING, PROTECTED
!!!<summary>returns a printable value of a field</summary>
GetPrintableValue               PROCEDURE(*FILE pFile), STRING, PROTECTED
!!!<summary>returns 'NAME1=value1,NAME2=value2...'</summary>
KeyValues                       PROCEDURE(*GROUP pGrp), STRING, PROTECTED
!!!<summary>returns 'NAME1=value1,NAME2=value2...'</summary>
KeyValues                       PROCEDURE(*FILE pFile), STRING, PROTECTED
!!!<summary>returns '(v11,v12,v13)'</summary>
Values                          PROCEDURE(*GROUP pGrp), STRING, PROTECTED
!!!<summary>returns '(v11,v12,v13),(v21,v22,v23)...'</summary>
Values                          PROCEDURE(*QUEUE pQue, BOOL pAllRecs), STRING, PROTECTED
!!!<summary>returns '(v11,v12,v13),(v21,v22,v23)...'</summary>
Values                          PROCEDURE(*FILE pFile, BOOL pAllRecs), STRING, PROTECTED

!!!<summary>returns total number of fields</summary>
FieldCount                      PROCEDURE(), LONG

!!!<summary>returns comma delimited list of field names</summary>
FieldNames                      PROCEDURE(), STRING
!!!<summary>returns a string of ?,?..?</summary>
Placeholders                    PROCEDURE(), STRING

!- set of methods returning sql query

!!!<summary>returns "CREATE TABLE [IF NOT EXISTS] table (fields);"</summary>
CreateQuery                     PROCEDURE(BOOL pIfNotExist = FALSE), STRING
!!!<summary>returns "'DROP TABLE IF EXISTS table;"</summary>
DropQuery                       PROCEDURE(), STRING
!!!<summary>returns "INSERT INTO table (fields) VALUES(values);"</summary>
InsertQuery                     PROCEDURE(BOOL pAllRecs = TRUE), STRING
!!!<summary>returns "INSERT INTO table (fields) VALUES (?,?,?);"</summary>
PrepareInsertQuery              PROCEDURE(), STRING
!!!<summary>returns "INSERT INTO table (fields) VALUES(values);"</summary>
InsertRecordQuery               PROCEDURE(), STRING
!!!<summary>returns "UPDATE table SET fields WHERE condition;"</summary>
UpdateRecordQuery               PROCEDURE(STRING pWhere), STRING
!!!<summary>returns "DELETE FROM table WHERE condition;"</summary>
DelereRecordQuery               PROCEDURE(STRING pWhere), STRING
                              END

TGroupEntity                  CLASS(TEntity), TYPE, MODULE('entity.clw'), LINK('entity.clw')
Init                            PROCEDURE(*GROUP pGrp, <STRING pName>)
                              END

TQueueEntity                  CLASS(TEntity), TYPE, MODULE('entity.clw'), LINK('entity.clw')
Init                            PROCEDURE(*QUEUE pQue, <STRING pName>)
                              END

TFileEntity                   CLASS(TEntity), TYPE, MODULE('entity.clw'), LINK('entity.clw')
Init                            PROCEDURE(*FILE pFile, <STRING pName>, BOOL pWithBlobs = FALSE)
                              END
