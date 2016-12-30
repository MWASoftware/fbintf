(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit FBMessages;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TIBDataBaseErrorMessage    = (ShowSQLCode,
                                  ShowIBMessage,
                                  ShowSQLMessage);

  TIBDataBaseErrorMessages   = set of TIBDataBaseErrorMessage;
  TIBClientError            = (
      ibxeUnknownError,
      ibxeInterBaseMissing,
      ibxeInterBaseInstallMissing,
      ibxeIB60feature,
      ibxeNotSupported,
      ibxeNotPermitted,
      ibxeFileAccessError,
      ibxeConnectionTimeout,
      ibxeCannotSetDatabase,
      ibxeCannotSetTransaction,
      ibxeOperationCancelled,
      ibxeDPBConstantNotSupported,
      ibxeDPBConstantUnknown,
      ibxeTPBConstantNotSupported,
      ibxeTPBConstantUnknown,
      ibxeDatabaseClosed,
      ibxeDatabaseOpen,
      ibxeDatabaseNameMissing,
      ibxeNotInTransaction,
      ibxeInTransaction,
      ibxeTimeoutNegative,
      ibxeNoDatabasesInTransaction,
      ibxeUpdateWrongDB,
      ibxeUpdateWrongTR,
      ibxeDatabaseNotAssigned,
      ibxeTransactionNotAssigned,
      ibxeXSQLDAIndexOutOfRange,
      ibxeXSQLDANameDoesNotExist,
      ibxeEOF,
      ibxeBOF,
      ibxeInvalidStatementHandle,
      ibxeSQLOpen,
      ibxeSQLClosed,
      ibxeDatasetOpen,
      ibxeDatasetClosed,
      ibxeUnknownSQLDataType,
      ibxeInvalidColumnIndex,
      ibxeInvalidParamColumnIndex,
      ibxeInvalidDataConversion,
      ibxeColumnIsNotNullable,
      ibxeBlobCannotBeRead,
      ibxeBlobCannotBeWritten,
      ibxeBlobNotOpen,
      ibxeEmptyQuery,
      ibxeCannotOpenNonSQLSelect,
      ibxeNoFieldAccess,
      ibxeFieldReadOnly,
      ibxeFieldNotFound,
      ibxeNotEditing,
      ibxeCannotInsert,
      ibxeCannotPost,
      ibxeCannotUpdate,
      ibxeCannotDelete,
      ibxeCannotRefresh,
      ibxeBufferNotSet,
      ibxeCircularReference,
      ibxeSQLParseError,
      ibxeUserAbort,
      ibxeDataSetUniDirectional,
      ibxeCannotCreateSharedResource,
      ibxeWindowsAPIError,
      ibxeColumnListsDontMatch,
      ibxeColumnTypesDontMatch,
      ibxeCantEndSharedTransaction,
      ibxeFieldUnsupportedType,
      ibxeCircularDataLink,
      ibxeEmptySQLStatement,
      ibxeIsASelectStatement,
      ibxeRequiredParamNotSet,
      ibxeNoStoredProcName,
      ibxeIsAExecuteProcedure,
      ibxeUpdateFailed,
      ibxeNotCachedUpdates,
      ibxeNotLiveRequest,
      ibxeNoProvider,
      ibxeNoRecordsAffected,
      ibxeNoTableName,
      ibxeCannotCreatePrimaryIndex,
      ibxeCannotDropSystemIndex,
      ibxeTableNameMismatch,
      ibxeIndexFieldMissing,
      ibxeInvalidCancellation,
      ibxeInvalidEvent,
      ibxeMaximumEvents,
      ibxeNoEventsRegistered,
      ibxeInvalidQueueing,
      ibxeInvalidRegistration,
      ibxeInvalidBatchMove,
      ibxeSQLDialectInvalid,
      ibxeSPBConstantNotSupported,
      ibxeSPBConstantUnknown,
      ibxeServiceActive,
      ibxeServiceInActive,
      ibxeServerNameMissing,
      ibxeQueryParamsError,
      ibxeStartParamsError,
      ibxeOutputParsingError,
      ibxeUseSpecificProcedures,
      ibxeSQLMonitorAlreadyPresent,
      ibxeCantPrintValue,
      ibxeEOFReached,
      ibxeEOFInComment,
      ibxeEOFInString,
      ibxeParamNameExpected,
      ibxeSuccess,
      ibxeDelphiException,
      ibxeNoOptionsSet,
      ibxeNoDestinationDirectory,
      ibxeNosourceDirectory,
      ibxeNoUninstallFile,
      ibxeOptionNeedsClient,
      ibxeOptionNeedsServer,
      ibxeInvalidOption,
      ibxeInvalidOnErrorResult,
      ibxeInvalidOnStatusResult,
      ibxeDPBConstantUnknownEx,
      ibxeTPBConstantUnknownEx,
      ibxeSV5APIError,
      ibxeThreadFailed,
      ibxeFieldSizeError,
      ibxeTransactionNotEnding,
      ibxeDscInfoTokenMissing,
      ibxeNoLoginDialog,
      ibxeEmptyAttachmentsList,
      ibxeFirebirdLibraryLoaded,
      ibxeInfoBufferIndexError,
      ibxeInfoBufferTypeError,
      ibxeInfoBufferOverflow,
      ibxServiceRequestIndexError,
      ibxServiceParamTypeError,
      ibxeOutputBlockIndexError,
      ibxeOutputBlockTypeError,
      ibxePBIndexError,
      ibxePBParamTypeError,
      ibxeDuplicateParamName,
      ibxeInvalidArrayDimensions,
      ibxeNotAMultiDatabaseTransaction,
      ibxeAttachmentListIndexError,
      ibxeNotAnArray,
      ibxeNotABlob,
      ibxeInvalidSubscript,
      ibxeArrayElementOverFlow,
      ibxArrayBoundsCantIncrease ,
      ibxeStatementNotPrepared,
      ibxeInterfaceOutofDate,
      ibxeUnexpectedDatabaseInfoResp,
      ibxeInvalidBlobMetaData,
      ibxeNoDPB,
      ibxeInEventWait,
      ibxeIncompatibleBlob,
      ibxeMissingColumnName,
      ibxStringTooLong,
      ibxFieldNotinDataSet,
      ibxeNotCurrentArray,
      ibxeNoDefaultCharacterSet,
      ibxeParamBufferOverflow,
      ibxeInvalidParamCount,
      ibxeInvalidVariantType,
      ibxeServiceRunning,
      ibxeUniqueRelationReqd
      );

function GetErrorMessage(ErrMess: TIBClientError): string;

resourcestring
  { generic strings used in code }
  SEOFReached = 'SEOFReached';
  SEOFInComment = 'EOF in comment detected';
  SEOFInString = 'EOF in string detected';
  SParamNameExpected = 'Parameter name expected';
  SCantPrintValue = 'Cannot print value';
  SSuccess = 'Successful execution';
  SDisconnectDatabase = 'Database is currently connected. Disconnect and continue?';
  SCommitTransaction = 'Transaction is currently Active. Rollback and continue?';
  sSQLErrorSeparator = ' When Executing: ';
  SNoDataSet = 'No dataset association';
  SSQLGenSelect = 'Must select at least one key field and one update field';
  SSQLNotGenerated = 'Update SQL statements not generated, exit anyway?';
  SSQLDataSetOpen = 'Unable to determine field names for %s';
  SDefaultTransaction = '%s, Default';
  SFirebirdAPIFuncNotFound = 'Unable to load Firebird Client Library Function "%s"';
  SDatabaseFilter = 'Database Files (*.fdb; *.gdb)|*.gdb; *.fdb|All files (*.*)|*.*';
  STrue = 'true';
  SFalse = 'false';
  SArray = '(array)';
  SBlob = '(blob)';

implementation

uses IBUtils;

resourcestring

{ strings used in error messages}
  SUnknownError = 'Unknown error';
  SInterBaseMissing = 'Firebird library not found in the path. Please install Firebird to use this functionality';
  SInterBaseInstallMissing = 'InterBase Install DLL ibinstall.dll not found in the path. Please install InterBase 6 to use this functionality';
  SIB60feature = '%s is an InterBase 6 function. Please upgrade to InterBase 6 to use this functonality';
  SNotSupported = 'Unsupported feature';
  SNotPermitted = 'Not permitted';
  SFileAccessError = 'Temporary file access error';
  SConnectionTimeout = 'Database connection timed out';
  SCannotSetDatabase = 'Cannot set database';
  SCannotSetTransaction = 'Cannot set transaction';
  SOperationCancelled = 'Operation cancelled at user''s request';
  SDPBConstantNotSupported = 'DPB Constant (isc_dpb_%s) is unsupported';
  SDPBConstantUnknown = 'DPB Constant (%d) is unknown';
  STPBConstantNotSupported = 'TPB Constant (isc_tpb_%s) is unsupported';
  STPBConstantUnknown = 'TPB Constant (%d) is unknown';
  SDatabaseClosed = 'Cannot perform operation -- DB is not open';
  SDatabaseOpen = 'Cannot perform operation -- DB is currently open';
  SDatabaseNameMissing = 'Database name is missing';
  SNotInTransaction = 'Transaction is not active';
  SInTransaction = 'Transaction is active';
  STimeoutNegative = 'Timeout values cannot be negative';
  SNoDatabasesInTransaction = 'No databases are listed in transaction component';
  SUpdateWrongDB = 'Updating wrong database';
  SUpdateWrongTR = 'Updating wrong transaction. Unique transaction expected in set';
  SDatabaseNotAssigned = 'Database not assigned';
  STransactionNotAssigned = 'Transaction not assigned';
  SXSQLDAIndexOutOfRange = 'XSQLDA index out of range';
  SXSQLDANameDoesNotExist = 'XSQLDA name does not exist (%s)';
  SEOF = 'End of file';
  SBOF = 'Beginning of file';
  SInvalidStatementHandle = 'Invalid statement handle';
  SSQLOpen = 'IBSQL Open';
  SSQLClosed = 'IBSQL Closed';
  SDatasetOpen = 'Dataset open';
  SDatasetClosed = 'Dataset closed';
  SUnknownSQLDataType = 'Unknown SQL Data type (%d)';
  SInvalidColumnIndex = 'Invalid column index (index exceeds permitted range)';
  SInvalidParamColumnIndex = 'Invalid parameter index (index exceeds permitted range)';
  SInvalidDataConversion = 'Invalid data conversion';
  SColumnIsNotNullable = 'Column cannot be set to null (%s)';
  SBlobCannotBeRead = 'Blob stream cannot be read';
  SBlobCannotBeWritten = 'Blob stream cannot be written';
  SBlobNotOpen = 'The Blob is not open';
  SEmptyQuery = 'Empty query';
  SCannotOpenNonSQLSelect = 'Cannot "open" a non-select statement. Use ExecQuery';
  SNoFieldAccess = 'No access to field "%s"';
  SFieldReadOnly = 'Field "%s" is read-only';
  SFieldNotFound = 'Field "%s" not found';
  SNotEditing = 'Not in edit mode';
  SCannotInsert = 'Cannot insert into dataset. (No insert query)';
  SCannotPost = 'Cannot post. (No update/insert query)';
  SCannotUpdate = 'Cannot update. (No update query)';
  SCannotDelete = 'Cannot delete from dataset. (No delete query)';
  SCannotRefresh = 'Cannot refresh row. (No refresh query)';
  SBufferNotSet = 'Buffer not set';
  SCircularReference = 'Circular references not permitted';
  SSQLParseError = 'SQL Parse Error:' + CRLF + CRLF + '%s';
  SUserAbort = 'User abort';
  SDataSetUniDirectional = 'Data set is uni-directional';
  {$IFDEF UNIX}
  SCannotCreateSharedResource = 'Cannot create shared resource. %s';
  {$ELSE}
  SCannotCreateSharedResource = 'Cannot create shared resource. (Windows error %d)';
  {$ENDIF}
  SWindowsAPIError = 'Windows API error. (Windows error %d [$%.8x])';
  SColumnListsDontMatch = 'Column lists do not match';
  SColumnTypesDontMatch = 'Column types don''t match. (From index: %d; To index: %d)';
  SCantEndSharedTransaction = 'Can''t end a shared transaction unless it is forced and equal ' +
                             'to the transaction''s TimeoutAction';
  SFieldUnsupportedType = 'Unsupported Field Type';
  SCircularDataLink = 'Circular DataLink Reference';
  SEmptySQLStatement = 'Empty SQL Statement';
  SIsASelectStatement = 'use Open for a Select Statement';
  SRequiredParamNotSet = 'Required parameter "%s" value not set';
  SNoStoredProcName = 'No Stored Procedure Name assigned';
  SIsAExecuteProcedure = 'use ExecProc for Procedure; use TQuery for Select procedures';
  SUpdateFailed = 'Update Failed';
  SNotCachedUpdates = 'CachedUpdates not enabled';
  SNotLiveRequest = 'Request is not live - cannot modify';
  SNoProvider = 'No Provider';
  SNoRecordsAffected = 'No Records Affected';
  SNoTableName = 'No Table Name assigned';
  SCannotCreatePrimaryIndex = 'Cannot Create Primary Index; are created automatically';
  SCannotDropSystemIndex = 'Cannot Drop System Index';
  STableNameMismatch = 'Table Name Mismatch';
  SIndexFieldMissing = 'Index Field Missing';
  SInvalidCancellation = 'Cannot Cancel events while processing';
  SInvalidEvent = 'Invalid Event';
  SMaximumEvents = 'Exceded Maximum Event limits';
  SNoEventsRegistered = 'No Events Registered';
  SInvalidQueueing = 'Invalid Queueing';
  SInvalidRegistration = 'Invalid Registration';
  SInvalidBatchMove = 'Invalid Batch Move';
  SSQLDialectInvalid = 'SQL Dialect Invalid';
  SSPBConstantNotSupported = 'SPB Constant Not supported';
  SSPBConstantUnknown = 'SPB Constant Unknown';
  SServiceActive = 'Cannot perform operation -- service is not attached';
  SServiceInActive = 'Cannot perform operation -- service is attached';
  SServerNameMissing = 'Server Name Missing';
  SQueryParamsError = 'Query Parameters missing or incorrect';
  SStartParamsError = 'start Parameters missing or incorrect';
  SOutputParsingError = 'Unexpected Output buffer value (%d)';
  SUseSpecificProcedures = 'Generic ServiceStart not applicable: Use Specific Procedures to set configuration params';
  SSQLMonitorAlreadyPresent = 'SQL Monitor Instance is already present';
  SDelphiException = 'DelphiException %s';
  SNoOptionsSet = 'No Install Options selected';
  SNoDestinationDirectory = 'DestinationDirectory is not set';
  SNosourceDirectory = 'SourceDirectory is not set';
  SNoUninstallFile = 'Uninstall File Name is not set';
  SOptionNeedsClient = '%s component requires Client to function properly';
  SOptionNeedsServer = '%s component requires Server to function properly';
  SInvalidOption = 'Invalid option specified';
  SInvalidOnErrorResult = 'Unexpected onError return value';
  SInvalidOnStatusResult = 'Unexpected onStatus return value';

  SDPBConstantUnknownEx = 'DPB Constant (%s) is unknown';
  STPBConstantUnknownEx = 'TPB Constant (%s) is unknown';
  SSV5APIError = 'SV5 API API Error - %s';
  SThreadFailed = '%s Thread failed with Exception: %s';
  sFieldSizeError = 'Field %s is too small to receive the data';
  STransactionNotEnding = 'Transaction is not being completed';
  SDscInfoTokenMissing = '%s token not found';
  SNoLoginDialog = 'Default Login Dlalog not found. Have you included ibexpress ' +
                   'in your program uses list?';
  SEmptyAttachmentsList = 'The list of database attachments cannot be empty';
  SFirebirdLibraryLoaded = 'The Firebird Library is already loaded';
  SInfoBufferIndexError = 'Info Buffer Index Out of Range (%d)';
  SInfoBufferTypeError = 'Invalid operation for Info Buffer Type (%d)';
  SInfoBufferOverflow = 'Info Buffer overlow';
  SServiceRequestIndexError = 'Service Request Index Out of Range (%d)';
  SServiceParamTypeError = 'Invalid Request for Service Param Type';
  SOutputBlockIndexError = 'Output Block Index Out of Range (%d)';
  SOutputBlockTypeError = 'Invalid Request for Output Block Type';
  SPBIndexError = 'DPB Index out of range (%d)';
  SPBParamTypeError = 'Invalid Request for DPB Param Type';
  SDuplicateParamName = 'Blob or array parameter name must be unique (%s)';
  SInvalidArrayDimensions = 'Invalid number of array dimensions (%d)';
  SNotAMultiDatabaseTransaction = 'This is not a multi-database transaction';
  SAttachmentListIndexError = 'Attachment List index out of range (%d)';
  SNotAnArray = 'Table Column must be an array';
  SNotABlob = 'Table Column must be a Blob';
  SInvalidSubscript = 'Invalid Subscript (%d) for Array Dimension %d';
  SArrayElementOverFlow = 'Array Element too big';
  SArrayBoundsCantIncrease = 'Array Bounds can only be narrowed';
  SStatementNotPrepared = 'The Statement has not been prepared';
  SInterfaceOutofDate = 'This interface is no longer up-to-date';
  SUnexpectedDatabaseInfoResp = 'Unexpected Database Information Response';
  SInvalidBlobMetaData = 'Unable to Access Blob Meta Data';
  SNoDPB = 'A DPB must be provided';
  SInEventWait = 'Already in Event Wait State';
  SIncompatibleBlob = 'Incompatible Blob SubTypes. %d expected, %d found';
  SMissingColumnName = 'Relation or Column Name Missing';
  SStringTooLong = 'String "%s" is too long. Max %d characters';
  SFieldNotinDataSet = 'Field %s is not a member of DataSet %s';
  SNotCurrentArray = 'Cannot Edit an Array that is not part of the current record';
  SNoDefaultCharacterSet = 'A connection default character set is required to perform this operation';
  SParamBufferOverflow = 'Parameter Buffer Overflow';
  SInvalidParamCount = 'Invalid Parameter Count. %d expected, %d found';
  SInvalidVariantType = 'Invalid variant type';
  SServiceRunning = 'Cannot start a new service while an existing service is running';
  SUniqueRelationReqd = 'All Output Fields must derived from the same table';

const
  IBErrorMessages: array[TIBClientError] of string = (
    SUnknownError,
    SInterBaseMissing,
    SInterBaseInstallMissing,
    SIB60feature,
    SNotSupported,
    SNotPermitted,
    SFileAccessError,
    SConnectionTimeout,
    SCannotSetDatabase,
    SCannotSetTransaction,
    SOperationCancelled,
    SDPBConstantNotSupported,
    SDPBConstantUnknown,
    STPBConstantNotSupported,
    STPBConstantUnknown,
    SDatabaseClosed,
    SDatabaseOpen,
    SDatabaseNameMissing,
    SNotInTransaction,
    SInTransaction,
    STimeoutNegative,
    SNoDatabasesInTransaction,
    SUpdateWrongDB,
    SUpdateWrongTR,
    SDatabaseNotAssigned,
    STransactionNotAssigned,
    SXSQLDAIndexOutOfRange,
    SXSQLDANameDoesNotExist,
    SEOF,
    SBOF,
    SInvalidStatementHandle,
    SSQLOpen,
    SSQLClosed,
    SDatasetOpen,
    SDatasetClosed,
    SUnknownSQLDataType,
    SInvalidColumnIndex,
    SInvalidParamColumnIndex,
    SInvalidDataConversion,
    SColumnIsNotNullable,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SBlobNotOpen,
    SEmptyQuery,
    SCannotOpenNonSQLSelect,
    SNoFieldAccess,
    SFieldReadOnly,
    SFieldNotFound,
    SNotEditing,
    SCannotInsert,
    SCannotPost,
    SCannotUpdate,
    SCannotDelete,
    SCannotRefresh,
    SBufferNotSet,
    SCircularReference,
    SSQLParseError,
    SUserAbort,
    SDataSetUniDirectional,
    SCannotCreateSharedResource,
    SWindowsAPIError,
    SColumnListsDontMatch,
    SColumnTypesDontMatch,
    SCantEndSharedTransaction,
    SFieldUnsupportedType,
    SCircularDataLink,
    SEmptySQLStatement,
    SIsASelectStatement,
    SRequiredParamNotSet,
    SNoStoredProcName,
    SIsAExecuteProcedure,
    SUpdateFailed,
    SNotCachedUpdates,
    SNotLiveRequest,
    SNoProvider,
    SNoRecordsAffected,
    SNoTableName,
    SCannotCreatePrimaryIndex,
    SCannotDropSystemIndex,
    STableNameMismatch,
    SIndexFieldMissing,
    SInvalidCancellation,
    SInvalidEvent,
    SMaximumEvents,
    SNoEventsRegistered,
    SInvalidQueueing,
    SInvalidRegistration,
    SInvalidBatchMove,
    SSQLDialectInvalid,
    SSPBConstantNotSupported,
    SSPBConstantUnknown,
    SServiceActive,
    SServiceInActive,
    SServerNameMissing,
    SQueryParamsError,
    SStartParamsError,
    SOutputParsingError,
    SUseSpecificProcedures,
    SSQLMonitorAlreadyPresent,
    SCantPrintValue,
    SEOFReached,
    SEOFInComment,
    SEOFInString,
    SParamNameExpected,
    SSuccess,
    SDelphiException,
    SNoOptionsSet,
    SNoDestinationDirectory,
    SNosourceDirectory,
    SNoUninstallFile,
    SOptionNeedsClient,
    SOptionNeedsServer,
    SInvalidOption,
    SInvalidOnErrorResult,
    SInvalidOnStatusResult,
    SDPBConstantUnknownEx,
    STPBConstantUnknownEx,
    SSV5APIError,
    SThreadFailed,
    SFieldSizeError,
    STransactionNotEnding,
    SDscInfoTokenMissing,
    SNoLoginDialog,
    SEmptyAttachmentsList,
    SFirebirdLibraryLoaded,
    SInfoBufferIndexError,
    SInfoBufferTypeError,
    SInfoBufferOverflow,
    SServiceRequestIndexError,
    SServiceParamTypeError,
    SOutputBlockIndexError,
    SOutputBlockTypeError,
    SPBIndexError,
    SPBParamTypeError,
    SDuplicateParamName,
    SInvalidArrayDimensions,
    SNotAMultiDatabaseTransaction,
    SAttachmentListIndexError,
    SNotAnArray,
    SNotABlob,
    SInvalidSubscript,
    SArrayElementOverFlow,
    SArrayBoundsCantIncrease,
    SStatementNotPrepared,
    SInterfaceOutofDate,
    SUnexpectedDatabaseInfoResp,
    SInvalidBlobMetaData,
    SNoDPB,
    SInEventWait,
    SIncompatibleBlob,
    SMissingColumnName,
    SStringTooLong,
    SFieldNotinDataSet,
    SNotCurrentArray,
    SNoDefaultCharacterSet,
    SParamBufferOverflow,
    SInvalidParamCount,
    SInvalidVariantType,
    SServiceRunning,
    SUniqueRelationReqd
  );

function GetErrorMessage(ErrMess: TIBClientError): string;
begin
  Result := IBErrorMessages[ErrMess];
end;

end.

