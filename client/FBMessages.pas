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
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TIBClientError            = (
      ibxeInterBaseMissing,
      ibxeIB60feature,
      ibxeNotSupported,
      ibxeNotPermitted,
      ibxeDatabaseClosed,
      ibxeNotInTransaction,
      ibxeTransactionNotAssigned,
      ibxeEOF,
      ibxeBOF,
      ibxeInvalidStatementHandle,
      ibxeSQLClosed,
      ibxeUnknownSQLDataType,
      ibxeInvalidColumnIndex,
      ibxeInvalidDataConversion,
      ibxeBlobCannotBeRead,
      ibxeBlobCannotBeWritten,
      ibxeBlobNotOpen,
      ibxeEmptyQuery,
      ibxeFieldNotFound,
      ibxeSQLParseError,
      ibxeIsASelectStatement,
      ibxeIsAExecuteProcedure,
      ibxeMaximumEvents,
      ibxeServiceActive,
      ibxeServiceInActive,
      ibxeServerNameMissing,
      ibxeOutputParsingError,
      ibxeEmptyAttachmentsList,
      ibxeInfoBufferTypeError,
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
      ibxeInvalidBlobMetaData,
      ibxeNoDPB,
      ibxeInEventWait,
      ibxeIncompatibleBlob,
      ibxeMissingColumnName,
      ibxStringTooLong,
      ibxFieldNotinDataSet,
      ibxeNoDefaultCharacterSet,
      ibxeParamBufferOverflow,
      ibxeInvalidParamCount,
      ibxeInvalidVariantType,
      ibxeInterfaceNotSupported,
      ibxeCharacterSetExists,
      ibxeUnknownUserCharSet,
      ibxeUninitializedInputParameter,
      ibxeMultiThreadRequired,
      ibxeTokenQueueOverflow,
      ibxeTokenQueueUnderflow,
      ibxErrorParsing,
      ibxeDLInfoError,
      ibxeDifferentAPIs,
      ibxeInvalidDateTimeStr,
      ibxeBadTimeZoneID,
      ibxeBadBCDConversion,
      ibxeBadTimeZoneName,
      ibxeTimeZoneUnknown,
      ibxeBadTimeSpecification,
      ibxeBCDTooBig,
      ibxeInvalidTimeZoneID,
      ibxeBadTimestampOrNoTimeZoneDBEntry,
      ibxeBCDOverflow,
      ibxeNoTimezoneSupport,
      ibxeDecFloatNotSupported,
      ibxeInt128NotSupported,
      ibxeUnknownParamTypeName,
      ibxInvalidQueryAction,
      ibxeSQLTypeUnchangeable,
      ibxeCannotIncreaseMetadatasize,
      ibxeBatchModeNotSupported
      );

function GetErrorMessage(ErrMess: TIBClientError): AnsiString;

{IBError is used internally and by IBX to throw an EIBClientError}

procedure IBError(ErrMess: TIBClientError; const Args: array of const);

resourcestring
  { generic strings used in code }
  SParamNameExpected = 'Parameter name expected';
  sSQLErrorSeparator = ' When Executing: ';
  SFirebirdAPIFuncNotFound = 'Unable to load Firebird Client Library Function "%s"';
  STrue = 'true';
  SFalse = 'false';
  SArray = '(array)';

implementation

uses IBUtils, IB;

resourcestring

{ strings used in error messages}
  SInterBaseMissing = 'Firebird library not found in the path. Please install Firebird to use this functionality';
  SIB60feature = '%s is an InterBase 6 function. Please upgrade to InterBase 6 to use this functonality';
  SNotSupported = 'Unsupported feature';
  SNotPermitted = 'Not permitted';
  SDatabaseClosed = 'Cannot perform operation -- DB is not open';
  SNotInTransaction = 'Transaction is not active';
  STransactionNotAssigned = 'Transaction not assigned';
  SEOF = 'End of file';
  SBOF = 'Beginning of file';
  SInvalidStatementHandle = 'Invalid statement handle';
  SSQLClosed = 'IBSQL Closed';
  SUnknownSQLDataType = 'Unknown SQL Data type (%d)';
  SInvalidColumnIndex = 'Invalid column index (index exceeds permitted range)';
  SInvalidDataConversion = 'Invalid data conversion from %s';
  SBlobCannotBeRead = 'Blob stream cannot be read';
  SBlobCannotBeWritten = 'Blob stream cannot be written';
  SBlobNotOpen = 'The Blob is not open';
  SEmptyQuery = 'Empty query';
  SFieldNotFound = 'Field "%s" not found';
  SSQLParseError = 'SQL Parse Error:' + CRLF + CRLF + '%s';
  SIsASelectStatement = 'use Open for a Select Statement';
  SIsAExecuteProcedure = 'use ExecProc for Procedure; use TQuery for Select procedures';
  SMaximumEvents = 'Exceded Maximum Event limits';
  SServiceActive = 'Cannot perform operation -- service is not attached';
  SServiceInActive = 'Cannot perform operation -- service is attached';
  SServerNameMissing = 'Server Name Missing';
  SOutputParsingError = 'Unexpected Output buffer value (%d) - %s';
  SEmptyAttachmentsList = 'The list of database attachments cannot be empty';
  SInfoBufferTypeError = 'Invalid operation for Info Buffer Type (%d)';
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
  SInvalidBlobMetaData = 'Unable to Access Blob Meta Data';
  SNoDPB = 'A DPB must be provided';
  SInEventWait = 'Already in Event Wait State';
  SIncompatibleBlob = 'Incompatible Blob SubTypes. %d expected, %d found';
  SMissingColumnName = 'Relation or Column Name Missing';
  SStringTooLong = 'String "%s" is too long. Max %d characters';
  SFieldNotinDataSet = 'Field %s is not a member of DataSet %s';
  SNoDefaultCharacterSet = 'A connection default character set is required to perform this operation';
  SParamBufferOverflow = 'Parameter Buffer Overflow';
  SInvalidParamCount = 'Invalid Parameter Count. %d expected, %d found';
  SInvalidVariantType = 'Invalid variant type';
  SInterfaceNotSupported = 'Interface not supported; Guid %s not found';
  SCharacterSetExists = 'Character set "%s" is already defined';
  SUnknownUserCharSet = 'Unknown user character set "%s"';
  SUninitializedInputParameter = 'SQL Param No. %d (%s) is uninitialised';
  SMultiThreadRequired = 'Multi-threading required for %s but not enabled. Please recompile with multi-threading support enabled. '+
                         'Hint: you probably need to add -dUseCThreads to the Custom Options.';
  STokenQueueOverflow = 'Error in SQL Token Analyser - token queue overflow';
  STokenQueueUnderflow = 'Error in SQL Token Analyser - token queue underflow';
  SErrorParsing = 'Error parsing SQL Statement at clause starting with %s';
  SDLInfoError = 'dlinfo returned error - %s';
  SDifferentAPIs = 'All transaction attachments must use the same Firebird Library';
  SInvalidDateTimeStr = '%s is not a valid Date/Time string';
  SBadTimeZoneID = 'Invalid Time Zone ID (%d,%d)';
  SBadBCDConversion = 'Conversion to BCD failed';
  SBadTimeZoneName = 'Invalid Time Zone Name "%s"';
  STimeZoneUnknown = 'Time Zone Unknown';
  SBadTimeSpecification = '%d:%d:%d.%d is not a valid time specification';
  SBCDTooBig = 'BCD Precision (%d) is too large for Firebird Data Type max precision (%d)';
  SInvalidTimeZoneID = 'Invalid Time Zone ID (%d)';
  SBadTimestampOrNoTimeZoneDBEntry = 'Bad Timestamp or missing time zone DB entry (%s) TZ ID = %d';
  SBCDOverflow = 'BCD Precision too large for Firebird data type,[%s]';
  SNoTimezoneSupport = 'TIME/TIMESTAMP WITH TIME ZONE data type not supported';
  SDecFloatNotSupported = 'DecFloat Data Type not supported';
  SInt128NotSupported = 'INT128 Data Type not supported';
  SUnknownParamTypeName = '%s:Unknown Param Type Name "%s"';
  SInvalidQueryAction = 'Query Action only valid for an Update or Insert Query';
  SSQLTypeUnchangeable = 'Cannot change SQL Type from %s to %s';
  SCannotIncreaseMetadatasize = 'Cannot increase Metadata size from %d to %d';
  SBatchModeNotSupported = 'Batch Mode is not available. Firebird 4 or later client nd server is required';

const
  IBErrorMessages: array[TIBClientError] of string = (
    SInterBaseMissing,
    SIB60feature,
    SNotSupported,
    SNotPermitted,
    SDatabaseClosed,
    SNotInTransaction,
    STransactionNotAssigned,
    SEOF,
    SBOF,
    SInvalidStatementHandle,
    SSQLClosed,
    SUnknownSQLDataType,
    SInvalidColumnIndex,
    SInvalidDataConversion,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SBlobNotOpen,
    SEmptyQuery,
    SFieldNotFound,
    SSQLParseError,
    SIsASelectStatement,
    SIsAExecuteProcedure,
    SMaximumEvents,
    SServiceActive,
    SServiceInActive,
    SServerNameMissing,
    SOutputParsingError,
    SEmptyAttachmentsList,
    SInfoBufferTypeError,
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
    SInvalidBlobMetaData,
    SNoDPB,
    SInEventWait,
    SIncompatibleBlob,
    SMissingColumnName,
    SStringTooLong,
    SFieldNotinDataSet,
    SNoDefaultCharacterSet,
    SParamBufferOverflow,
    SInvalidParamCount,
    SInvalidVariantType,
    SInterfaceNotSupported,
    SCharacterSetExists,
    SUnknownUserCharSet,
    SUninitializedInputParameter,
    SMultiThreadRequired,
    STokenQueueOverflow,
    STokenQueueUnderflow,
    SErrorParsing,
    SDLInfoError,
    SDifferentAPIs,
    SInvalidDateTimeStr,
    SBadTimeZoneID,
    SBadBCDConversion,
    SBadTimeZoneName,
    STimeZoneUnknown,
    SBadTimeSpecification,
    SBCDTooBig,
    SInvalidTimeZoneID,
    SBadTimestampOrNoTimeZoneDBEntry,
    SBCDOverflow,
    SNoTimezoneSupport,
    SDecFloatNotSupported,
    SInt128NotSupported,
    SUnknownParamTypeName,
    SInvalidQueryAction,
    SSQLTypeUnchangeable,
    SCannotIncreaseMetadatasize,
    SBatchModeNotSupported
  );

function GetErrorMessage(ErrMess: TIBClientError): AnsiString;
begin
  Result := IBErrorMessages[ErrMess];
end;

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
begin
  raise EIBClientError.Create(Ord(ErrMess),
                              Format(GetErrorMessage(ErrMess), Args));
end;

end.

