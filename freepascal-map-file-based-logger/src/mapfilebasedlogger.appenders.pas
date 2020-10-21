{Unit contains API for Appender creating.}
unit MapFileBasedLogger.Appenders;

{$mode objfpc}{$H+}

interface

uses MapFileBasedLogger.BasicTypes, Classes;

type

  { ILogFilter filtered messages by level, callable info(unit,class,method) or message body}
  ILogFilter = interface
    function IsAvailableCallable(const CallableInfo: TCallableInfo): boolean;
    function IsAvailableLogLevel(const LogLevel: TLogLevel;
      BaseLogLevel: TLogLevel): boolean;
    function IsAvailableMessage(const message: string): boolean;
  end;

  TLogFilterArray = specialize TArray<ILogFilter>;
  { TAbstractLogFilter }

  TAbstractLogFilter = class(TInterfacedObject, ILogFilter)
    function IsAvailableCallable(const CallableInfo: TCallableInfo): boolean; virtual;
    function IsAvailableLogLevel(const GivenLogLevel: TLogLevel;
      BaseLogLevel: TLogLevel): boolean;
    function IsAvailableMessage(const message: string): boolean; virtual;
  end;

  { ILogAppender }

  ILogAppender = interface
    function GetDateTimeFormat(): string;
    function AppendMessageTo(const LogLevel: TLogLevel;
      const CallableInfo: TCallableInfo; Message: string): boolean;
    function GetLogFilterList(): TLogFilterArray;
  end;

  TLogAppenderArray = specialize TArray<ILogAppender>;
  { TAbstractLogAppender }

  TAbstractLogAppender = class(TInterfacedObject, ILogAppender)
  strict private
  const UNKNOWN_CALLABLE_STRING:String='UNKNOWN METHOD';
  strict private
    FLogFilterArray: TLogFilterArray;
    {Get printable name of message log level
    @param LogLevel given log level value
    @returns string representation log level name without prefix 'll'}
    function GetLevelName(LogLevel: TLogLevel): string;
  strict protected
    {DateTime format. By default DefaultFormatSettings.LongDateFormat+' '+
    DefaultFormatSettings.LongTimeFormat+'.zzz'}
    FDateTimeFormat: string;
    {for multithreading.}
    FRTLCriticalSection: TRTLCriticalSection;
    {Directly flushing message into device(file,db,console,mail...)}
    function WriteToAppendable(FullMessage: string):boolean; virtual; abstract;
  protected
    constructor Create(LogFilterArray: TLogFilterArray);
    destructor Destroy; override;
  public
    function GetDateTimeFormat(): string; virtual;
    function AppendMessageTo(const LogLevel: TLogLevel;
      const CallableInfo: TCallableInfo; Message: string): boolean;
    function GetLogFilterList(): TLogFilterArray;
  end;

  { TConsoleAppender writes message with datetime,level and callable info in console
  if it exists.}
  TConsoleAppender = class(TAbstractLogAppender)
  strict protected
    function WriteToAppendable(FullMessage: string):boolean; override;
  end;

const
  {Max saving period in days}
  DEFAULT_MAX_SAVING_PERIOD_IN_DAYS = 7;
  {Default max size - 100mb.}
  DEFAULT_MAX_FILE_SIZE = 100 * 1024 * 1024;

type

  { TRotateFileAppender flushes data on }

  TRotateFileAppender = class(TAbstractLogAppender)
  strict private
  const
    FILE_EXTENSION_FORMAT:   String='yyyy-mm-dd';
    strict private
    {basic filenae}
    FFileName: string;
    FLoggingDirectory: string;
    FMaxPeriodInDays: integer;
    FMaxFileSize: int64;
    FCurrentFileStream: TFileStream;
    function GetCurrentFileName(): string;
    function GetCurrentFileStream():TFileStream;
    procedure RemoveUnusedLogFiles();
    function TestChangedFileNameDate():boolean;
  strict protected
    function WriteToAppendable(FullMessage: string):boolean; override;
  protected
    constructor Create(FileName: string; LoggingDirectory: string;
      LogFilterArray: TLogFilterArray; MaxPeriodInDays: integer; MaxFileSize: int64);
    destructor Destroy; override;
  end;

type
  { TAppenderFactory }

  TAppenderFactory = class
    class function BuildConsoleAppender(LogFilterArray: TLogFilterArray = nil): ILogAppender;
    class function BuildRotateFileAppender(FileName: string; LoggingDirectory: string;
      LogFilterArray: TLogFilterArray = nil;
      MaxPeriodInDays: integer = DEFAULT_MAX_SAVING_PERIOD_IN_DAYS;
      MaxFileSize: int64 = DEFAULT_MAX_FILE_SIZE): ILogAppender;
  end;

implementation

uses TypInfo, DateUtils, LazFileUtils,SysUtils;
resourcestring
  EXTENSION_HAS_WRONG_FORMAT='File extention ''%s'' don''t match for ''%s'' format!';
{ TRotateFileAppender }

constructor TRotateFileAppender.Create(FileName: string; LoggingDirectory: string;
  LogFilterArray: TLogFilterArray; MaxPeriodInDays: integer; MaxFileSize: int64);
var
  FullFileName: string;

  LogFileExists: boolean;
begin
  inherited Create(LogFilterArray);
  FFileName := FileName;
  FMaxPeriodInDays := MaxPeriodInDays;
  FMaxFileSize := MaxFileSize;
  FLoggingDirectory := LoggingDirectory;
  if (not DirectoryExistsUTF8(LoggingDirectory)) then
    ForceDirectoriesUTF8(LoggingDirectory);
  RemoveUnusedLogFiles();
  FullFileName := GetCurrentFileName();
end;

function TRotateFileAppender.WriteToAppendable(FullMessage: string):boolean;
var
  MessageLength, Count: integer;
begin
  FullMessage := FullMessage + LineEnding;
  MessageLength := Length(FullMessage);
  Count := GetCurrentFileStream().WriteData(PChar(@FullMessage[1]), MessageLength);
  result:=MessageLength = Count;
end;

function TRotateFileAppender.GetCurrentFileName(): string;
var
  Extension: string;
  CurrentDate: TDateTime;
  DateString:String;
begin
  CurrentDate := Date;
  DateTimeToString(DateString,FILE_EXTENSION_FORMAT,CurrentDate);
  Extension := '.' +DateString;
  Result := IncludeTrailingPathDelimiter(FLoggingDirectory) +
    LazFileUtils.ExtractFileNameOnly(FFileName) + extension;
end;

function TRotateFileAppender.GetCurrentFileStream(): TFileStream;
Var
  CurrentFileName:String;
begin
  EnterCriticalSection(FRTLCriticalSection);
  if TestChangedFileNameDate() then
    FreeAndNil(FCurrentFileStream);
  try
    if FCurrentFileStream<>nil then
      exit(FCurrentFileStream);
    CurrentFileName:=GetCurrentFileName;
    if FileExists(CurrentFileName)then
       FCurrentFileStream:=TFileStream.Create(CurrentFileName,fmOpenWrite or fmShareDenyWrite)
    else
      FCurrentFileStream:=TFileStream.Create(CurrentFileName,fmCreate or fmShareDenyWrite);
    FCurrentFileStream.Seek(0,TSeekOrigin.soEnd);
    result:=FCurrentFileStream;
  finally
    LeaveCriticalSection(FRTLCriticalSection);
  end;
end;

procedure TRotateFileAppender.RemoveUnusedLogFiles();
var  SearchRec:TSearchRec;
begin
  if(FindFirstUTF8(FLoggingDirectory,faNormal, SearchRec)<>0)then
    exit;

end;

function TRotateFileAppender.TestChangedFileNameDate(): boolean;
Var
  Extention,DateString:String;
  FileDate:TDateTime;
begin
  Extention:=ExtractFileExt(GetCurrentFileName);
  DateString:=Extention.Substring(1);
  FileDate:=ScanDateTime(FILE_EXTENSION_FORMAT,DateString);
  result:=FileDate<>Date;
end;

destructor TRotateFileAppender.Destroy;
begin
  FreeAndNil(FCurrentFileStream);
  inherited Destroy;
end;

{ TConsoleAppender }

function TConsoleAppender.WriteToAppendable(FullMessage: string):Boolean;
begin
  writeln(FullMessage);
Result:=true;
end;

{ TAbstractLogFilter }

function TAbstractLogFilter.IsAvailableCallable(
  const CallableInfo: TCallableInfo): boolean;
begin
  Result := True;
end;

function TAbstractLogFilter.IsAvailableLogLevel(const GivenLogLevel: TLogLevel;
  BaseLogLevel: TLogLevel): boolean;
begin
  Result := Ord(GivenLogLevel) >= Ord(BaseLogLevel);
end;

function TAbstractLogFilter.IsAvailableMessage(const message: string): boolean;
begin
  Result := True;
end;

{ TAppenderFactory }

class function TAppenderFactory.BuildConsoleAppender(LogFilterArray:
  TLogFilterArray): ILogAppender;
begin
  Result := TConsoleAppender.Create(LogFilterArray);
end;

class function TAppenderFactory.BuildRotateFileAppender(FileName: string;
  LoggingDirectory: string; LogFilterArray: TLogFilterArray;
  MaxPeriodInDays: integer; MaxFileSize: int64): ILogAppender;
begin
  Result := TRotateFileAppender.Create(FileName, LoggingDirectory,
    LogFilterArray, MaxPeriodInDays, MaxFileSize);
end;

{ TAbstractLogAppender }

function TAbstractLogAppender.GetLevelName(LogLevel: TLogLevel): string;
begin
  Result := GetEnumName(TypeInfo(TLogLevel), Ord(LogLevel));
  Result := Result.Replace('ll', '', [rfReplaceAll]).ToUpper;
end;

constructor TAbstractLogAppender.Create(LogFilterArray: TLogFilterArray);
begin
  inherited Create;
  FDateTimeFormat := DefaultFormatSettings.LongDateFormat + ' ' +
    DefaultFormatSettings.LongTimeFormat + '.zzz';
  FLogFilterArray := LogFilterArray;
  InitCriticalSection(FRTLCriticalSection);
end;

destructor TAbstractLogAppender.Destroy;
begin
  SetLength(FLogFilterArray, 0);
  DoneCriticalSection(FRTLCriticalSection);
  inherited Destroy;
end;

function TAbstractLogAppender.GetDateTimeFormat(): string;
begin
  Result := FDateTimeFormat;
end;

function TAbstractLogAppender.AppendMessageTo(const LogLevel: TLogLevel;
  const CallableInfo: TCallableInfo; Message: string): boolean;
var
  LevelName: string;
  DatetimeString: string;
  CallableInfoString: string;
begin

  DateTimeToString(DatetimeString, FDateTimeFormat, now());
  LevelName := GetLevelName(LogLevel);
  CallableInfoString := specialize IfThen<String>(CallableInfo<>nil,CallableInfo.ToString,UNKNOWN_CALLABLE_STRING);
  {uses TRTLCriticalSection for work in multithreading environment.}
  EnterCriticalSection(FRTLCriticalSection);
  try
    Result:=WriteToAppendable(DatetimeString + ' ' + LevelName + ' ' +
      CallableInfoString + ' ' + Message);
  finally
    LeaveCriticalSection(FRTLCriticalSection);
  end;

end;


function TAbstractLogAppender.GetLogFilterList(): TLogFilterArray;
begin
  Result := FLogFilterArray;
end;

end.
