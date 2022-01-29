{Unit represented API for logging as slf4j(for java) for your pascal application.

How to use this unit?

@orderedList(
@item(Choose application build type 'Release' in Lazarus.)
@item(Add deifinition '-Xm' for map file generating.)
@item(Build your application.)
@item(Run first time for processing map file and generating '*.json' file
packed with gzip algorithm.)
@item(Distribute this file with application(in directory with main app file).)
)
@author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)
@created(2020-05-21)
@lastmod(2020-07-03)}
unit MapFileBasedLogger.Logger;

{$mode objfpc}{$H+}

interface

uses
  MapFileBasedLogger.Appenders,
  MapFileBasedLogger.BasicTypes,
  common.autocloseable;

type

  {Free pascal map-file based logger for application(extended TCustomApplication)).
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }
  IMapFileBasedLogger = interface
    {The method processes message with specified log level and flushes it
     in output specified appenders.}
    procedure Log(LogLevel: TLogLevel; const Msg: string);
  end;

const
  LOG_DATA_SECTION_NAME: string = 'LOG_DATA';

type
  { IMapFileInfoManipulator presents methods for load data from executable file location,
  read and write section from executable file. These methods are necessary for saving
  log information by build application CI/CD stage.}
  IMapFileInfoManipulator = interface

    {Try to load map file info from MapFilePath.@br
    After loading file located at MapFilePath will be deleted.}
    procedure LoadMapFile;
    {Write data new section with name 'Logger Data' into executable file after all section(in the end of file)
    Data will saved in gzip stream.
    @raises EWriteError if error occured while file data processing.}
    procedure AppendSectionToExecutable();
    function ReadSectionFromExecutable(): boolean;
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
  end;
  { TMapFileInfoManipulatorFactory }

  TMapFileInfoManipulatorFactory = class
    class function CreateMapFileInfoManipulator(AExeFilePath: string;
      AIsSectionCheck: boolean = False): IMapFileInfoManipulator;
  end;
type
   {Factory for Logger instance creating.
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }

  { TMapFileBasedLoggerFactory }

  TMapFileBasedLoggerFactory = class
    {Create new Logger from map file @bold(ExePath) with minimum loggable level @bold(BaseLoggerLevel)
    @param(MapFileInfoManipulator instance of )
    @param(Appenders appenders for this logger. May be ommited(logger don't write messages));
    @param(BaseLoggerLevel one of values @link(TLogLevel)).Message level order by name from higher to lowest. Logged message from BaseLoggerLevel
    and higher.
    @returns(@link(IMapFileBasedLogger) instance.)
    @raises(ELogException if not found map file for application.
    @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  )}    class
    function CreateLoggerFrom(MapFileInfoManipulator: IMapFileInfoManipulator;
      Appenders: TLogAppenderArray = nil;
      BaseLoggerLevel: TLogLevel = llInfo): IMapFileBasedLogger;
  end;



resourcestring
  E_APPENDER_CALL_FAILED = 'don''t write to device appender called %s ';
  E_MAP_FILE_NOT_FOUND = 'CAUTION! MAP file not found!';

implementation

uses
  SysUtils,
  Types,
  Classes,
  LazFileUtils,
  common.Data.Mapper,
  CustApp.AddSectionManiputlator;

type
  {Basic implementation IMapFileBasedLogger.
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }
  TMapFileBasedLogger = class(TInterfacedObject, IMapFileBasedLogger)
  strict private
    FMapFileInfoManipulator: IMapFileInfoManipulator;
    FBasicLevelIndex: integer;
    FAppenders: TLogAppenderArray;
  public
    {Creates new instance of logger for application ExeFilePath with list of appenders Appenders
    and BaseLoggerLevel basic message log level.}
    constructor Create(MapFileInfoManipulator: IMapFileInfoManipulator;
      Appenders: TLogAppenderArray; BaseLoggerLevel: TLogLevel);
    destructor Destroy; override;
    {@seeAlso(IMapFileBasedLogger.Log)}
    procedure Log(LogLevel: TLogLevel; const Msg: string);
  end;

  { TMapFileInfoManipulator }

  TMapFileInfoManipulator = class(TInterfacedObject, IMapFileInfoManipulator)
  strict private
    FMapFileInfo: TMapFileInfo;
    FInitialized: boolean;
    FSupportedMapper: IDataMapper;
    FExeFileName: string;
    {@param ExeFilePath exe path.
    @returns JsonFilePath ::=ExeFilePath + MAP_FILE_EXTENSION}
    function GetMapFilePath(ExeFilePath: string): string;
    {Trim map file to only code section data.
    @param MapFilePath map file location.
    @returns array of lines code section.}
    function LoadTrimmedMapFile(MapFilePath: string): TStringDynArray;
    {Get first line of data section index.
    @param Strings array of lines map file data.
    @returns index of first line that contains '.data' string.}
    function GetDataSectionIndex(Strings: TStrings): integer;
    function ReadLogSectionData(): string;
    class procedure TryWriteLogSectionDataProc(AAdditionalSectionManipulator:
      IAutoCloseable; AMethodData: ITriedMethodData); static;
    procedure WriteLogSectionData(Data: string);
    class function TryReadLogSectionDataProc(AAdditionalSectionManipulator:
      IAutoCloseable; AMethodData: ITriedMethodData): TStringList; static;
    class function CreateSectionManipulatorFunction(AMethodData: ITriedMethodData):
      IAutoCloseable; static;
  strict private
  const
    MAP_FILE_EXTENSION = '.map';
  private
    property ExeFileName: string read FExeFileName;
  public
    constructor Create(AExeFileName: string);
    destructor Destroy; override;
  public
    procedure AppendSectionToExecutable();
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
    procedure LoadMapFile();
    function ReadSectionFromExecutable(): boolean;
  end;

function TMapFileInfoManipulator.GetMapFilePath(ExeFilePath: string): string;
begin
  Result := ExeFilePath + MAP_FILE_EXTENSION;
end;

procedure TMapFileInfoManipulator.AppendSectionToExecutable();
var
  MapInfoJsonData: string;
begin
  if FInitialized then
    exit;
  MapInfoJsonData := FSupportedMapper.SerializeTo(FMapFileInfo, TMapFileInfo);
  WriteLogSectionData(MapInfoJsonData);
  FInitialized := True;
end;

function TMapFileInfoManipulator.FindCallerInfoByAddress(CallerPointer:
  CodePointer): TCallableInfo;
begin
  Result := FMapFileInfo.FindCallerInfoByAddress(CallerPointer);
end;

procedure TMapFileInfoManipulator.LoadMapFile();
var
  MapFileDataStringArray: TStringDynArray;
  i: integer;
  MapFilePath: string;
begin
  if FInitialized then
    exit;
  MapFilePath := GetMapFilePath(ExtractFileNameWithoutExt(FExeFileName));
  MapFileDataStringArray := LoadTrimmedMapFile(MapFilePath);
  for i := Low(MapFileDataStringArray) to High(MapFileDataStringArray) do
  begin
    MapFileDataStringArray[i] := MapFileDataStringArray[i].trim();
  end;
  FMapFileInfo := TMapFileInfo.Create(MapFileDataStringArray);
  SetLength(MapFileDataStringArray, 0);
  MapFileDataStringArray := nil;
  //DeleteFile(MapFilePath);
end;

function TMapFileInfoManipulator.LoadTrimmedMapFile(MapFilePath: string): TStringDynArray;
var
  MapFileData: TStringList;
  MemoryMapIndex: integer;
  DataSectionIndex: integer;
  MapFileDataStrings: TStrings;
begin
  MapFileData := TStringList.Create;
  MapFileData.LoadFromFile(MapFilePath);
  MemoryMapIndex := MapFileData.IndexOf(MAP_FILE_MEMORY_MAP_MARKER);
  DataSectionIndex := GetDataSectionIndex(MapFileData);
  MapFileDataStrings := MapFileData.Slice(MemoryMapIndex + 1);
  Result := MapFileDataStrings.ToStringArray(0, DataSectionIndex - MemoryMapIndex - 2);
  FreeAndNil(MapFileDataStrings);
  FreeAndNil(MapFileData);
end;

function TMapFileInfoManipulator.GetDataSectionIndex(Strings: TStrings): integer;
var
  i: integer;
begin
  Result := Strings.Count - 1;
  for i := 0 to Result do
  begin
    if Strings[i].trim().StartsWith(DATA_SECTION_MARKER) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TMapFileInfoManipulator.ReadSectionFromExecutable(): boolean;
var
  MapInfoJsonData: string;
begin
  Result := False;
  if FInitialized then
  begin
    Result := True;
    exit;
  end;
  MapInfoJsonData := string(ReadLogSectionData());
  if trim(MapInfoJsonData) = '' then
    exit;
  FMapFileInfo := FSupportedMapper.DeSerializeFrom(MapInfoJsonData,
    TMapFileInfo, TCreateObjectFunction(@TMapFileInfo.CreateMapFileInfo)) as TMapFileInfo;
  FInitialized := True;
  Result := True;
end;

class function TMapFileInfoManipulator.TryReadLogSectionDataProc(
  AAdditionalSectionManipulator: IAutoCloseable;
  AMethodData: ITriedMethodData): TStringList;
var
  ResultString: string;
begin
  Result := nil;
  ResultString := (AAdditionalSectionManipulator as
    IAdditionalSectionManager).ReadSectionDataByName(LOG_DATA_SECTION_NAME);
  Result := TStringList.Create;
  Result.Add(ResultString);
end;

class function TMapFileInfoManipulator.CreateSectionManipulatorFunction(
  AMethodData: ITriedMethodData): IAutoCloseable;
begin
  Result := nil;
  if Length(AMethodData.GetreferenceArray().GetReferences()) <> 1 then
    exit;
  if AMethodData.GetreferenceArray().GetReferences()[0] is TDummyElement then
    exit;
  if not (AMethodData.GetreferenceArray().GetReferences()[0] is
    TMapFileInfoManipulator) then
    exit;
  Result := TAdditionalSectionManagerFactory.CreateAdditionalSectionManager(
    (AMethodData.GetreferenceArray().GetReferences()[0] as
    TMapFileInfoManipulator).ExeFileName);
end;

function TMapFileInfoManipulator.ReadLogSectionData(): string;
var
  TriedResult: ITriedMethodResult;
  CreationMethodParams: TObjectDynArray;
begin
  Result := '';
  SetLength(CreationMethodParams, 1);
  CreationMethodParams[0] := self;
  TriedResult := TAutoCloseableExecutor.TryExecuteWithAutocloseable(
    TAutoCloseableCreatorFunction(@CreateSectionManipulatorFunction),
    TTriedFunction(@TryReadLogSectionDataProc), CreationMethodParams);
  if TriedResult.HasException then
  begin
    writeln(format('Section ''%s'' not found, cause: %s.',
      [LOG_DATA_SECTION_NAME, TriedResult.GetExceptionMessage]));
    TriedResult.SuppressException;
    TriedResult := nil;
    exit;
  end;
  if TriedResult.HasResult then
  begin
    Result := (TriedResult.GetResult as TStringList)[0];
    TriedResult := nil;
  end;
end;

class procedure TMapFileInfoManipulator.TryWriteLogSectionDataProc(
  AAdditionalSectionManipulator: IAutoCloseable; AMethodData: ITriedMethodData);
var
  Data: string;
begin
  if Length(AMethodData.GetreferenceArray().GetReferences()) <> 1 then
    exit;
  if AMethodData.GetreferenceArray().GetReferences()[0] is TDummyElement then
    exit;
  if not (AMethodData.GetreferenceArray().GetReferences()[0] is TStringList) then
    exit;
  Data := (AMethodData.GetreferenceArray().GetReferences()[0] as TStringList)[0];
  (AAdditionalSectionManipulator as IAdditionalSectionManager).WriteDataSection(
    LOG_DATA_SECTION_NAME, Data);
end;

procedure TMapFileInfoManipulator.WriteLogSectionData(Data: string);
var
  TriedResult: ITriedMethodResult;
  CreationMethodParams: TObjectDynArray;
  WriterMethodParam: TObjectDynArray;
begin
  SetLength(CreationMethodParams, 1);
  CreationMethodParams[0] := self;
  SetLength(WriterMethodParam, 1);
  WriterMethodParam[0] := TStringList.Create;
  (WriterMethodParam[0] as TStringList).Add(Data);
  TriedResult := TAutoCloseableExecutor.TryExecuteWithAutocloseable(
    TAutoCloseableCreatorFunction(@CreateSectionManipulatorFunction),
    TTriedProc(@TryWriteLogSectionDataProc), CreationMethodParams, WriterMethodParam);
  FreeAndNil(WriterMethodParam[0]);
  if TriedResult.HasException then
  begin
    writeln(format('Section ''%s'' can''t written, cause: %s.',
      [LOG_DATA_SECTION_NAME, TriedResult.GetExceptionMessage]));
    TriedResult.SuppressException;
    TriedResult := nil;
    exit;
  end;
  TriedResult := nil;
end;

constructor TMapFileInfoManipulator.Create(AExeFileName: string);
begin
  FInitialized := False;
  FSupportedMapper := TDataMapperFactory.CreateDataMapper(mtJSON);
  FExeFileName := AExeFileName;
end;

destructor TMapFileInfoManipulator.Destroy;
begin
  FreeAndNil(FMapFileInfo);
  FExeFileName := '';
  FSupportedMapper := nil;
  FInitialized := False;
  inherited Destroy;
end;

{ TMapFileInfoManipulatorFactory }

class function TMapFileInfoManipulatorFactory.CreateMapFileInfoManipulator(
  AExeFilePath: string; AIsSectionCheck: boolean): IMapFileInfoManipulator;
var
  IsSectionExists: boolean;
begin
  Result := TMapFileInfoManipulator.Create(AExeFilePath);
  IsSectionExists := False;
  {Try to read data section from executable.}
  IsSectionExists := Result.ReadSectionFromExecutable();

  if not IsSectionExists and AIsSectionCheck then
    raise EFCreateError.Create('Log section excepted but not found!');
  if IsSectionExists then
    exit;
  {The section is empty. Try to load info from map file.}
  Result.LoadMapFile();
  {Try to write data into executable. }
  Result.AppendSectionToExecutable();
end;

{ TMapFileBasedLoggerFactory }

class function TMapFileBasedLoggerFactory.CreateLoggerFrom(
  MapFileInfoManipulator: IMapFileInfoManipulator; Appenders: TLogAppenderArray;
  BaseLoggerLevel: TLogLevel): IMapFileBasedLogger;
begin
  Result := TMapFileBasedLogger.Create(MapFileInfoManipulator, Appenders,
    BaseLoggerLevel);
end;

{ TMapFileBasedLogger }

constructor TMapFileBasedLogger.Create(MapFileInfoManipulator: IMapFileInfoManipulator;
  Appenders: TLogAppenderArray; BaseLoggerLevel: TLogLevel);
begin
  FBasicLevelIndex := Ord(BaseLoggerLevel);
  FMapFileInfoManipulator := MapFileInfoManipulator;
  FAppenders := Appenders;
end;

destructor TMapFileBasedLogger.Destroy;
var
  i: integer;
begin
  for i := Low(FAppenders) to High(FAppenders) do
  begin
    FAppenders[i] := nil;
  end;
  SetLength(FAppenders, 0);
  FMapFileInfoManipulator := nil;
  inherited Destroy;
end;

procedure TMapFileBasedLogger.Log(LogLevel: TLogLevel; const Msg: string);
var
  CallerPointer: CodePointer;
  CallerInfo: TCallableInfo;
  i: integer;
  Appender: ILogAppender;
begin
  CallerPointer := get_caller_addr(get_frame);
  CallerInfo := FMapFileInfoManipulator.FindCallerInfoByAddress(CallerPointer);

  if (FAppenders = nil) then
    exit;
  for i := Low(FAppenders) to High(FAppenders) do
  begin
    Appender := FAppenders[i];
    if not (Appender.AppendMessageTo(LogLevel, CallerInfo, Msg)) then
      raise EWriteError.CreateFmt(E_APPENDER_CALL_FAILED,
        [TObject(Appender).QualifiedClassName]);

  end;
end;

end.
