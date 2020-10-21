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
  MapFileBasedLogger.BasicTypes;

type

  {Free pascal map-file based logger for application(extended TCustomApplication)).
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }
  IMapFileBasedLogger = interface
    {The method processes message with specified log level and flushes it
     in output specified appenders.}
    procedure Log(LogLevel: TLogLevel; const Msg: string);
  end;

  { IMapFileInfoManipulator presents methods for load data from executable file location,
  read and write section from executable file. These methods are necessary for saving
  log information by build application CI/CD stage.}
  IMapFileInfoManipulator = interface
    {Try to load map file info from MapFilePath.@br
    After loading file located at MapFilePath will be deleted.
    @param ExeFilePath executable file location.}
    procedure LoadMapFile(ExeFilePath: string);
    {Write data new section with name 'Logger Data' into executable file after all section(in the end of file)
    Data will saved in gzip stream.
    @param ExeFilePath file for info saving.
    @raises EWriteError if error occured while file data processing.}
    procedure AppendSectionToExecutable(ExeFilePath: string);
    function ReadSectionFromExecutable(ExeFilePath: string): boolean;
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
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

  { TMapFileInfoManipulatorFactory }

  TMapFileInfoManipulatorFactory = class
    class function CreateMapFileInfoManipulator(ExeFilePath: string):
      IMapFileInfoManipulator;
  end;

resourcestring
  E_APPENDER_CALL_FAILED = 'don''t write to device appender called %s ';
  E_MAP_FILE_NOT_FOUND = 'CAUTION! MAP file not found!';

implementation

uses
  SysUtils,
  Types,
  Classes,
  fpjsonrtti,
  fpjson,
  PasZLib,
  LazFileUtils,
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
  private
  const
    MAP_FILE_EXTENSION = '.map';
  private
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
    function ReadLogSectionData(ExeFileName: string): TBytes;
    procedure WriteLogSectionData(ExeFileName: string; Data: TBytes);
  public
    procedure AppendSectionToExecutable(ExeFilePath: string);
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
    procedure LoadMapFile(ExeFilePath: string);
    function ReadSectionFromExecutable(ExeFilePath: string): boolean;
    destructor Destroy; override;
  end;

function TMapFileInfoManipulator.GetMapFilePath(ExeFilePath: string): string;
begin
  Result := ExeFilePath + MAP_FILE_EXTENSION;
end;

procedure TMapFileInfoManipulator.AppendSectionToExecutable(ExeFilePath: string);
var
  MapInfoJsonData: string;
  JsonStreamer: TJSONStreamer;
  CompressedBase64String: TBytes;
begin
  if FInitialized then
    exit;
  JsonStreamer := TJSONStreamer.Create(nil);
  MapInfoJsonData := JsonStreamer.ObjectToJSONString(FMapFileInfo);
  FreeAndNil(JsonStreamer);
  // CompressedBase64String := CompressMapInfoToGzip(MapInfoJsonData);
  WriteLogSectionData(ExeFilePath, TEncoding.UTF8.GetBytes(MapInfoJsonData));
  FInitialized := True;
end;

{function TMapFileInfoManipulator.CompressMapInfoToGzip(MapInfoJsonData: string): TBytes;
var
  CompressStream:     TZipper;
  TargetFileStream:   TBytesStream;
  SourceStringStream: TStringStream;
begin
  SourceStringStream := TStringStream.Create(MapInfoJsonData);
  TargetFileStream   := TBytesStream.Create();
  CompressStream     := Tcompressionstream.Create(Tcompressionlevel.cldefault,
    TargetFileStream);
  try
    CompressStream.SourceOwner := True;
    CompressStream.CopyFrom(SourceStringStream, 0);
    Result := TargetFileStream.Bytes;
  finally
    FreeAndNil(CompressStream);
    FreeAndNil(SourceStringStream);
  end;
end;}

function TMapFileInfoManipulator.FindCallerInfoByAddress(
  CallerPointer: CodePointer): TCallableInfo;
begin
  Result := FMapFileInfo.FindCallerInfoByAddress(CallerPointer);
end;

procedure TMapFileInfoManipulator.LoadMapFile(ExeFilePath: string);
var
  MapFileDataStringArray: TStringDynArray;
  i: integer;
  MapFilePath: string;
begin
  if FInitialized then
    exit;
  MapFilePath := GetMapFilePath(ExeFilePath);
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
  Result := MapFileDataStrings.ToStringArray(0, DataSectionIndex -
    MemoryMapIndex - 2);
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

function TMapFileInfoManipulator.ReadSectionFromExecutable(ExeFilePath: string): boolean;
var
  JsonDeStreamer: TJSONDeStreamer;
  MapInfoJsonData: string;
  SectionData: TBytes;
begin
  Result := False;
  if FInitialized then
    exit;
  SectionData := ReadLogSectionData(ExeFilePath);
  if SectionData = nil then
    exit;
  //  MapInfoJsonData := DecompressGzData(SectionData);
  SetLength(SectionData, 0);
  FMapFileInfo := TMapFileInfo.Create();
  JsonDeStreamer := TJSONDeStreamer.Create(nil);
  JsonDeStreamer.JSONToObject(MapInfoJsonData, FMapFileInfo);
  FreeAndNil(JsonDeStreamer);
  FInitialized := True;
end;

{function TMapFileInfoManipulator.DecompressGzData(Data: TBytes): string;
var
  DeCompressStream: TDeCompressionStream;
  SourceFileStream: TBytesStream;
  TargetStringStream: TStringList;
  Count: integer;
begin
  Result := '';
  SourceFileStream := TBytesStream.Create(Data);
  DeCompressStream := TDeCompressionStream.Create(SourceFileStream);
  TargetStringStream := TStringList.Create();
  try
    TargetStringStream.LoadFromStream(DeCompressStream);
    Result := TargetStringStream.Text;
  finally
    FreeAndNil(TargetStringStream);
    FreeAndNil(DeCompressStream);
    FreeAndNil(SourceFileStream);
  end;
end;}

function TMapFileInfoManipulator.ReadLogSectionData(ExeFileName: string): TBytes;
var
  AddSectionManiputlator: TAddSectionManiputlator;
begin
  AddSectionManiputlator := TAddSectionManiputlator.Create(ExeFileName);
  Result := AddSectionManiputlator.ReadSectionDataAsBytesAt(0);
  FreeAndNil(AddSectionManiputlator);
end;

procedure TMapFileInfoManipulator.WriteLogSectionData(ExeFileName: string;
  Data: TBytes);
var
  AddSectionManiputlator: TAddSectionManiputlator;
begin
  AddSectionManiputlator := TAddSectionManiputlator.Create(ExeFileName);
  AddSectionManiputlator.WriteDataSection(Data);
  FreeAndNil(AddSectionManiputlator);
end;

destructor TMapFileInfoManipulator.Destroy;
begin
  FreeAndNil(FMapFileInfo);
  FInitialized := False;
  inherited Destroy;
end;

{ TMapFileInfoManipulatorFactory }

class function TMapFileInfoManipulatorFactory.CreateMapFileInfoManipulator(
  ExeFilePath: string): IMapFileInfoManipulator;
begin
  Result := TMapFileInfoManipulator.Create;
  {Try to read data section from executable.}
  if Result.ReadSectionFromExecutable(ExeFilePath) then
    exit;
  {The section is empty. Try to load info from map file.}
  Result.LoadMapFile(ExtractFileNameWithoutExt(ExeFilePath));
  {Try to write data into executable. }
  Result.AppendSectionToExecutable(ExeFilePath);
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
