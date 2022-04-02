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
  { IMapFileInfoManager presents methods for load data from executable file location,
  read and write section from executable file. These methods are necessary for saving
  log information by build application CI/CD stage.}
  IMapFileInfoManager = interface
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
  end;

  { TMapFileInfoManagerFactory }

  TMapFileInfoManagerFactory = class
    class function CreateMapFileInfoManager(AExeFilePath: string; MethodSignatureFilter: string = ''; AIsSectionCheck: boolean = False): IMapFileInfoManager;
  end;

type

  {Free pascal map-file based logger for application(extended TCustomApplication)).
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }
  IMapFileBasedLogger = interface
    procedure Trace(const Msg: string; parameters: array of const);
    procedure Debug(const Msg: string; parameters: array of const);
    procedure Config(const Msg: string; parameters: array of const);
    procedure Info(const Msg: string; parameters: array of const);
    procedure Warning(const Msg: string; parameters: array of const);
    procedure Error(const Msg: string; parameters: array of const);
    procedure Trace(const Msg: string);
    procedure Debug(const Msg: string);
    procedure Config(const Msg: string);
    procedure Info(const Msg: string);
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
  end;

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
    function CreateLoggerFrom(MapFileInfoManipulator: IMapFileInfoManager; Appenders: TLogAppenderArray = nil;
      BaseLoggerLevel: TLogLevel = llInfo): IMapFileBasedLogger;
  end;

const
  LOG_DATA_SECTION_NAME: string = 'LOG_DATA';

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

const


  MAP_FILE_EXTENSION = '.map';

type
  { TMapFileInfoManager }
  TMapFileInfoManager = class(TInterfacedObject, IMapFileInfoManager)
  strict private
    FInitialized: boolean;
    FExeFileName: string;
    FMapFileInfo: TMapFileInfo;
    procedure WriteLogSectionData(Data: string);
    {Try to load map file info from MapFilePath.@br
    After loading file located at MapFilePath will be deleted.}
    procedure LoadMapFile(MethodSignatureFilter: string);
    {Write data new section with name 'Logger Data' into executable file after all section(in the end of file)
    Data will saved in gzip stream.
    @raises EWriteError if error occured while file data processing.}
    procedure AppendSectionToExecutable();
    function ReadSectionFromExecutable(): boolean;
    {@param ExeFilePath exe path.
    @returns JsonFilePath ::=ExeFilePath + MAP_FILE_EXTENSION}
    function GetMapFilePath(ExeFilePath: string): string;
    {Trim map file to only code section data.
    @param MapFilePath map file location.
    @returns array of lines code section.}
    function LoadTrimmedMapFile(MapFilePath: string): TStringDynArray;
    function GetCodeSectionStartIndex(Strings: TStringDynArray): SizeInt;
    function GetCodeSectionEndIndex(Strings: TStringDynArray): SizeInt;
    function GetCodeSectionIndex(Strings: TStringDynArray; Marker: string): SizeInt;
    function ReadLogSectionData(): string;
    class procedure TryWriteLogSectionDataProc(AAdditionalSectionManipulator: IAutoCloseable; AMethodData: ITriedMethodData); static;
    class function TryReadLogSectionDataProc(AAdditionalSectionManipulator: IAutoCloseable; AMethodData: ITriedMethodData): TStringList; static;
    class function CreateSectionManipulatorFunction(AMethodData: ITriedMethodData): IAutoCloseable; static;
  private
    property ExeFileName: string read FExeFileName;
  public
    constructor Create(AExeFileName: string; MethodSignatureFilter: string; AIsSectionCheck: boolean);
    destructor Destroy; override;
  public
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
  end;

  {Basic implementation IMapFileBasedLogger.
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }

  { TMapFileBasedLogger }

  TMapFileBasedLogger = class(TInterfacedObject, IMapFileBasedLogger)
  strict private
    FMapFileInfoManipulator: IMapFileInfoManager;
    FBasicLevelIndex: integer;
    FAppenders: TLogAppenderArray;
    procedure Log(ALogLevel: TLogLevel; const Msg: string; parameters: array of const);
  public
    {Creates new instance of logger for application ExeFilePath with list of appenders Appenders
    and BaseLoggerLevel basic message log level.}
    constructor Create(MapFileInfoManipulator: IMapFileInfoManager; Appenders: TLogAppenderArray; BaseLoggerLevel: TLogLevel);
    destructor Destroy; override;
    procedure Trace(const Msg: string; parameters: array of const);
    procedure Debug(const Msg: string; parameters: array of const);
    procedure Config(const Msg: string; parameters: array of const);
    procedure Info(const Msg: string; parameters: array of const);
    procedure Warning(const Msg: string; parameters: array of const);
    procedure Error(const Msg: string; parameters: array of const);
    procedure Trace(const Msg: string);
    procedure Debug(const Msg: string);
    procedure Config(const Msg: string);
    procedure Info(const Msg: string);
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
  end;

{ TMapFileInfoManager }
constructor TMapFileInfoManager.Create(AExeFileName: string; MethodSignatureFilter: string; AIsSectionCheck: boolean);
  var
    IsSectionExists: boolean;
  begin
    IsSectionExists := False;
    FInitialized := False;
    FExeFileName := AExeFileName;
    {Try to read data section from executable.}
    IsSectionExists := ReadSectionFromExecutable();
    if not IsSectionExists and AIsSectionCheck then
      raise EFCreateError.Create('Log section excepted but not found!');
    if IsSectionExists then
      exit;
    {The section is empty. Try to load info from map file.}
    LoadMapFile(MethodSignatureFilter);
    {Try to write data into executable. }
    AppendSectionToExecutable();
  end;

destructor TMapFileInfoManager.Destroy;
  begin
    FreeAndNil(FMapFileInfo);
    FExeFileName := EmptyStr;
    FInitialized := False;
    inherited Destroy;
  end;

procedure TMapFileInfoManager.WriteLogSectionData(Data: string);
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
    TriedResult := TAutoCloseableExecutor.TryExecuteWithAutocloseable(TAutoCloseableCreatorFunction(@CreateSectionManipulatorFunction),
      TTriedProc(@TryWriteLogSectionDataProc), CreationMethodParams, WriterMethodParam);
    FreeAndNil(WriterMethodParam[0]);
    if TriedResult.HasException then
    begin
      writeln(format('Section ''%s'' can''t written, cause: %s.', [LOG_DATA_SECTION_NAME, TriedResult.GetExceptionMessage]));
      TriedResult.SuppressException;
      TriedResult := nil;
      exit;
    end;
    TriedResult := nil;
  end;

procedure TMapFileInfoManager.LoadMapFile(MethodSignatureFilter: string);
  var
    MapFileDataStringArray: TStringDynArray;
    MapFilePath: string;
  begin
    if FInitialized then
      exit;
    MapFilePath := GetMapFilePath(ExtractFileNameWithoutExt(FExeFileName));
    MapFileDataStringArray := LoadTrimmedMapFile(MapFilePath);
    FMapFileInfo := TMapFileInfo.Create(MapFileDataStringArray, MethodSignatureFilter);
    SetLength(MapFileDataStringArray, 0);
    DeleteFile(MapFilePath);
  end;

procedure TMapFileInfoManager.AppendSectionToExecutable();
  var
    MapInfoJsonData: string;
    SupportedMapper: IDataMapper;
  begin
    if FInitialized then
      exit;
    SupportedMapper := TDataMapperFactory.CreateDataMapper(mtJSON);
    try
      MapInfoJsonData := SupportedMapper.SerializeTo(FMapFileInfo, TMapFileInfo);
      WriteLogSectionData(MapInfoJsonData);
      FInitialized := True;
    finally
      SupportedMapper := nil;
    end;
  end;

function TMapFileInfoManager.GetMapFilePath(ExeFilePath: string): string;
  begin
    Result := ExeFilePath + MAP_FILE_EXTENSION;
  end;

function TMapFileInfoManager.LoadTrimmedMapFile(MapFilePath: string): TStringDynArray;
  var
    MapFileData: TStringList;
    CodeSectionStartIndex: integer;
    CodeSectionEndIndex: integer;
    MapFileDataStrings: TStringDynArray;
  begin
    MapFileData := TStringList.Create;
    try
      MapFileData.LoadFromFile(MapFilePath);
      MapFileDataStrings := MapFileData.ToStringArray;
      CodeSectionStartIndex := GetCodeSectionStartIndex(MapFileDataStrings);
      CodeSectionEndIndex := GetCodeSectionEndIndex(MapFileDataStrings);
      Result := MapFileData.ToStringArray(CodeSectionStartIndex, CodeSectionEndIndex);
    finally
      SetLength(MapFileDataStrings, 0);
      FreeAndNil(MapFileData);
    end;
  end;

function TMapFileInfoManager.GetCodeSectionStartIndex(Strings: TStringDynArray): SizeInt;
  begin
    Result := GetCodeSectionIndex(Strings, CODE_SECTION_START_MARKER);
  end;

function TMapFileInfoManager.GetCodeSectionEndIndex(Strings: TStringDynArray): SizeInt;
  begin
    Result := GetCodeSectionIndex(Strings, CODE_SECTION_END_MARKER);
  end;

function TMapFileInfoManager.GetCodeSectionIndex(Strings: TStringDynArray; Marker: string): SizeInt;
  var
    i: integer;
  begin
    Result := Length(Strings) - 1;
    for i := 0 to Result do
      if Strings[i].trim().StartsWith(Marker) then
      begin
        Result := i;
        break;
      end;

  end;


function TMapFileInfoManager.ReadSectionFromExecutable(): boolean;
  var
    MapInfoJsonData: string;
    SupportedMapper: IDataMapper;
  begin
    Result := False;
    if FInitialized then
    begin
      Result := True;
      exit;
    end;
    MapInfoJsonData := string(ReadLogSectionData());
    if trim(MapInfoJsonData) = EmptyStr then
      exit;
    SupportedMapper := TDataMapperFactory.CreateDataMapper(MapperType.mtJSON);
    try
      FMapFileInfo := SupportedMapper.DeSerializeFrom(MapInfoJsonData, TMapFileInfo, TCreateObjectFunction(@TMapFileInfo.CreateMapFileInfo)) as
        TMapFileInfo;
      FInitialized := True;
      Result := True;
    finally
      SupportedMapper := nil;
    end;
  end;

function TMapFileInfoManager.ReadLogSectionData(): string;
  var
    TriedResult: ITriedMethodResult;
    CreationMethodParams: TObjectDynArray;
  begin
    Result := EmptyStr;
    SetLength(CreationMethodParams, 1);
    CreationMethodParams[0] := self;
    TriedResult := TAutoCloseableExecutor.TryExecuteWithAutocloseable(TAutoCloseableCreatorFunction(@CreateSectionManipulatorFunction),
      TTriedFunction(@TryReadLogSectionDataProc), CreationMethodParams);
    if TriedResult.HasException then
    begin
      writeln(format('Section ''%s'' not found, cause: %s.', [LOG_DATA_SECTION_NAME, TriedResult.GetExceptionMessage]));
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

function TMapFileInfoManager.FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
  begin
    Result := FMapFileInfo.FindCallerInfoByAddress(CallerPointer);
  end;

class function TMapFileInfoManager.TryReadLogSectionDataProc(AAdditionalSectionManipulator: IAutoCloseable; AMethodData: ITriedMethodData): TStringList;
  var
    ResultString: string;
  begin
    Result := nil;
    ResultString := (AAdditionalSectionManipulator as IAdditionalSectionManager).ReadSectionDataByName(LOG_DATA_SECTION_NAME);
    Result := TStringList.Create;
    Result.Add(ResultString);
  end;

class function TMapFileInfoManager.CreateSectionManipulatorFunction(AMethodData: ITriedMethodData): IAutoCloseable;
  begin
    Result := nil;
    if Length(AMethodData.GetreferenceArray().GetReferences()) <> 1 then
      exit;
    if AMethodData.GetreferenceArray().GetReferences()[0] is TDummyElement then
      exit;
    if not (AMethodData.GetreferenceArray().GetReferences()[0] is TMapFileInfoManager) then
      exit;
    Result := TAdditionalSectionManagerFactory.CreateAdditionalSectionManager(
      (AMethodData.GetreferenceArray().GetReferences()[0] as TMapFileInfoManager).ExeFileName);
  end;

class procedure TMapFileInfoManager.TryWriteLogSectionDataProc(AAdditionalSectionManipulator: IAutoCloseable; AMethodData: ITriedMethodData);
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

{ TMapFileInfoManagerFactory }

class function TMapFileInfoManagerFactory.CreateMapFileInfoManager(AExeFilePath: string; MethodSignatureFilter: string;
  AIsSectionCheck: boolean): IMapFileInfoManager;
  begin
    Result := TMapFileInfoManager.Create(AExeFilePath, MethodSignatureFilter, AIsSectionCheck);
  end;

{ TMapFileBasedLoggerFactory }

class function TMapFileBasedLoggerFactory.CreateLoggerFrom(MapFileInfoManipulator: IMapFileInfoManager; Appenders: TLogAppenderArray;
  BaseLoggerLevel: TLogLevel): IMapFileBasedLogger;
  begin
    Result := TMapFileBasedLogger.Create(MapFileInfoManipulator, Appenders, BaseLoggerLevel);
  end;

{ TMapFileBasedLogger }

constructor TMapFileBasedLogger.Create(MapFileInfoManipulator: IMapFileInfoManager; Appenders: TLogAppenderArray; BaseLoggerLevel: TLogLevel);
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
      FAppenders[i] := nil;
    SetLength(FAppenders, 0);
    FMapFileInfoManipulator := nil;
    inherited Destroy;
  end;

procedure TMapFileBasedLogger.Log(ALogLevel: TLogLevel; const Msg: string; parameters: array of const);
  var
    CallerPointer: CodePointer;
    CallerInfo: TCallableInfo;
    i: integer;
    Appender: ILogAppender;
    targetMsg: string;
  begin
    if Ord(ALogLevel) < Ord(FBasicLevelIndex) then exit;

    CallerPointer :=
      {real caller address from stack frame}
      get_caller_addr(
      {plain call from +1 level abstraction}
      get_caller_frame(
      {for current frame}
      get_frame));
    CallerInfo := FMapFileInfoManipulator.FindCallerInfoByAddress(CallerPointer);
    if (FAppenders = nil) then
      exit;
    targetMsg := msg;
    if (Length(parameters) > 0) then
      targetMsg := format(Msg, parameters);
    for i := 0 to Length(FAppenders) - 1 do
    begin
      Appender := FAppenders[i];
      if not (Appender.AppendMessageTo(ALogLevel, CallerInfo, targetMsg)) then
        raise EWriteError.CreateFmt(E_APPENDER_CALL_FAILED, [TObject(Appender).QualifiedClassName]);

    end;
  end;

procedure TMapFileBasedLogger.Trace(const Msg: string; parameters: array of const);
  begin
    Log(llTrace, msg, parameters);
  end;

procedure TMapFileBasedLogger.Debug(const Msg: string; parameters: array of const);
  begin
    Log(llDebug, msg, parameters);
  end;

procedure TMapFileBasedLogger.Config(const Msg: string; parameters: array of const);
  begin
    Log(llConfig, msg, parameters);
  end;

procedure TMapFileBasedLogger.Info(const Msg: string; parameters: array of const);
  begin
    Log(llInfo, msg, parameters);
  end;

procedure TMapFileBasedLogger.Warning(const Msg: string; parameters: array of const);
  begin
    Log(llWarn, msg, parameters);
  end;

procedure TMapFileBasedLogger.Error(const Msg: string; parameters: array of const);
  begin
    Log(llError, msg, parameters);
  end;

procedure TMapFileBasedLogger.Trace(const Msg: string);
  begin
     Log(llTrace, msg, []);
  end;

procedure TMapFileBasedLogger.Debug(const Msg: string);
  begin
    Log(llDebug, msg, []);
  end;

procedure TMapFileBasedLogger.Config(const Msg: string);
  begin
    Log(llConfig, msg, []);
  end;

procedure TMapFileBasedLogger.Info(const Msg: string);
  begin
    Log(llInfo, msg, []);
  end;

procedure TMapFileBasedLogger.Warning(const Msg: string);
  begin
    Log(llWarn, msg, []);
  end;

procedure TMapFileBasedLogger.Error(const Msg: string);
  begin
    Log(llError, msg, []);
  end;

end.
