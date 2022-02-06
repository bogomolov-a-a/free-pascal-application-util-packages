{Basic types for MapFileBasedLogger}
unit MapFileBasedLogger.BasicTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types;

const
  CODE_SECTION_START_MARKER = '.text';
  CODE_ITEM_START_MARKER = CODE_SECTION_START_MARKER + '.';
  CODE_ITEM_START_MARKER_LENGTH = Length(CODE_ITEM_START_MARKER);

type
   {Message level for logger
  @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)  }
  TLogLevel = (
    {For sequntial debugging application(step by step debugging).}
    llTrace,
    {For debugging without entering and leaving methods.}
    llDebug,
    {For check configuration parameters, such as db connection string etc.}
    llConfig,
    {Basic message level, for information about apllication work.}
    llInfo,

  {Warning message level, for errors that aren't critical for application(for example, network
  unavailable but work with local data processing can be continued).Work can't be continued.}
    llWarn,
    {Error message level, for errors that are critical for application,
    work can't be continued.}
    llError,
    {User message level, for example only specified messages}
    llCustom,
    {turn off logging}
    llOff);

  {Parsed callable info.}

  { TCallableInfo }

  TCallableInfo = class(TCollectionItem)
  strict private
    FUnitName: string;
    FClassName: string;
    FMethodName: string;
    FAdditionalCallableInfo: string;
    FParameters: TStringList;
    FReturnType: string;
    FStartCallableAddress: SizeInt;
    FEndCallableAddress: SizeInt;
  public
    constructor Create(ACollection: TCollection); override;
    {Check Address in callable address  range.
    @param Address caller address
    @returns @true if Address in callable address  range}
    function IsMethodGammaAddress(Address: Pointer): boolean;
    {String containing: TCallableInfo.UnitName +LOG_PART_SEPARATOR+ TCallableInfo.ClassName +LOG_PART_SEPARATOR+ TCallableInfo.MethodNames}
    function ToString: ansistring; override;
    {Cleaning state.}
    destructor Destroy; override;
  published
    {Unit name. Can't be ''}
    property UnitName: string read FUnitName write FUnitName;
    {Class name. Can be ''}
    property ClassName: string read FClassName write FClassName;
    {Method names list. Can be empty(for entry point)}
    property MethodName: string read FMethodName write FMethodName;
    property AdditionalCallableInfo: string
      read FAdditionalCallableInfo write FAdditionalCallableInfo;
    {Parameter type names list. Can be empty}
    property Parameters: TStringList read FParameters write FParameters;
    {Return type name. Can be ''}
    property ReturnType: string read FReturnType write FReturnType;
    {Range start address, inclusive.}
    property StartCallableAddress: SizeInt read FStartCallableAddress
      write FStartCallableAddress;
    {Range end address, inclusive.}
    property EndCallableAddress: SizeInt read FEndCallableAddress
      write FEndCallableAddress;
  end;

  { TCallableInfoCollectionEnumerator }

  TCallableInfoCollectionEnumerator = class(TCollectionEnumerator)
  strict private
    function GetCurrent: TCallableInfo;
  public
    property Current: TCallableInfo read GetCurrent;
  end;

  { TCallableInfoCollection }

  TCallableInfoCollection = class(TCollection)
  private
    procedure SetItem(Index: integer; AValue: TCallableInfo);
    function GetItem(Index: integer): TCallableInfo;
  public
    constructor Create;
    function Add: TCallableInfo;
    property Items[Index: integer]: TCallableInfo read GetItem write SetItem;
    function GetEnumerator: TCallableInfoCollectionEnumerator;
    class function CreateCallableInfoCollection: TCollection; static;
  end;

  {Linker map file code section start marker.}
  {
    Persistence Map file info containing parsed callable info.
    Must be serialized into file and distributed with application.@br
     @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)
  }

  { TMapFileInfo }

  TMapFileInfo = class(TPersistent)
  strict  private
    FCallableInfoList: TCallableInfoCollection;
    procedure ParsePreparedMapFileData(AMapFileData: TStringDynArray);
    procedure HandleMethodSignature(AMapFileData: TStringDynArray;
      var Index: integer; var ResultArray: TStringDynArray);
    {Exclude from AMapFileData strings with (@link TRY_RES_STRING_MODULE_NAME) and (@link TRY_TC_MODULE_NAME)}
    function FilterMapFileDataString(AMapFileData: TStringDynArray;
      MethodSignatureFilter: string): TStringDynArray;
    function IsAvailableMethodSignature(TestingMethodSignature: string;
      MethodSignatureFilters: TStringArray): boolean;
    function IsPascalMain(TestingSignature: string): boolean;
    procedure HandleRegularMethod(AMapFileData: TStringDynArray;
      var Index: integer; var ResultArray: TStringDynArray);
    procedure HandlePascalMain(MethodSignature: string;
      var ResultArray: TStringDynArray);

  public
    {Creates instance from AMapFileData lines.
    @param AMapFileData lines from map file in '.text' section(only code).}
    constructor Create(AMapFileData: TStringDynArray; MethodSignatureFilter: string);
    {For serialization}
    constructor Create;
    {For serialization, cleaning state.}
    destructor Destroy; override;
    {find callable info by CallerPointer
    @param CallerInfo code pointer from stack frame
    @returns @nil, if callable info for passed CallerPointer not found.}
    function FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
    class function CreateMapFileInfo: TObject; static;
  published
    {list of callable infos.
    @seeAlso(TCallableInfo)}
    property CallableInfoList: TCallableInfoCollection read FCallableInfoList;
  end;

  EMapInfoCreateException = class(Exception);

const
  {callable info part separator }
  LOG_PART_SEPARATOR = ':';
  {procedure marker}
  PROC_TYPE = 'PROC';
  {function marker}
  FUNC_TYPE = 'FUNC';

const
  PASCAL_MAIN_METHOD_SIGNATURE = 'n_main';
  PASCAL_PROGRAM_NAMESPACE = 'n_p$';
  BASIC_METHOD_SIGNATURE_FILTER =
    PASCAL_PROGRAM_NAMESPACE + ',' + PASCAL_MAIN_METHOD_SIGNATURE;
  N_MAIN_METHOD_SIGNATURE_FILTER = CODE_ITEM_START_MARKER + PASCAL_MAIN_METHOD_SIGNATURE;
//----------------------REGEXP---------------------------------------------//
const
  BASICALLY_METHOD_SIGNATURE_PATTERN =
    '^(n\_)(main|((p\$)*([a-z][a-z0-9\_\.]+))(\$)*(\_\$)+(([a-z][a-z0-9\.]*)*(\_\$)+((\_)+(([a-z][a-z0-9\.]*)+((\$[a-z0-9\.]+)*)((\$\$[a-z0-9\.]*)*))(\_\$)?)*|(((?12))((?14))((?15))((?17))((?19))))*(\_\_\$)*(\$\_)((?14))((?15))((?17)))$';
//----------------------END REGEXP-----------------------------------------//
implementation

uses Regexpr, StrUtils;

type
{ The finite state machine implemented following grammatic:
    Map file structure contains one more sections(and linker log),
  but we are currently interested only '.text' section.
  It mapped to real address table for methods(and units, classes in runtime).@br
  This section started with constant @link(MAP_FILE_MEMORY_MAP_MARKER).@br
  BNF form for map file certain line for parsing:@br
  @bold(<mapFileHeaderString> ::= [<applicationEntry><moduleNameSeparator>]<moduleName>
  <moduleNameSeparator> [<classNameSeparator><className><classNameSeparator>]
  [<methodDefintionSeparator><methodDefinitions>][<targetMethodSeparator><methodDefinition>]
  [' '<startAddress>' '<length>' '<moduleObjectFile>],
  where)
  @orderedList(
  @item(@bold(applicationEntry), application main module(with @italic(lpr module)).
  Value must be only 'n_p')
  @item(@bold(moduleNameSeparator), symbol @link(UNIT_NAME_TAIL_SEPARATOR))
  @item(@bold(moduleName), unit that contains method(or class).Started with 'n_'
  and must satisfy pattern 'n_[A-Za-z0-9_]*|n_main'. 'main' - program entry point
  main 'begin...end' block. @link(UNIT_PROGRAM_IDENTIFIER))
  @item(@bold(classNameSeparator), symbol @link(CLASS_NAME_SEPARATOR))
  @item(@bold(className), class name that contains method(can be empty
  if this method is owned unit). Must be satisfy pattern '[A-Za-z0-9_]*')
  @item(@bold(methodDefinitionSeparator) for non target method(outer methods for any inner)
  Value must be @link(METHOD_DEFINITION_SEPARATOR))
  @item(@bold(<methodDefinitions>) ::= <methodDefintionSeparator>[<methodDefinitions>|<methodDefinition>],
  method definition. Each module definition contain raw method name, list of parameters
  and  return type if exists one of them.)
  @item(@bold(<methodDefinition>) ::=<methodName>[<parameterTypeNameSeparator><parameterTypeName>]
  [<returTypeSeparator><returnTypeName>])
  @item(@bold(methodName), method name. Must be satisfy [A-Za-z0-9]* because
  symbol '_' is methodDefinitionSeparator)
  @item(@bold(parameterTypeNameSeparator), symbol @link(PARAMETER_TYPE_NAME_SEPARATOR))
  @item(@bold(parameterTypeName), parameter type name. Must be satisfy pattern '[A-Za-z0-9]*')
  @item(@bold(returTypeSeparator), symbol @link(RETURN_TYPE_NAME_SEPARATOR))
  @item(@bold(returnTypeName), return value type name. Must be satisfy pattern '[A-Za-z0-9]*')
  @item(@bold(targetMethodSeparator), symbol @link(TARGET_METHOD_DEFINITION_SEPARATOR))
  @item(@bold(startAddress), start entry point(main) address, in bytes )
  @item(@bold(length), length entry point(main) address, in bytes)
  @item(@bold(moduleObjectFile), object file for module(e.g. 'moduleName.o'))
  )
  All names are in lower cases!@br
  @bold(<addressInfoString> ::= <startAddress>' '<length>' '<moduleObjectFile>,where)
  @orderedList(
     @item(@bold(startAddress), start entry point(main) address, in bytes )
  @item(@bold(length), length entry point(main) address, in bytes)
  @item(@bold(moduleObjectFile), object file for module(e.g. 'moduleName.o'))
  )
   @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)
 }

  { TCallableInfoParser }

  TCallableInfoParser = class
  strict private
    FCallableInfoList: TCallableInfoCollection;
    FRegExp: TRegExpr;
    procedure FillAddressInfo(CallableInfo: TCallableInfo; AddressInfoString: string);
    procedure FillNamingInfo(CallableInfo: TCallableInfo;
      MethodSignatureString: string);
  public
   {Creates parser instance with specified CallableInfoList.
   @param CallableInfoList TMapFileInfo.CallableInfoList
   }
    constructor Create(var CallableInfoList: TCallableInfoCollection);
    destructor Destroy; override;
   {Add new item in callable collection
   @param MethodSignatureString <mapFileHeaderString>
   @param AddressInfoString <addressInfoString>}
    procedure ParseCallableInfoFromStrings(MethodSignatureString: string;
      AddressInfoString: string);
  end;

{ TCallableInfo }

constructor TCallableInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParameters := TStringList.Create;
end;

destructor TCallableInfo.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

function TCallableInfo.IsMethodGammaAddress(Address: Pointer): boolean;
begin
  Result := (CodePtrUInt(Address) >= FStartCallableAddress) and
    (CodePtrUInt(Address) <= FEndCallableAddress);
end;

function TCallableInfo.ToString: ansistring;
begin
  Result := FUnitName + LOG_PART_SEPARATOR + FClassName;
{+ LOG_PART_SEPARATOR +
    FMethodNames[0]};
  if FReturnType = EmptyStr then
    Result := Result + LOG_PART_SEPARATOR + PROC_TYPE
  else
    Result := Result + LOG_PART_SEPARATOR + FUNC_TYPE;
end;

{ TMapFileInfo }

constructor TMapFileInfo.Create(AMapFileData: TStringDynArray;
  MethodSignatureFilter: string);
var
  FilteredMapFileData: TStringDynArray;
  Temp: TStringList;
  i: integer;
begin
  FilteredMapFileData := FilterMapFileDataString(AMapFileData, MethodSignatureFilter);
  Create;
  Temp := TStringList.Create();
  try
    Temp.Duplicates := TDuplicates.dupError;
    for i := 0 to round(Length(FilteredMapFileData) / 2) - 1 do
      Temp.Add(FilteredMapFileData[i * 2]);
    Temp.Sorted := True;
    Temp.SaveToFile(
      'c:\Users\artem.bogomolov.a\develop\projects\private\family-budget\family-budget-server\target\map.1');
  finally
    FreeAndNil(Temp);
  end;
  ParsePreparedMapFileData(FilteredMapFileData);
end;

constructor TMapFileInfo.Create();
begin
  inherited Create;
  FCallableInfoList := TCallableInfoCollection.Create();
end;

destructor TMapFileInfo.Destroy;
begin
  FCallableInfoList.Clear;
  FreeAndNil(FCallableInfoList);
  inherited Destroy;
end;

procedure TMapFileInfo.ParsePreparedMapFileData(AMapFileData: TStringDynArray);
var
  i, Count: integer;
  MethodSignatureString: string;
  AddressInfoString: string;
  CallableInfoParser: TCallableInfoParser;
begin
  i := 0;
  Count := High(AMapFileData);
  CallableInfoParser := TCallableInfoParser.Create(FCallableInfoList);
  while i < Count - 1 do
  begin
    MethodSignatureString := AMapFileData[i];
    Inc(i);
    AddressInfoString := AMapFileData[i];
    Inc(i);
    CallableInfoParser.ParseCallableInfoFromStrings(
      MethodSignatureString, AddressInfoString);
  end;
  FreeAndNil(CallableInfoParser);
end;

procedure TMapFileInfo.HandleMethodSignature(AMapFileData: TStringDynArray;
  var Index: integer; var ResultArray: TStringDynArray);
var
  TestingMethodSignature: string;
begin
  TestingMethodSignature := AMapFileData[Index].trim();
  if IsPascalMain(TestingMethodSignature) then
  begin
    HandlePascalMain(TestingMethodSignature, ResultArray);
    exit;
  end;
  HandleRegularMethod(AMapFileData, Index, ResultArray);
end;

procedure TMapFileInfo.HandlePascalMain(MethodSignature: string;
  var ResultArray: TStringDynArray);
var
  PascalMainMethodArray: TStringArray;
  PascalMainTempMapArray: TStringDynArray;
  Index: integer;
begin
  PascalMainMethodArray := MethodSignature.Split(' ', TStringSplitOptions.ExcludeEmpty);
  SetLength(PascalMainTempMapArray, 2);
  PascalMainTempMapArray[0] := PascalMainMethodArray[0];
  PascalMainTempMapArray[1] := PascalMainMethodArray[1] + ' ' + PascalMainMethodArray[2];
  Index := 0;
  HandleRegularMethod(PascalMainTempMapArray, Index, ResultArray);
  SetLength(PascalMainTempMapArray, 0);
end;

procedure TMapFileInfo.HandleRegularMethod(AMapFileData: TStringDynArray;
  var Index: integer; var ResultArray: TStringDynArray);
begin
  SetLength(ResultArray, Length(ResultArray) + 1);
  ResultArray[Length(ResultArray) - 1] :=
    AMapFileData[Index].trim().Substring(CODE_ITEM_START_MARKER_LENGTH);
  Inc(Index);
  SetLength(ResultArray, Length(ResultArray) + 1);
  ResultArray[Length(ResultArray) - 1] := AMapFileData[Index].trim();
end;

function TMapFileInfo.FilterMapFileDataString(AMapFileData: TStringDynArray;
  MethodSignatureFilter: string): TStringDynArray;
var
  i: integer;
  MethodSignatureFilters: TStringArray;
  TestingSignature: string;
begin
  MethodSignatureFilter := MethodSignatureFilter + ',' + BASIC_METHOD_SIGNATURE_FILTER;
  MethodSignatureFilters := MethodSignatureFilter.Split(',');
  i := 0;
  while i <= Length(AMapFileData) - 1 do
  begin
    TestingSignature := AMapFileData[i].trim();
    if IsAvailableMethodSignature(TestingSignature, MethodSignatureFilters) then
    begin
      HandleMethodSignature(AMapFileData, i, Result);
    end;
    Inc(i);
  end;
  SetLength(MethodSignatureFilters, 0);
end;

function TMapFileInfo.IsAvailableMethodSignature(TestingMethodSignature: string;
  MethodSignatureFilters: TStringArray): boolean;
var
  i: integer;
begin
  Result := False;
  if not TestingMethodSignature.StartsWith(CODE_ITEM_START_MARKER) then
    exit;
  for i := 0 to Length(MethodSignatureFilters) - 1 do
  begin
    if TestingMethodSignature.Contains(MethodSignatureFilters[i]) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TMapFileInfo.IsPascalMain(TestingSignature: string): boolean;
begin
  Result := TestingSignature.StartsWith(N_MAIN_METHOD_SIGNATURE_FILTER);
end;

function TMapFileInfo.FindCallerInfoByAddress(CallerPointer: CodePointer): TCallableInfo;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FCallableInfoList.Count - 1 do
  begin
    if TCallableInfo(FCallableInfoList.Items[i]).IsMethodGammaAddress(
      CallerPointer) then
    begin
      Result := TCallableInfo(FCallableInfoList.Items[i]);
      break;
    end;
  end;
end;

class function TMapFileInfo.CreateMapFileInfo: TObject;
begin
  Result := TMapFileInfo.Create;
end;

{ TCallableInfoCollectionEnumerator }

function TCallableInfoCollectionEnumerator.GetCurrent: TCallableInfo;
begin
  Result := inherited GetCurrent as TCallableInfo;
end;

{ TCallableInfoCollection }

constructor TCallableInfoCollection.Create;
begin
  inherited Create(TCallableInfo);
end;

function TCallableInfoCollection.Add: TCallableInfo;
begin
  Result := inherited Add as TCallableInfo;
end;

procedure TCallableInfoCollection.SetItem(Index: integer; AValue: TCallableInfo);
begin
  inherited SetItem(Index, AValue);
end;

function TCallableInfoCollection.GetItem(Index: integer): TCallableInfo;
begin
  Result := inherited GetItem(Index) as TCallableInfo;
end;

function TCallableInfoCollection.GetEnumerator: TCallableInfoCollectionEnumerator;
begin
  Result := inherited GetEnumerator as TCallableInfoCollectionEnumerator;
end;

class function TCallableInfoCollection.CreateCallableInfoCollection: TCollection;
begin
  Result := TCallableInfoCollection.Create;
end;

{ TCallableInfoParser }

constructor TCallableInfoParser.Create(var CallableInfoList: TCallableInfoCollection);
begin
  FCallableInfoList := CallableInfoList;
  FRegExp := TRegExpr.Create(BASICALLY_METHOD_SIGNATURE_PATTERN);
  //FRegExp.ModifierG := True;
  //FRegExp.ModifierM := True;
  FRegExp.EmptyInputRaisesError := True;
  FRegExp.Compile;
end;

destructor TCallableInfoParser.Destroy;
begin
  FreeAndNil(FRegExp);
  inherited Destroy;
end;

procedure TCallableInfoParser.FillAddressInfo(CallableInfo: TCallableInfo;
  AddressInfoString: string);
var
  AddressInfoStringParts: TStringArray;
begin
  AddressInfoStringParts := AddressInfoString.Split(' ',
    TStringSplitOptions.ExcludeEmpty);
  CallableInfo.StartCallableAddress :=
    Hex2Dec64(AddressInfoStringParts[0].trim().Substring(2));
  CallableInfo.EndCallableAddress :=
    CallableInfo.StartCallableAddress + Hex2Dec64(
    AddressInfoStringParts[1].trim().Substring(2));
end;

procedure TCallableInfoParser.FillNamingInfo(CallableInfo: TCallableInfo;
  MethodSignatureString: string);
var
  parameters: TStringArray;
  i: integer;
begin
  writeln(format('parsing signature''%s''', [MethodSignatureString]));
  FRegExp.Exec(MethodSignatureString);
  repeat
    for i := 0 to FRegExp.SubExprMatchCount do
    begin
      writeln(format('signature part %d is ''%s''', [i, FRegExp.Match[i]]));
    end;
  until FRegExp.ExecNext();
  CallableInfo.UnitName := FRegExp.Match[2];
  CallableInfo.ClassName := FRegExp.Match[5];
  CallableInfo.AdditionalCallableInfo := FRegExp.Match[3];
  CallableInfo.MethodName := FRegExp.Match[4];
  parameters := string(FRegExp.Match[5]).Split(['$'], TStringSplitOptions.ExcludeEmpty);
  for i := 0 to Length(parameters) - 1 do
    CallableInfo.Parameters.Add(parameters[i]);
  //  SetLength(parameters, 0);
  CallableInfo.ReturnType := FRegExp.Match[6];
  writeln(format('signature''%s'' parsed', [MethodSignatureString]));
end;

procedure TCallableInfoParser.ParseCallableInfoFromStrings(
  MethodSignatureString: string;
  AddressInfoString: string);
var
  CallableInfo: TCallableInfo;
begin
  CallableInfo := FCallableInfoList.Add;
  FillNamingInfo(CallableInfo, MethodSignatureString);
  FillAddressInfo(CallableInfo, AddressInfoString);
end;

end.
