{Basic types for MapFileBasedLogger}
unit MapFileBasedLogger.BasicTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  regexpr;

const
  CODE_SECTION_START_MARKER = '.text';
  {Linker map file code section end marker(data section started with string contain it).}
  CODE_SECTION_END_MARKER = '.data';
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
const
  METHOD_NAME_INFO_PRINTABLE_FORMAT = '%s %s(%s)%s';
  FUNCTION_MARKER = 'FUNC';
  PROCEDURE_MARKER = 'PROC';

type
  { TMethodNameInfo }

  TMethodNameInfo = class(TCollectionItem)
  strict private
    FName: string;
    FParameters: TStringList;
    FReturnType: string;
  private
    class procedure CreateMethodNameInfo(ATarget: TMethodNameInfo;
      AName: string; AParameterString: string; AReturnType: string);
  public
    constructor Create(ACollection: TCollection); override;
    constructor Create;
    destructor Destroy; override;
    function ToString: ansistring; override;
  published
    {Method names list. Can't be empty}
    property Name: string read FName write FName;
    {Parameter type names list. Can be empty}
    property Parameters: TStringList read FParameters write FParameters;
    {Return type name. Can be ''}
    property ReturnType: string read FReturnType write FReturnType;
  end;


  { TMethodInfoCollectionEnumerator }

  TMethodInfoCollectionEnumerator = class(TCollectionEnumerator)
  strict private
    function GetCurrent: TMethodNameInfo;
  public
    property Current: TMethodNameInfo read GetCurrent;
  end;


  TMethodInfoCollection = class(TCollection)
  private
    procedure SetItem(Index: integer; AValue: TMethodNameInfo);
    function GetItem(Index: integer): TMethodNameInfo;
  public
    constructor Create;
    function Add: TMethodNameInfo;
    property Items[Index: integer]: TMethodNameInfo read GetItem write SetItem;
    function GetEnumerator: TMethodInfoCollectionEnumerator;
    function ToString: ansistring; override;
  end;

  { TCallableAddressInfo }

  TCallableAddressInfo = class(TPersistent)
  strict private
    FStartCallableAddress: SizeInt;
    FEndCallableAddress: SizeInt;
  private
    constructor Create(AddressInfoString: string);
  public
    constructor Create;
    destructor Destroy; override;
    function IsMethodGammaAddress(Address: Pointer): boolean;
  published
    {Range start address, inclusive.}
    property StartCallableAddress: SizeInt read FStartCallableAddress
      write FStartCallableAddress;
    {Range end address, inclusive.}
    property EndCallableAddress: SizeInt read FEndCallableAddress
      write FEndCallableAddress;
  end;

const
  CALLABLE_NAME_PRINTABLE_FORMAT = '%s::%s<(%s)>:%s';

type
  { TCallableNameInfo }

  TCallableNameInfo = class(TPersistent)
  strict private
    FRegExp: TRegExpr;
    FUnitName: string;
    FClassName: string;
    FTargetMethodInfo: TMethodNameInfo;
    FWrappingMethods: TMethodInfoCollection;
    function IsCurrentMethodPascalMain(): boolean;
    procedure CreatePascalMainMethodInfo;
    procedure CreateRegularMethodInfo;
  private
    constructor Create(MethodSignatureString: string);
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: ansistring; override;
  published
    {Unit name. Can't be ''}
    property UnitName: string read FUnitName write FUnitName;
    {Class name. Can be ''}
    property ClassName: string read FClassName write FClassName;
    property TargetMethodInfo: TMethodNameInfo
      read FTargetMethodInfo write FTargetMethodInfo;
    property WrappingMethods: TMethodInfoCollection read FWrappingMethods;
  end;

  { TCallableInfo }

  TCallableInfo = class(TCollectionItem)
  strict private
    FCallableNameInfo: TCallableNameInfo;
    FCallableAddressInfo: TCallableAddressInfo;
  public
    constructor Create(ACollection: TCollection); override;
    constructor Create(ACollection: TCollection; ACallableNameInfo: TCallableNameInfo;
      ACallableAddressInfo: TCallableAddressInfo);
    {Check Address in callable address  range.
    @param Address caller address
    @returns @true if Address in callable address  range}
    function IsMethodGammaAddress(Address: Pointer): boolean;
    {String containing: TCallableInfo.UnitName +LOG_PART_SEPARATOR+ TCallableInfo.ClassName +LOG_PART_SEPARATOR+ TCallableInfo.MethodNames}
    function ToString: ansistring; override;
    {Cleaning state.}
    destructor Destroy; override;
  published
    property CallableAddressInfo: TCallableAddressInfo
      read FCallableAddressInfo write FCallableAddressInfo;
    property CallableNameInfo: TCallableNameInfo
      read FCallableNameInfo write FCallableNameInfo;
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
  PASCAL_MAIN_METHOD_SIGNATURE = 'n_main';
  PASCAL_MAIN_METHOD_NAME = 'PASCALMAIN';
  PASCAL_PROGRAM_NAMESPACE = 'n_p$';
  BASIC_METHOD_SIGNATURE_FILTER =
    PASCAL_PROGRAM_NAMESPACE + ',' + PASCAL_MAIN_METHOD_SIGNATURE;
  N_MAIN_METHOD_SIGNATURE_FILTER = CODE_ITEM_START_MARKER + PASCAL_MAIN_METHOD_SIGNATURE;

const
  //----------------------REGEXP---------------------------------------------//
  BASICALLY_METHOD_SIGNATURE_PATTERN =
    '^(?:n\_)(?:(main)|((?:p\$)*(?:[a-z][a-z0-9\_\.]+))(?:\$)*(?:\_\$)(?:([a-z][a-z0-9\.]++)(?:\_\$)(?:(?:\_)((?:[a-z][a-z0-9\.]*+)?)((?:\$[a-z0-9\.]++)*)((?:\$\$[a-z0-9\.]*+)?)(?:\_\$)*)*(?:(?:\_)((?:[a-z][a-z0-9\.]*+)?)((?:\$[a-z0-9\.]++)*)((?:\$\$[a-z0-9\.]*+)?)(?:\_\$))?)*(?:\$\_)(?:((?:[a-z][a-z0-9\.]*+)?|init_implicit|finalize_implicit)((?:\$[a-z0-9\.]*+)*)((?:\$\$[a-z0-9\.]*+)?)))$';
  //--------------------------  MATCH INDEXES---------------------------------//
  PASCAL_MAIN_INDEX = 1;
  NAMESPACE_MATCH_INDEX = 2;
  CLASS_NAME_MATCH_INDEX = 3;
  CALLABLE_METHOD_INFO_GROUP_COUNT = 3;
  MAXIMUM_MATCH_INDEX = 12;
//----------------------END REGEXP-----------------------------------------//s
implementation

uses StrUtils;

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
TCallableInfoParser }

  TCallableInfoParser = class
  strict private
    FCallableInfoList: TCallableInfoCollection;
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

{ TMethodNameInfo }

constructor TMethodNameInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Create;
end;

constructor TMethodNameInfo.Create;
begin
  FName := '';
  FParameters := TStringList.Create;
  FReturnType := '';
end;

destructor TMethodNameInfo.Destroy;
begin
  FReturnType := '';
  FreeAndNil(FParameters);
  FName := '';
  inherited Destroy;
end;

function TMethodNameInfo.ToString: ansistring;
var
  parameterList: string;
  i: integer;
  methodType: string;
begin
  Result := '';
  if FName = EmptyStr then
    exit;
  methodType := specialize IfThen<string>(FReturnType <> EmptyStr,
    FUNCTION_MARKER, PROCEDURE_MARKER);
  parameterList := '';
  for i := 0 to FParameters.Count - 1 do
    parameterList := parameterList + FParameters[i] + ',';
  if Length(parameterList) > 1 then
    parameterList := parameterList.Substring(0, Length(parameterList) - 1);
  Result := Format(METHOD_NAME_INFO_PRINTABLE_FORMAT,
    [methodType, FName, parameterList, FReturnType]);
end;

class procedure TMethodNameInfo.CreateMethodNameInfo(ATarget: TMethodNameInfo;
  AName: string; AParameterString: string; AReturnType: string);
var
  tempArray: TStringArray;
  i: integer;
begin
  ATarget.Name := AName;
  tempArray := AParameterString.Split(['$'], TStringSplitOptions.ExcludeEmpty);
  for i := 0 to Length(tempArray) - 1 do
    ATarget.FParameters.Add(tempArray[i].trim());
  SetLength(tempArray, 0);
  tempArray := AReturnType.Split(['$$'], TStringSplitOptions.ExcludeEmpty);
  if Length(tempArray) = 1 then
    ATarget.ReturnType := tempArray[0].trim();
end;

{ TMethodInfoCollectionEnumerator }

function TMethodInfoCollectionEnumerator.GetCurrent: TMethodNameInfo;
begin
  Result := inherited GetCurrent as TMethodNameInfo;
end;

procedure TMethodInfoCollection.SetItem(Index: integer; AValue: TMethodNameInfo);
begin
  inherited setItem(Index, AValue);
end;

function TMethodInfoCollection.GetItem(Index: integer): TMethodNameInfo;
begin
  Result := inherited GetItem(Index) as TMethodNameInfo;
end;

constructor TMethodInfoCollection.Create;
begin
  inherited Create(TMethodNameInfo);
end;

function TMethodInfoCollection.Add: TMethodNameInfo;
begin
  Result := inherited Add as TMethodNameInfo;
end;

function TMethodInfoCollection.GetEnumerator: TMethodInfoCollectionEnumerator;
begin
  Result := inherited GetEnumerator as TMethodInfoCollectionEnumerator;
end;

function TMethodInfoCollection.ToString: ansistring;
var
  i: integer;
begin
  Result := '';
  for i := Count - 1 downto 0 do
    Result := Result + Items[i].ToString + '>';
end;

{ TCallableNameInfo }
constructor TCallableNameInfo.Create;
begin
  FUnitName := '';
  FClassName := '';
  FTargetMethodInfo := TMethodNameInfo.Create;
  FWrappingMethods := TMethodInfoCollection.Create;
  FRegExp := TRegExpr.Create(BASICALLY_METHOD_SIGNATURE_PATTERN);
  FRegExp.Compile;
end;

constructor TCallableNameInfo.Create(MethodSignatureString: string);
begin
  Create;
  writeln(format('parsing signature''%s''', [MethodSignatureString]));
  if not FRegExp.Exec(MethodSignatureString) then
    raise EMapInfoCreateException.CreateFmt('Can''t read signature ''%s''',
      [MethodSignatureString]);
  if (IsCurrentMethodPascalMain()) then
  begin
    CreatePascalMainMethodInfo();
    exit;
  end;
  CreateRegularMethodInfo();

  writeln(format('signature''%s'' parsed', [MethodSignatureString]));
end;

destructor TCallableNameInfo.Destroy;
begin
  FreeAndNil(FRegExp);
  FreeAndNil(FWrappingMethods);
  FreeAndNil(FTargetMethodInfo);
  FClassName := '';
  FUnitName := '';
  inherited Destroy;
end;

procedure TCallableNameInfo.CreatePascalMainMethodInfo();
begin
  TMethodNameInfo.CreateMethodNameInfo(FTargetMethodInfo,
    PASCAL_MAIN_METHOD_NAME,
    EmptyStr,
    EmptyStr);
end;

procedure TCallableNameInfo.CreateRegularMethodInfo();
var
  i: integer;
  isTarget: boolean;
begin
  FUnitName := FRegExp.Match[NAMESPACE_MATCH_INDEX];
  FClassName := FRegExp.Match[CLASS_NAME_MATCH_INDEX];
  i := MAXIMUM_MATCH_INDEX;
  isTarget := False;
  while i > CLASS_NAME_MATCH_INDEX do
  begin
    if trim(FRegExp.Match[i - 2]) = EmptyStr then
    begin
      i := i - CALLABLE_METHOD_INFO_GROUP_COUNT;
      continue;
    end;
    if not isTarget then
    begin
      isTarget := True;
      TMethodNameInfo.CreateMethodNameInfo(FTargetMethodInfo,
        FRegExp.Match[i - 2],
        FRegExp.Match[i - 1],
        FRegExp.Match[i]);
    end
    else
    begin
      TMethodNameInfo.CreateMethodNameInfo(FWrappingMethods.Add,
        FRegExp.Match[i - 2],
        FRegExp.Match[i - 1],
        FRegExp.Match[i]);
    end;
    i := i - CALLABLE_METHOD_INFO_GROUP_COUNT;
  end;
end;

function TCallableNameInfo.IsCurrentMethodPascalMain: boolean;
begin
  Result := trim(FRegExp.Match[PASCAL_MAIN_INDEX]) <> EmptyStr;

end;

function TCallableNameInfo.ToString: ansistring;
begin
  Result := format(CALLABLE_NAME_PRINTABLE_FORMAT,
    [FUnitName, FClassName, FWrappingMethods.toString, FTargetMethodInfo.toString]);
end;

{ TCallableAddressInfo }

constructor TCallableAddressInfo.Create;
begin
  FStartCallableAddress := 0;
  FEndCallableAddress := 0;
end;

constructor TCallableAddressInfo.Create(AddressInfoString: string);
var
  AddressInfoStringParts: TStringArray;
begin
  AddressInfoStringParts := AddressInfoString.Split(' ',
    TStringSplitOptions.ExcludeEmpty);
  FStartCallableAddress :=
    Hex2Dec64(AddressInfoStringParts[0].trim().Substring(2));
  FEndCallableAddress :=
    FStartCallableAddress + Hex2Dec64(AddressInfoStringParts[1].trim().Substring(2));
end;

destructor TCallableAddressInfo.Destroy;
begin
  FEndCallableAddress := 0;
  FStartCallableAddress := 0;
  inherited Destroy;
end;

function TCallableAddressInfo.IsMethodGammaAddress(Address: Pointer): boolean;
var
  AddressUint: SizeInt;
begin
  AddressUint := CodePtrUInt(Address);
  Result := (AddressUint >= FStartCallableAddress) and
    (AddressUint <= FEndCallableAddress);
end;

{ TCallableInfo }

constructor TCallableInfo.Create(ACollection: TCollection);
begin
  Create(ACollection, TCallableNameInfo.Create, TCallableAddressInfo.Create);
end;

constructor TCallableInfo.Create(ACollection: TCollection;
  ACallableNameInfo: TCallableNameInfo; ACallableAddressInfo: TCallableAddressInfo);
begin
  inherited Create(ACollection);
  FCallableAddressInfo := ACallableAddressInfo;
  FCallableNameInfo := ACallableNameInfo;
end;

destructor TCallableInfo.Destroy;
begin
  FreeAndNil(FCallableNameInfo);
  FreeAndNil(FCallableAddressInfo);
  inherited Destroy;
end;

function TCallableInfo.IsMethodGammaAddress(Address: Pointer): boolean;
begin
  Result := FCallableAddressInfo.IsMethodGammaAddress(Address);
end;

function TCallableInfo.ToString: ansistring;
begin
  Result := FCallableNameInfo.ToString;
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

{ TMapFileInfo }

constructor TMapFileInfo.Create(AMapFileData: TStringDynArray;
  MethodSignatureFilter: string);
var
  FilteredMapFileData: TStringDynArray;
begin
  FilteredMapFileData := FilterMapFileDataString(AMapFileData, MethodSignatureFilter);
  Create;
  ParsePreparedMapFileData(FilteredMapFileData);
end;

constructor TMapFileInfo.Create();
begin
  inherited Create;
  FCallableInfoList := TCallableInfoCollection.Create();
end;

destructor TMapFileInfo.Destroy;
begin
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
  Count := Length(AMapFileData);
  CallableInfoParser := TCallableInfoParser.Create(FCallableInfoList);
  while i < Count do
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
  CallableInfo: TCallableInfo;
  i: integer;
begin
  Result := nil;
  for i := 0 to FCallableInfoList.Count - 1 do
  begin
    CallableInfo := FCallableInfoList.Items[i];
    if CallableInfo.IsMethodGammaAddress(CallerPointer) then
    begin
      Result := CallableInfo;
      break;
    end;
  end;
end;

class function TMapFileInfo.CreateMapFileInfo: TObject;
begin
  Result := TMapFileInfo.Create;
end;

{ TCallableInfoParser }

constructor TCallableInfoParser.Create(var CallableInfoList: TCallableInfoCollection);
begin
  FCallableInfoList := CallableInfoList;
end;

destructor TCallableInfoParser.Destroy;
begin
  inherited Destroy;
end;

procedure TCallableInfoParser.ParseCallableInfoFromStrings(
  MethodSignatureString: string;
  AddressInfoString: string);
begin
  TCallableInfo.Create(FCallableInfoList,
    TCallableNameInfo.Create(MethodSignatureString),
    TCallableAddressInfo.Create(AddressInfoString));
end;

end.
