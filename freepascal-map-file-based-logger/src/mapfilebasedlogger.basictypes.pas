{Basic types for MapFileBasedLogger}
unit MapFileBasedLogger.BasicTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types;

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
  TCallableInfo = class;

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
    FCallableInfoList: TCollection;
  private
    {fill data for new item
    @param MethodHeader <mapFileHeaderString>
    @param AddressInfo <addressInfoString>
    }
    procedure FillCallableInfo(HeaderData: TStringArray;
      AddressInfoData: TStringArray);
    {Address presented in hex format leading 0x, remove this prefix.
    @param Address in pattern 0x12344567890ABCDEF
    @return address in pattern 12344567890ABCDEF
    }
    function RemoveXFromHexString(Address: string): string;
    {
    Can parsed <addressInfoString>.
    @param Length string for parsing.
    @returns @true if Length passed <addressInfoString> BNF,else - program entry point record.
    }
    function IsAvailableLength(Length: string): boolean;
    {
     Parse unitName from <mapFileHeaderString>. Remove unit name from FullMethodName.
     @param FullMethodName <mapFileHeaderString>, in progress.
     @return value for filling TCallableInfo.UnitName property.
    }
    function GetUnitName(var FullMethodName: string): string;
    {
     Parse className from <mapFileHeaderString>. Remove class name from FullMethodName.
     @param FullMethodName <mapFileHeaderString>, in progress.
     @return value for filling TCallableInfo.ClassName property.
    }
    function GetClassName(var FullMethodName: string): string;
    {
     Parse methodNames from <mapFileHeaderString>. Remove class name from FullMethodName.
     @param FullMethodName <mapFileHeaderString>, in progress.
     @return value for filling TCallableInfo.MethodNames property.
    }
    function GetMethodNamesList(var FullMethodName: string): TStringDynArray;
    {
     Extract part from FullMethodName bounded HeadDelimiter and TailDelimiter.
     Remove <HeadDelimiter><partData><TailDelimiter> from FullMethodName
     @param FullMethodName <HeadDelimiter><partData><TailDelimiter><otherMapFileHeaderString>,
     in progress
     @param HeadDelimiter leading separator for part
     @param TailDelimiter tailing separator for part
     @returns <partData> or '' if part empty.
    }
    function ExtractPartName(var FullMethodName: string;
      const HeadDelimiter: string; const TailDelimiter: string): string;
    {If PartName ends with METHOD_DEFINITION_SEPARATOR return it to FullMethodName and remove from PartName
    @param PartName checking part name.
    @param <mapFileHeaderString>, in progress}
    procedure ReturnMethodSeparator(var PartName: string; var FullMethodName: string);
    {Fill address info for regular methods, @bold(not entry point!).
    @param CallableInfo filling callable info.
    @param AddressInfoData <addressInfoString>.
    }
    procedure FillAddressInfoFromAddressData(CallableInfo: TCallableInfo;
      AddressInfoData: TStringArray);
    {Fill address info for @bold(application entry point!).
    @param CallableInfo filling callable info.
    @param AddressInfoData <addressInfoString>.
    }
    procedure FillAddressInfoFromHeaderData(CallableInfo: TCallableInfo;
      HeaderData: TStringArray);
    procedure CorrectUnitSepartor(var FullMethodName: string);
  public
    {Creates parser instance with specified CallableInfoList.
    @param CallableInfoList TMapFileInfo.CallableInfoList
    }
    constructor Create(var CallableInfoList: TCollection);
    {Add new item in callable collection
    @param MethodHeader <mapFileHeaderString>
    @param AddressInfo <addressInfoString>}
    procedure ParseCallableInfoFromStrings(MethodHeader: string;
      AddressInfo: string);
  end;

  {
    Persistence Map file info containing parsed callable info.
    Must be serialized into file and distributed with application.@br
     @author Artem A. Bogomolov(artem.bogomolov.a@gmail.com)
  }

  { TMapFileInfo }

  TMapFileInfo = class(TPersistent)
  strict  private
    FCallableInfoList: TCollection;
  private
    procedure ParsePreparedMapFileData(AMapFileData: TStringDynArray);
    {Exclude from AMapFileData strings with (@link TRY_RES_STRING_MODULE_NAME) and (@link TRY_TC_MODULE_NAME)}
    function FilterMapFileDataString(AMapFileData: TStringDynArray): TStringDynArray;
  public
    {Creates instance from AMapFileData lines.
    @param AMapFileData lines from map file in '.text' section(only code).}
    constructor Create(AMapFileData: TStringDynArray);
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
    property CallableInfoList: TCollection read FCallableInfoList;
  end;

  {Parsed callable info.}
  TCallableInfo = class(TCollectionItem)
  strict private
    FUnitName: string;
    FClassName: string;
    FMethodNames: TStrings;
    FParameters: TStrings;
    FReturnType: string;
    FStartCallableAddress: int64;
    FEndCallableAddress: int64;
  public
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
    property MethodNames: TStrings read FMethodNames write FMethodNames;
    {Parameter type names list. Can be empty}
    property Parameters: TStrings read FParameters write FParameters;
    {Return type name. Can be ''}
    property ReturnType: string read FReturnType write FReturnType;
    {Range start address, inclusive.}
    property StartCallableAddress: int64 read FStartCallableAddress
      write FStartCallableAddress;
    {Range end address, inclusive.}
    property EndCallableAddress: int64 read FEndCallableAddress
      write FEndCallableAddress;
  end;

const
  {callable info part separator }
  LOG_PART_SEPARATOR = ':';
  {procedure marker}
  PROC_TYPE = 'PROC';
  {function marker}
  FUNC_TYPE = 'FUNC';
  {code section record marker}
  CODE_SECTION_MARKER = '.text.';
  {pascal application entry point }
  PASCALMAIN_PROC_NAME = 'PASCALMAIN';
  {main proc name}
  MAIN_PROC_NAME = 'main';
  {Unit name prefix.}
  UNIT_NAME_SEPARATOR = 'n_';
  {Linker map file code section start marker.}
  MAP_FILE_MEMORY_MAP_MARKER = 'Memory map (ImageBase=0x0000000100000000)';
  {Linker map file code section end marker(data section started with string contain it).}
  DATA_SECTION_MARKER = '.data';
  {Unit name separator.}
  UNIT_NAME_TAIL_SEPARATOR = '$';
  {main application module prefix.}
  UNIT_PROGRAM_IDENTIFIER = 'n_p';
  {Class name separator.}
  CLASS_NAME_SEPARATOR = '_$';
  {Method definition separator.}
  METHOD_DEFINITION_SEPARATOR = '_';
  {Parameter type name separator.}
  PARAMETER_TYPE_NAME_SEPARATOR = '$';
  {Return type name separator.}
  RETURN_TYPE_NAME_SEPARATOR = '$$';
  {Target method(terminal method)}
  TARGET_METHOD_DEFINITION_SEPARATOR = '_$$_';
  {May be resource string module name}
  TRY_RES_STRING_MODULE_NAME = 'n_RESSTR_';
  {Unknown module name}
  TRY_TC_MODULE_NAME = 'n_TC_';

//----------------------REGEXP---------------------------------------------//
const
  BASICALLY_METHOD_SIGNATURE_PATTERN = '(n\_([A-z0-9\.]*))'+//namespace, or module name
  '((\$)([A-z0-9\.]*))*'+//module name in namespace
  '(\$)*'//optionally namespace or module name separator
  ;
//----------------------END REGEXP-----------------------------------------//
implementation

uses StrUtils,Regex;

{ TCallableInfoParser }

procedure TCallableInfoParser.FillCallableInfo(HeaderData: TStringArray;
  AddressInfoData: TStringArray);
var
  Length: string;
  FullMethodName: string;
  CallableInfo: TCallableInfo;
begin
  CallableInfo := TCallableInfo(FCallableInfoList.Add);
  Length := AddressInfoData[1];
  FullMethodName := HeaderData[0].Substring(CODE_SECTION_MARKER.Length);
  CallableInfo.UnitName := GetUnitName(FullMethodName);
  CallableInfo.ClassName := {GetClassName(}FullMethodName{)};
  CallableInfo.MethodNames := TStringList.Create;
  CallableInfo.MethodNames.Add('');
  CallableInfo.Parameters := TStringList.Create;
  CallableInfo.Parameters.Add('');
  if IsAvailableLength(Length) then
  begin
    FillAddressInfoFromAddressData(CallableInfo, AddressInfoData);
    exit;
  end;
  FillAddressInfoFromHeaderData(CallableInfo, HeaderData);
end;

function TCallableInfoParser.RemoveXFromHexString(Address: string): string;
begin
  Result := Address.Substring(2);
end;

function TCallableInfoParser.IsAvailableLength(Length: string): boolean;
begin
  Result := (Length <> PASCALMAIN_PROC_NAME) and (Length <> MAIN_PROC_NAME);
end;

function TCallableInfoParser.GetUnitName(var FullMethodName: string): string;
begin
  Result := FullMethodName.Substring(0, FullMethodName.IndexOf(
    UNIT_NAME_TAIL_SEPARATOR));
  if (Result = UNIT_PROGRAM_IDENTIFIER) then
  begin
    FullMethodName := FullMethodName.Substring(Length(UNIT_PROGRAM_IDENTIFIER));
    Result := ExtractPartName(FullMethodName, UNIT_NAME_TAIL_SEPARATOR,
      UNIT_NAME_TAIL_SEPARATOR);
  end
  else
  begin
    Result := Result.Substring(length(UNIT_NAME_SEPARATOR));
  end;
  FullMethodName := FullMethodName.substring(length(UNIT_NAME_SEPARATOR) +
    length(Result));
  ReturnMethodSeparator(Result, FullMethodName);
  CorrectUnitSepartor(FullMethodName);
end;

function TCallableInfoParser.GetClassName(var FullMethodName: string): string;
begin
  Result := FullMethodName;{ExtractPartName(FullMethodName, CLASS_NAME_SEPARATOR,
    CLASS_NAME_SEPARATOR);
  if (Result = '') then
    writeln(FullMethodName);
  returnMethodSeparator(Result, FullMethodName);}
end;

function TCallableInfoParser.GetMethodNamesList(var FullMethodName: string):
TStringDynArray;
begin
  SetLength(Result, 1);
  Result[0] := {ExtractPartName(FullMethodName, METHOD_DEFINITION_SEPARATOR,
    TARGET_METHOD_DEFINITION_SEPARATOR)}FullMethodName;

end;

function TCallableInfoParser.ExtractPartName(var FullMethodName: string;
  const HeadDelimiter: string; const TailDelimiter: string): string;
var
  partNameFrontIndex, partNameLastIndex: integer;
  partNameFrontIndexWithCarry, partNameLastIndexWithoutCarry: integer;
  headCarry, tailCarry: integer;
begin
  Result := '';
  headCarry := length(HeadDelimiter);
  tailCarry := length(TailDelimiter);
  partNameFrontIndex := FullMethodName.IndexOf(HeadDelimiter);
  partNameFrontIndexWithCarry := partNameFrontIndex + headCarry;
  partNameLastIndex := FullMethodName.IndexOf(TailDelimiter,
    partNameFrontIndexWithCarry);
  partNameLastIndexWithoutCarry := partNameLastIndex - tailCarry;
  Result := FullMethodName.Substring(partNameFrontIndexWithCarry,
    partNameLastIndexWithoutCarry);
  FullMethodName := FullMethodName.Substring(partNameLastIndexWithoutCarry - headCarry);
end;

procedure TCallableInfoParser.ReturnMethodSeparator(var PartName: string;
  var FullMethodName: string);
begin
  if (PartName.EndsWith(METHOD_DEFINITION_SEPARATOR)) then
  begin
    PartName := PartName.Substring(0, length(PartName) - 1);
    FullMethodName := METHOD_DEFINITION_SEPARATOR + FullMethodName;
  end;
end;

procedure TCallableInfoParser.FillAddressInfoFromAddressData(
  CallableInfo: TCallableInfo; AddressInfoData: TStringArray);
begin
  CallableInfo.StartCallableAddress :=
    Hex2Dec64(RemoveXFromHexString(AddressInfoData[0]));
  CallableInfo.EndCallableAddress :=
    CallableInfo.StartCallableAddress +
    Hex2Dec64(RemoveXFromHexString(AddressInfoData[1]));

end;

procedure TCallableInfoParser.FillAddressInfoFromHeaderData(
  CallableInfo: TCallableInfo;
  HeaderData: TStringArray);
begin
  CallableInfo.StartCallableAddress := Hex2Dec64(RemoveXFromHexString(HeaderData[1]));
  CallableInfo.EndCallableAddress :=
    CallableInfo.StartCallableAddress + Hex2Dec64(RemoveXFromHexString(HeaderData[2]));
end;

procedure TCallableInfoParser.CorrectUnitSepartor(var FullMethodName: string);
begin
  if FullMethodName.StartsWith(UNIT_NAME_TAIL_SEPARATOR) then
    FullMethodName := FullMethodName.Substring(1);
end;

constructor TCallableInfoParser.Create(var CallableInfoList: TCollection);
begin
  FCallableInfoList := CallableInfoList;
end;

procedure TCallableInfoParser.ParseCallableInfoFromStrings(MethodHeader: string;
  AddressInfo: string);
var
  HeaderData: TStringArray;
  AddressInfoData: TStringArray;
begin
  HeaderData := MethodHeader.Split(' ', TStringSplitOptions.ExcludeEmpty);
  AddressInfoData := AddressInfo.Split(' ', TStringSplitOptions.ExcludeEmpty);
  FillCallableInfo(HeaderData, AddressInfoData);
end;

{ TCallableInfo }

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
  if FReturnType = '' then
    Result := Result + LOG_PART_SEPARATOR + PROC_TYPE
  else
    Result := Result + LOG_PART_SEPARATOR + FUNC_TYPE;
end;

destructor TCallableInfo.Destroy;
begin
  FreeAndNil(FMethodNames);
  FreeAndNil(FParameters);
  inherited Destroy;
end;

{ TMapFileInfo }

constructor TMapFileInfo.Create(AMapFileData: TStringDynArray);
var
  FilteredMapFileData: TStringDynArray;
begin
  Create;
  FilteredMapFileData :={FilterMapFileDataString(}AMapFileData{)};
  ParsePreparedMapFileData(FilteredMapFileData);
end;

constructor TMapFileInfo.Create;
begin
  inherited Create;
  FCallableInfoList := TCollection.Create(TCallableInfo);
end;

destructor TMapFileInfo.Destroy;
begin
  FCallableInfoList.Clear;
  FreeAndNil(FCallableInfoList);
  inherited Destroy;
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

function TMapFileInfo.FilterMapFileDataString(AMapFileData: TStringDynArray):
TStringDynArray;
begin

end;

procedure TMapFileInfo.ParsePreparedMapFileData(AMapFileData: TStringDynArray);
var
  i, Count: integer;
  MapFileString: string;
  AddressInfo: string;
  CallableInfoParser: TCallableInfoParser;
begin
  i := 0;
  Count := High(AMapFileData);
  CallableInfoParser := TCallableInfoParser.Create(FCallableInfoList);
  while i < Count - 1 do
  begin
    mapFileString := AMapFileData[i];
    if not mapFileString.StartsWith(CODE_SECTION_MARKER) then
    begin
      Inc(i);
      Continue;
    end;
    Inc(i);
    AddressInfo := AMapFileData[i];
    CallableInfoParser.ParseCallableInfoFromStrings(mapFileString, AddressInfo);
  end;
  FreeAndNil(CallableInfoParser);
end;

end.
