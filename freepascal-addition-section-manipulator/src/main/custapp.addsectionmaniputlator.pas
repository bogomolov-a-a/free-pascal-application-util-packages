unit CustApp.AddSectionManiputlator;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type

  { IAdditionalSectionManipulator }

  IAdditionalSectionManipulator = interface
    {Return initialized(may be empty) list of section names who has been written
    into binary file.}
    function GetSectionNames(): TStringList;
    {
    Return true, if section with specified name exist in the binary file}
    function ExistsSectionByName(Name: string): boolean;
    {
     Prepare section data to write(add descriptor).
     Can't rewrite section, only add new by name.
     @param Name section name
     @param Data section data as UTF-8 encoding string.
     @return true - if section data successful added.
    }
    function WriteDataSection(Name: string; Data: string): boolean; overload;
    {
     Prepare section data to write(add descriptor).
     Can't rewrite section, only add new by name.
     @param Name section name
     @param Data section data as raw byte array.
     @return true - if section data successful added.
    }
    function WriteDataSection(Name: string; Data: TBytes): boolean; overload;
    {
     Retrieves section data as string
     @param Name  section name in section table.
     @throw ESectionReadError if section by name not found.
     @return string representation of section content. String encoding - UTF-8
    }
    function ReadSectionDataByName(Name: string): string;
    {
     Retrieves section data as raw byte array
     @param Name  section name in section table.
     @throw ESectionReadError if section by name not found.
     @return raw byte array representation of section content.
    }
    function ReadSectionDataAsBytesByName(Name: string): TBytes;
    {
     Retrieves maximum section table size available for writing in section table
     @param Name section name
     @param Data section data as raw byte array.
     @return true - if section data successful added.
    }
    function GetSectionTableSize(): SizeInt;
    {
     Flushes dirty section on disk image. Free file descriptor after it.
    }
    procedure Close();
  end;

  { TAdditionalSectionManipulatorFactory }

  TAdditionalSectionManipulatorFactory = class(TObject)
    {
     @param ExeFilePath
    }    class function CreateAdditionalSectionManipulator(ExeFilePath: string):
      IAdditionalSectionManipulator; static;
  end;

const
  {ADDITional SeCtion Table(ord(T)-65=20 mod 16->4}
  ADDITION_SECTION_TABLE_MARKER: DWORD = $ADD145C4;
  {compressed section size  = 32KB}
  MAX_SECTION_TABLE_COMPRESSED_SIZE: SizeInt = 32 * 1024;

type
  {Raised when section table not found or corrupted.}
  ESectionTableReadError = class(Exception);
  {Raised when section not found or corrupted.}
  ESectionReadError = class(Exception);
  {Raised when section can't be written in the section table}
  ESectionWriteError = class(Exception);
  {Raised when section table can't be written in the executable file}
  ESectionTableWriteError = class(Exception);

implementation

{ TAdditionalSection }
uses
  PseCmn,
{$IFDEF WIN64 or $ifdef WIN32}
                  PsePeFile,PsePe,
          {$ELSE}
         {$IFDEF WIN16}
           PseNeFile,
         {$ELSE}
                {$IFDEF MSDOS}
                PseMzFile,
                {$ELSE}
                       {$IFDEF UNIX}
                          PseElfFile,
                       {$ENDIF}
                {$ENDIF}
         {$ENDIF}

 {$ENDIF}
  PseFile,
  common.Data.compressor,
  common.Data.Mapper,
  HlpHashFactory,
  HlpIHash,
  HlpIHashResult;

type

  { TAdditionalSection }

  {Section can be saved custom data, such as json,xml,binary..}
  TAdditionalSection = class(TCollectionItem)
  strict private
    FCompressedDataSize: SizeInt;
    FDataSize: SizeInt;
    FCompressedDataOffset: SizeInt;
    FName: string;
    FHash: string;
    FHashAlg: string;
    FData: TBytes;
    FModified: boolean;
  private
    property Modified: boolean read FModified;
    constructor Create(AName: string; AData: TBytes; AHash: IHash);
  public
    {compressed data}
    property Data: TBytes read FData write FData;
  published
    {actual raw data size}
    property DataSize: SizeInt read FDataSize write FDataSize;
    {section binary file data size}
    property ComressedDataSize: SizeInt read FCompressedDataSize
      write FCompressedDataSize;
    {actual data offset}
    property CompressedDataOffset: SizeInt
      read FCompressedDataOffset write FCompressedDataOffset;
    {section name}
    property Name: string read FName write FName;
    {section compressed data hash}
    property Hash: string read FHash write FHash;
    {hash alg}
    property HashAlg: string read FHashAlg write FHashAlg;
  end;

  { TAdditionalSectionCollectionEnumerator }

  TAdditionalSectionCollectionEnumerator = class(TCollectionEnumerator)
  strict private
    function GetCurrent: TAdditionalSection;
  public
    property Current: TAdditionalSection read GetCurrent;
  end;

  { TAdditionalSectionCollection }

  TAdditionalSectionCollection = class(TCollection)
  strict  private
    procedure SetItem(Index: integer; AValue: TAdditionalSection);
    function GetItem(Index: integer): TAdditionalSection;
  public
    constructor Create;
    function CreateFrom(Name: string; Data: TBytes; Hash: IHash): TAdditionalSection;
    property Items[Index: integer]: TAdditionalSection read GetItem write SetItem;
    function GetEnumerator: TAdditionalSectionCollectionEnumerator;
  end;

  { TAdditionalSectionTable }

  TAdditionalSectionTable = class
  strict private
    FAdditionalSectionCollection: TAdditionalSectionCollection;
    FAdditionalSectionCollectionData: string;
    FHash: string;
    FHashAlg: string;
  private
    property AdditionalSectionCollection: TAdditionalSectionCollection
      read FAdditionalSectionCollection write FAdditionalSectionCollection;
    function Flush(TableSize: SizeInt; out Count: SizeInt): TBytes;
    procedure Fetch();
  published
    property Hash: string read FHash write FHash;
    property HashAlg: string read FHashAlg write FHashAlg;
    property AdditionalSectionCollectionData: string
      read FAdditionalSectionCollectionData write FAdditionalSectionCollectionData;

  end;

  { TAdditionalSectionManipulator }

  TAdditionalSectionManipulator = class(TInterfacedObject,
    IAdditionalSectionManipulator)
  strict private
    FAdditionalSectionTable: TAdditionalSectionTable;
    FInitialized: boolean;
    FModified: boolean;
    FExeFileStream: TFileStream;
    FPseFile: TPseFile;
    FBaseSectionTableOffset: SizeInt;
    FBaseRawDataOffset: SizeInt;
    function GetSectionByName(Name: string): TAdditionalSection;
    function CheckExecutableFile(): boolean;
    {$IF Defined(WIN64) or Defined(WIN32)}
    function CheckExecutableFileWin32x64():boolean;
    {$ENDIF}
    {$IFDEF WIN16}
    function CheckExecutableFileWin16():boolean;
    {$ENDIF}
    {$IFDEF MSDOS}
    function CheckExecutableFileMsDos():boolean;
    {$ENDIF}
    {$IFDEF UNIX}
    function CheckExecutableFileUnix():boolean;
    {$ENDIF}
    procedure CheckFileFormat();
    function CheckSectionTableAvailable(): boolean;
    procedure Fetch();
    procedure Flush();
  public
    constructor Create(ExeFilePath: string);
    destructor Destroy; override;
    function GetSectionNames(): TStringList;
    function ExistsSectionByName(Name: string): boolean;
    function WriteDataSection(Name: string; Data: string): boolean; overload;
    function WriteDataSection(Name: string; Data: TBytes): boolean; overload;
    function ReadSectionDataByName(Name: string): string;
    function ReadSectionDataAsBytesByName(Name: string): TBytes;
    function GetSectionTableSize(): SizeInt;
    procedure Close();
  end;

var
  SupportedEncoding: TEncoding;
  SupportedHash: IHash = nil;
  SupportedCompressor: IDataCompressor = nil;
  SupportedMapper: IDataMapper = nil;

{ TAdditionalSection }
constructor TAdditionalSection.Create(AName: string; AData: TBytes; AHash: IHash);
var
  HashValue: IHashResult;
  HashAlgName: string;
  HashValueString: string;
begin
  FModified := True;
  FName := AName;
  FData := SupportedCompressor.CompressDataGzip(AData);
  FDataSize := Length(AData);
  FCompressedDataSize := Length(FData);
  HashAlgName := AHash.GetName;
  HashAlg := Copy(HashAlgName, 1, Length(HashAlgName));
  HashValue := AHash.ComputeBytes(FData);
  HashValueString := HashValue.ToString();
  Hash := Copy(HashValueString, 1, Length(HashValueString));
  HashValue := nil;
end;

{ TAdditionalSectionCollectionEnumerator }
function TAdditionalSectionCollectionEnumerator.GetCurrent: TAdditionalSection;
begin
  Result := inherited GetCurrent as TAdditionalSection;
end;

{ TAdditionalSectionCollection }
procedure TAdditionalSectionCollection.SetItem(Index: integer;
  AValue: TAdditionalSection);
begin
  inherited SetItem(Index, AValue);
end;

function TAdditionalSectionCollection.GetItem(Index: integer): TAdditionalSection;
begin
  Result := inherited GetItem(Index) as TAdditionalSection;
end;

constructor TAdditionalSectionCollection.Create;
begin
  inherited Create(TAdditionalSection);
end;

function TAdditionalSectionCollection.CreateFrom(Name: string;
  Data: TBytes; Hash: IHash): TAdditionalSection;
begin
  Result := TAdditionalSection.Create(Name, Data, Hash);
end;

function TAdditionalSectionCollection.GetEnumerator:
TAdditionalSectionCollectionEnumerator;
begin
  Result := inherited GetEnumerator as TAdditionalSectionCollectionEnumerator;
end;

{ TAdditionalSectionTable }
procedure TAdditionalSectionTable.Fetch();
begin
  if (FAdditionalSectionTable.HashAlg <> SupportedHash.GetName) then
    raise ESectionTableReadError.CreateFmt('Unsupported hash algorithm ''%s''',
      [FAdditionalSectionTable.HashAlg]);
  EvaluatedHash := SupportedHash.ComputeString(
    FAdditionalSectionTable.AdditionalSectionCollectionData,
    SupportedEncoding).ToString();
  if FAdditionalSectionTable.Hash <> EvaluatedHash then
    raise ESectionTableReadError.CreateFmt('Wrong hash value ''%s''',
      [FAdditionalSectionTable.Hash]);
  AdditionalSectionCollection :=
    SupportedMapper.DeSerializeFrom(
    FAdditionalSectionTable.AdditionalSectionCollectionData,
    TAdditionalSectionCollection) as TAdditionalSectionCollection;

  for AdditionalSection in FAdditionalSectionTable.AdditionalSectionCollection do
  begin
    //    AdditionalSection.CompressedDataOffset;
  end;
end;

destructor TAdditionalSectionTable.Destroy;
begin
  FreeAndNil(FAdditionalSectionCollection);
  inherited Destroy;
end;
{ TAdditionalSectionManiputlator }
constructor TAdditionalSectionManipulator.Create(ExeFilePath: string);
begin
  FAdditionalSectionTable := TAdditionalSectionTable.Create;
  if (Trim(ExeFilePath) = '') then
  begin
    raise ESectionTableReadError.Create('Filename not specified');
  end;
  try
    FExeFileStream := TFileStream.Create(ExeFilePath, fmOpenReadWrite or
      fmShareExclusive);
  except
    FExeFileStream := TFileStream.Create(ExeFilePath, fmOpenRead or fmShareExclusive);
  end;
  FPseFile := TPseFile.GetInstance(FExeFileStream, False);
  CheckFileFormat();
  FBaseSectionTableOffset := FPseFile.GetSizeOfFileImage();
  FBaseRawDataOffset := FBaseSectionTableOffset + GetSectionTableSize();
  Fetch();
  FInitialized := True;
  FModified := False;
end;

destructor TAdditionalSectionManipulator.Destroy;
begin
  if FInitialized then
    Close();
  FreeAndNil(FAdditionalSectionTable);
  inherited Destroy;
end;

function TAdditionalSectionManipulator.GetSectionNames(): TStringList;
var
  ReadCollectionItem: TCollectionItem;
begin
  Result := TStringList.Create;
  for ReadCollectionItem in FAdditionalSectionTable.AdditionalSectionCollection do
  begin
    Result.Add((ReadCollectionItem as TAdditionalSection).Name);
  end;
end;

function TAdditionalSectionManipulator.ExistsSectionByName(Name: string): boolean;
var
  Names: TStringList;
begin
  Names := GetSectionNames();
  Result := Names.IndexOf(Name) > -1;
  FreeAndNil(Names);
end;

function TAdditionalSectionManipulator.WriteDataSection(Name: string;
  Data: string): boolean;
begin
  Result := WriteDataSection(Name, TEncoding.UTF8.GetBytes(Data));
end;

function TAdditionalSectionManipulator.WriteDataSection(Name: string;
  Data: TBytes): boolean;
begin
  Result := False;
  if not FInitialized then
    raise ESectionWriteError.CreateFmt(
      'Section ''%s'' can''t be written, because section manipulator is closed or not initialized',
      [Name]);
  if ExistsSectionByName(Name) then
    raise ESectionWriteError.CreateFmt(
      'Section ''%s'' can''t be overwrite exists data', [Name]);
  Result := FAdditionalSectionTable.AdditionalSectionCollection.CreateFrom(
    Name, Data, SupportedHash) <> nil;
end;

function TAdditionalSectionManipulator.GetSectionByName(Name: string):
TAdditionalSection;

var
  AdditionalSectionCollectionItem: TAdditionalSection;
  BasicAdditionalSectionCollectionItem: TCollectionItem;
begin
  Result := nil;
  if (Trim(Name) = '') then
    raise ESectionReadError.Create('Section name is empty!');
  for BasicAdditionalSectionCollectionItem in
    FAdditionalSectionTable.AdditionalSectionCollection do
  begin
    AdditionalSectionCollectionItem :=
      BasicAdditionalSectionCollectionItem as TAdditionalSection;
    if AdditionalSectionCollectionItem.Name = Name then
    begin
      Result := AdditionalSectionCollectionItem;
      exit;
    end;
  end;
end;

function TAdditionalSectionManipulator.CheckExecutableFile(): boolean;
begin
  Result := True
  {$IF Defined(WIN64) or Defined(WIN32)}
  and CheckExecutableFileWin32x64()
  {$ENDIF}
  {$IFDEF WIN16}
  and CheckExecutableFileWin16()
  {$ENDIF}
  {$IFDEF MSDOS}
  and CheckExecutableFileMsDos()
  {$ENDIF}
  {$IFDEF UNIX}
  and CheckExecutableFileUnix()
  {$ENDIF}
  ;
end;

{$IF Defined(WIN64) or Defined(WIN32)}
function TAdditionalSectionManipulator.CheckExecutableFileWin32x64(): boolean;
begin
  Result:=((FPseFile as TPsePeFile).ImageHeader.Characteristics and IMAGE_FILE_EXECUTABLE_IMAGE<>0)
  and((FPseFile as TPsePeFile).ImageHeader.NumberOfSymbols=0)
  and((FPseFile as TPsePeFile).ImageHeader.PointerToSymbolTable=0)
end;
{$ENDIF}

{$IFDEF WIN16}
function TAdditionalSectionManiputlator.CheckExecutableFileWin16():boolean;
begin
  Result:=false;
end;
{$ENDIF}
{$IFDEF MSDOS}
function TAdditionalSectionManiputlator.CheckExecutableFileMsDos():boolean;
begin
  Result:=false;
end;
{$ENDIF}
{$IFDEF UNIX}
function TAdditionalSectionManiputlator.CheckExecutableFileUnix():boolean;
begin
  Result:=false;
end;
{$ENDIF}

procedure TAdditionalSectionManipulator.CheckFileFormat();
begin
  if not CheckExecutableFile() then
  begin
    raise ESectionTableReadError.CreateFmt(
      'Section table can''t be read because executable file is not valid for ''%s'' type.'
      + 'May be he has debug info and can''t be released. Please check and try again.',
      [FPseFile.GetFriendlyName]);
  end;
end;

function TAdditionalSectionManipulator.CheckSectionTableAvailable(): boolean;
begin
  Result := (FExeFileStream.Position < FExeFileStream.Size) and
    (FExeFileStream.ReadDWord = ADDITION_SECTION_TABLE_MARKER);
end;

function TAdditionalSectionManipulator.ReadSectionDataByName(Name: string): string;
begin
  Result := TEncoding.UTF8.GetString(ReadSectionDataAsBytesByName(Name));
end;

function TAdditionalSectionManipulator.ReadSectionDataAsBytesByName(
  Name: string): TBytes;
var
  section: TAdditionalSection;
begin
  Result := nil;
  section := GetSectionByName(Name);
  if (section = nil) then
    raise ESectionReadError.CreateFmt('Section with name ''%s'' not found!', [Name]);
  Result := SupportedCompressor.DeCompressDataGzip(section.Data);
end;

function TAdditionalSectionManipulator.GetSectionTableSize(): SizeInt;
begin
  Result := MAX_SECTION_TABLE_COMPRESSED_SIZE;
end;

procedure TAdditionalSectionManipulator.Fetch();
var
  CompressedSectionTableSize: SizeInt;
  ActualCompressedSectionTableSize: SizeInt;
  Data, RawData: TBytes;
  EvaluatedHash: string;
  AdditionalSection: TAdditionalSection;
begin
  if FInitialized then
    raise ESectionTableReadError.Create('Section table already initialized!');
  CompressedSectionTableSize := GetSectionTableSize();
  FExeFileStream.Seek(FBaseSectionTableOffset, TSeekOrigin.soBeginning);
  if not CheckSectionTableAvailable() then
  begin
    writeln('Section table not found!');
    exit;
  end;
  ActualCompressedSectionTableSize := FExeFileStream.ReadDWord;
  if (ActualCompressedSectionTableSize > CompressedSectionTableSize) then
    raise ESectionTableReadError.CreateFmt(
      'Section table actual size %d greather than excepted max section table size is %d',
      [ActualCompressedSectionTableSize, CompressedSectionTableSize]);
  SetLength(Data, ActualCompressedSectionTableSize);
  if FExeFileStream.Read(Data, ActualCompressedSectionTableSize) <>
    ActualCompressedSectionTableSize then
    raise ESectionTableReadError.Create('Section table data corrupted!');
  RawData := SupportedCompressor.DeCompressDataGzip(Data);
  FAdditionalSectionTable := SupportedMapper.DeSerializeFrom(
    RawData, TEncoding.UTF8, TAdditionalSectionTable) as TAdditionalSectionTable;
  FAdditionalSectionTable.Fetch();
end;

procedure TAdditionalSectionManipulator.Flush();
var
  TableData: TBytes;
  MaximumTableSize: SizeInt;
  ActualTableSize: SizeInt;
  AdditionalSection: TAdditionalSection;
begin
  if not FInitialized then
    raise ESectionTableWriteError.Create(
      'Can''t write before initialization or after finalization section table!');
  if not FModified then
    exit;
  MaximumTableSize := GetSectionTableSize();
  //  FBaseSectionTableOffset:=;
  for AdditionalSection in FAdditionalSectionTable.AdditionalSectionCollection do ;
  TableData := FAdditionalSectionTable.Flush(MaximumTableSize, ActualTableSize);
  FExeFileStream.Seek(FBaseSectionTableOffset, soBeginning);
  FExeFileStream.WriteDWord(ADDITION_SECTION_TABLE_MARKER);
  FExeFileStream.WriteDWord(ActualTableSize);
  FExeFileStream.Write(TableData, MaximumTableSize);
  //  FExeFileStream.
end;

function TAdditionalSectionTable.Flush(TableSize: SizeInt; out Count: SizeInt): TBytes;
var
  GzippedData: TBytes;
  GzippedDataLength: SizeInt;
  I: integer;
begin
  SetLength(Result, 0);
  if (TableSize < 0) then
    raise ESectionTableWriteError.CreateFmt('Wrong table size %d', [TableSize]);
  SetLength(Result, TableSize);
  FillByte(Result, TableSize, 0);
  FAdditionalSectionCollectionData :=
    SupportedMapper.SerializeTo(FAdditionalSectionCollection,
    TAdditionalSectionCollection);
  GzippedData := SupportedCompressor.CompressDataGzip(
    SupportedEncoding.GetBytes(FAdditionalSectionCollectionData));
  GzippedDataLength := Length(GzippedData);
  if GzippedDataLength > TableSize then
    raise ESectionTableWriteError.CreateFmt(
      'Compressed table size is too big! Size %d bytes', [GzippedDataLength]);
  for I := 0 to GzippedDataLength - 1 do
    Result[i] := GzippedData[i];
  Count := GzippedDataLength;
end;

procedure TAdditionalSectionManipulator.Close();
begin
  Flush();
  FInitialized := False;
  FreeAndNil(FPseFile);
  FreeAndNil(FExeFileStream);
end;

{ TAdditionalSectionManipulatorFactory }

class function TAdditionalSectionManipulatorFactory.CreateAdditionalSectionManipulator(
  ExeFilePath: string): IAdditionalSectionManipulator;
begin
  Result := TAdditionalSectionManipulator.Create(ExeFilePath);
end;

initialization

{$IFDEF WIN64 or WIN32}
  TPseFile.RegisterFile(TPsePeFile);
{$ELSE}
  {$IFDEF WIN16}
           TPseFile.RegisterFile(TPseNeFile);
  {$ELSE}
         {$IFDEF MSDOS}
                  TPseFile.RegisterFile(TPseMzFile);
         {$ELSE}
                {$IFDEF UNIX}
                          TPseFile.RegisterFile(TPseElfFile);
                {$ENDIF}
         {$ENDIF}
  {$ENDIF}

 {$ENDIF}
  SupportedHash := HlpHashFactory.THashFactory.TCrypto.CreateSHA2_512();
  SupportedCompressor := TDataCompressorFactory.CreateDataCompressor();
  SupportedMapper := TDataMapperFactory.CreateDataMapper(mtJSON);
  SupportedEncoding := TEncoding.UTF8;

finalization
  SupportedHash := nil;
  SupportedCompressor := nil;
  SupportedMapper := nil;
  SupportedEncoding := nil;
end.
