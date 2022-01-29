unit CustApp.AddSectionManiputlator;

{$mode objfpc}{$H+}

interface

uses Classes, common.autocloseable, SysUtils;

type

  { IAdditionalSectionManager
    Provides methods for read and write additional resource section into executable
    file.
  }
  IAdditionalSectionManager = interface(IAutoCloseable)
    ['{944DF94F-E0DE-43A7-8149-B08D933971DC}']
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
    function ReadSectionDataAsBytesByName(Name: string): TBytesStream;
    {
     Retrieves maximum section table size available for writing in section table
     @return size in byte of section table
    }
    function GetSectionTableSize(): SizeInt;
    {
     Retrives offset for section data in executable file.
     @return offset in bytes for section data
    }
    function GetRawDataOffset(): SizeInt;
    function GetSectionTableHeadersSize(): SizeInt;
  end;

  { TAdditionalSectionManagerFactory }

  TAdditionalSectionManagerFactory = class(TObject)
    {
     @param ExeFilePath
    }    class function CreateAdditionalSectionManager(ExeFilePath: string):
      IAdditionalSectionManager; static;
  end;

const
  {ADDITional SeCtion Table(ord(T)-65=20 mod 16->4}
  ADDITION_SECTION_TABLE_MARKER: DWORD = $ADD145C4;
  {compressed section size  = 32KB}
  MAX_SECTION_TABLE_COMPRESSED_SIZE: SizeInt = SizeOf(TByteArray);

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
    FModified: boolean;
    FCompressedDataSize: SizeInt;
    FDataSize: SizeInt;
    FCompressedDataOffset: SizeInt;
    FName: string;
    FHash: string;
    FHashAlg: string;
    FData: TBytesStream;
  private
    property Modified: boolean read FModified;
    procedure Fetch(AStream: TStream);
    procedure MarkDirty();
    procedure MarkFlushed();
    function Flush(CurrentSectionDataOffset: SizeInt): SizeInt;
    class procedure CreateFor(ANewSection: TAdditionalSection;
      AName: string; AData: TBytesStream); static;
  public
    {compressed data}
    property Data: TBytesStream read FData write FData;
    destructor Destroy; override;
  published
    {actual raw data size}
    property DataSize: SizeInt read FDataSize write FDataSize;
    {section binary file data size}
    property CompressedDataSize: SizeInt read FCompressedDataSize
      write FCompressedDataSize;
    {actual data offset}
    property CompressedDataOffset: SizeInt read FCompressedDataOffset
      write FCompressedDataOffset;
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
    function CreateFrom(Name: string; Data: TBytesStream): TAdditionalSection;
    property Items[Index: integer]: TAdditionalSection read GetItem write SetItem;
    function GetEnumerator: TAdditionalSectionCollectionEnumerator;
    class function CreateSectionCollection: TCollection; static;
  end;

  { TAdditionalSectionTable }

  TAdditionalSectionTable = class(TPersistent)
  strict private
    FAdditionalSectionCollection: TAdditionalSectionCollection;
    FAdditionalSectionCollectionData: string;
    FHash: string;
    FHashAlg: string;
  private
    property AdditionalSectionCollection: TAdditionalSectionCollection
      read FAdditionalSectionCollection write FAdditionalSectionCollection;
    procedure Fetch(AStream: TStream);
    function Flush(TableSize: SizeInt; out Count: SizeInt): TBytesStream;
  public
    destructor Destroy; override;
    procedure InitNewTable();
  published
    property Hash: string read FHash write FHash;
    property HashAlg: string read FHashAlg write FHashAlg;
    property AdditionalSectionCollectionData: string
      read FAdditionalSectionCollectionData write FAdditionalSectionCollectionData;
  end;

  { TAdditionalSectionManager }

  TAdditionalSectionManager = class(TAutoCloseable,
    IAdditionalSectionManager)
  strict private
    FInitialized: boolean;
    FModified: boolean;
    FBaseSectionTableOffset: SizeInt;
    FRawDataSectionTableOffset: SizeInt;
    FBaseRawDataOffset: SizeInt;
    FAdditionalSectionTable: TAdditionalSectionTable;
    FExeFileStream: TFileStream;
    FPseFile: TPseFile;
    procedure Fetch();
    procedure CheckFileFormat();
    procedure Flush();
    procedure PrepareSectionTableForFlushing();
    procedure WriteSectionTableToStream(TempExeStream: TMemoryStream);
    procedure WriteSectionDatasToStream(TempExeStream: TStream);
    procedure WriteNewTableOnFileImage(TempExeStream: TStream);
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
    function GetSectionByName(Name: string): TAdditionalSection;
    function CheckSectionTableAvailable(): boolean;
    function ReadSectionTableData(): TBytesStream;
  public
    constructor Create(ExeFilePath: string);
    procedure DestroyResources; override;
    procedure Close(); override;
    function GetSectionNames(): TStringList;
    function ExistsSectionByName(Name: string): boolean;
    function WriteDataSection(Name: string; Data: string): boolean; overload;
    function WriteDataSection(Name: string; Data: TBytes): boolean; overload;
    function ReadSectionDataByName(Name: string): string;
    function ReadSectionDataAsBytesByName(Name: string): TBytesStream;
    function GetSectionTableSize(): SizeInt;
    function GetSectionTableHeadersSize(): SizeInt;
    function GetRawDataOffset(): SizeInt;
  end;

  { THashVerifier }

  { THashDataManager }

  THashDataManager = class
    class procedure VerifyHash(Data: TStream; HashAlg: string; ExceptedHash: string); static;
    class procedure EvaluateHash(Data: TStream; out HashAlg: string;
      out EvaluatedHash: string); static;
  end;

var
  SupportedEncoding: TEncoding;
  SupportedHash: IHash = nil;
  SupportedCompressor: IDataCompressor = nil;
  SupportedMapper: IDataMapper = nil;

{ TAdditionalSection }

destructor TAdditionalSection.Destroy;
begin
  FModified := False;
  FName := '';
  FreeAndNil(FData);
  FCompressedDataSize := 0;
  FCompressedDataOffset := 0;
  FDataSize := 0;
  FHashAlg := '';
  FHash := '';
  inherited Destroy;
end;

procedure TAdditionalSection.Fetch(AStream: TStream);
begin
  if FCompressedDataSize <= 0 then
    raise ESectionReadError.CreateFmt('Can''t read section data. Wrong data size: %d',
      [FCompressedDataSize]);
  FData := TBytesStream.Create();
  AStream.Position := FCompressedDataOffset;
  if FData.CopyFrom(AStream, FCompressedDataSize) <> FCompressedDataSize then
    raise ESectionReadError.Create('Can''t read section data. Section data corrupted!');
  THashDataManager.VerifyHash(FData, FHashAlg, FHash);
  writeln(format('Section data with name ''%s'' successful fetched from executable image',
    [FName]));
end;

procedure TAdditionalSection.MarkDirty();
begin
  FModified := True;
end;

procedure TAdditionalSection.MarkFlushed;
begin
  FModified := False;
end;

function TAdditionalSection.Flush(CurrentSectionDataOffset: SizeInt): SizeInt;
begin
  FCompressedDataOffset := CurrentSectionDataOffset;
  Result := FCompressedDataOffset + FCompressedDataSize;
end;

class procedure TAdditionalSection.CreateFor(ANewSection: TAdditionalSection;
  AName: string; AData: TBytesStream);
var
  AHash, AHashAlg: string;
begin
  writeln(format('New section with name ''%s'' filling started', [AName]));
  ANewSection.Name := AName;
  ANewSection.Data := SupportedCompressor.CompressDataZip(AData);
  ANewSection.DataSize := AData.Size;
  ANewSection.CompressedDataSize := ANewSection.Data.Size;
  THashDataManager.EvaluateHash(ANewSection.Data, AHashAlg, AHash);
  ANewSection.HashAlg := AHashAlg;
  ANewSection.Hash := AHash;
  ANewSection.MarkDirty();
  writeln(format('New section with name ''%s'' filling successful finished', [AName]));
end;

{ TAdditionalSectionCollectionEnumerator }
constructor TAdditionalSectionCollection.Create;
begin
  inherited Create(TAdditionalSection);
end;

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

function TAdditionalSectionCollection.CreateFrom(Name: string;
  Data: TBytesStream): TAdditionalSection;
begin
  Result := Add as TAdditionalSection;
  TAdditionalSection.CreateFor(Result, Name, Data);
end;

function TAdditionalSectionCollection.GetEnumerator:
TAdditionalSectionCollectionEnumerator;
begin
  Result := TAdditionalSectionCollectionEnumerator.Create(Self);
end;

class function TAdditionalSectionCollection.CreateSectionCollection: TCollection;
begin
  Result := TAdditionalSectionCollection.Create;
end;

{ TAdditionalSectionTable }
destructor TAdditionalSectionTable.Destroy;
begin
  FreeAndNil(FAdditionalSectionCollection);
  FHash := '';
  FHashAlg := '';
  FAdditionalSectionCollectionData := '';
  inherited Destroy;
end;

procedure TAdditionalSectionTable.Fetch(AStream: TStream);
var
  AdditionalSection: TAdditionalSection;
  DataStream: TStringStream;
begin
  writeln('Section table fetching started');
  DataStream := TStringStream.Create(FAdditionalSectionCollectionData);
  try
    THashDataManager.VerifyHash(DataStream, FHashAlg, FHash);
  finally
    FreeAndNil(DataStream);
  end;
  writeln('Section hash and hash algorithm applied!');
  FAdditionalSectionCollection :=
    SupportedMapper.DeSerializeFrom(FAdditionalSectionCollectionData,
    TAdditionalSectionCollection, TCreateObjectFunction(
    @TAdditionalSectionCollection.CreateSectionCollection)) as
    TAdditionalSectionCollection;
  for AdditionalSection in FAdditionalSectionCollection do
  begin
    AdditionalSection.Fetch(AStream);
  end;
  writeln('Section table fetching successful finished');
end;

function TAdditionalSectionTable.Flush(TableSize: SizeInt;
  out Count: SizeInt): TBytesStream;
var
  TableData: string;
  Stream: TStringStream;
  TableDataStream: TStream;
begin
  writeln('Section table flushing started');
  if (TableSize < 0) then
    raise ESectionTableWriteError.CreateFmt('Wrong table size %d', [TableSize]);
  FAdditionalSectionCollectionData :=
    SupportedMapper.SerializeTo(FAdditionalSectionCollection,
    TAdditionalSectionCollection);
  Stream := TStringStream.Create(FAdditionalSectionCollectionData);
  try
    THashDataManager.EvaluateHash(Stream, FHashAlg, FHash);
  finally
    FreeAndNil(Stream);
  end;
  TableData := SupportedMapper.SerializeTo(Self, TAdditionalSectionTable);
  TableDataStream := TBytesStream.Create(SupportedEncoding.GetBytes(TableData));
  try
    Result := SupportedCompressor.CompressDataZip(TableDataStream);
  finally
    FreeAndNil(TableDataStream);
  end;
  Count := Result.Size;
  if Count > TableSize then
    raise ESectionTableWriteError.CreateFmt(
      'Compressed table size is too big! Size %d bytes', [Count]);
  Result.Size := TableSize;
  writeln('Section table flushing successful finished');
end;

procedure TAdditionalSectionTable.InitNewTable();
begin
  if FAdditionalSectionCollection <> nil then
    raise ESectionTableReadError.Create(
      'Section table already initialized, but call method "InitNewTable". It is error!');
  FAdditionalSectionCollection := TAdditionalSectionCollection.Create;
  writeln('New section table initalized.');
end;

{ TAdditionalSectionManager }
constructor TAdditionalSectionManager.Create(ExeFilePath: string);
begin
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
  FRawDataSectionTableOffset := FBaseSectionTableOffset + GetSectionTableHeadersSize();
  FBaseRawDataOffset := FBaseSectionTableOffset + GetRawDataOffset();
  Fetch();
  FInitialized := True;
  FModified := False;
  writeln('Section manager initialized!');
end;

procedure TAdditionalSectionManager.DestroyResources;
begin
  FreeAndNil(FPseFile);
  FreeAndNil(FExeFileStream);
  FreeAndNil(FAdditionalSectionTable);
  inherited DestroyResources;
  writeln('Section manager all resources freed!');
end;

procedure TAdditionalSectionManager.Close();
begin
  Flush();
  FInitialized := False;
  FModified := False;
  writeln('Section manager closed!');
end;

procedure TAdditionalSectionManager.Fetch();
var
  RawDataStream: TBytesStream;
begin
  if FInitialized then
    raise ESectionTableReadError.Create('Section table already initialized!');
  FExeFileStream.Seek(FBaseSectionTableOffset, TSeekOrigin.soBeginning);
  if not CheckSectionTableAvailable() then
  begin
    FAdditionalSectionTable := TAdditionalSectionTable.Create;
    FAdditionalSectionTable.InitNewTable();
    exit;
  end;
  RawDataStream := ReadSectionTableData();
  try
    FAdditionalSectionTable :=
      SupportedMapper.DeSerializeFrom(RawDataStream, TEncoding.UTF8,
      TAdditionalSectionTable) as TAdditionalSectionTable;
    FAdditionalSectionTable.Fetch(FExeFileStream);
  finally
    FreeAndNil(RawDataStream);
  end;
end;

procedure TAdditionalSectionManager.CheckFileFormat();
begin
  writeln('Validation executable image format started');
  if not CheckExecutableFile() then
  begin
    raise ESectionTableReadError.CreateFmt(
      'Section table can''t be read because executable file is not valid for ''%s'' type.'
      + 'May be he has debug info and can''t be released. Please check and try again.',
      [FPseFile.GetFriendlyName]);
  end;
  writeln('Executable image format for this os and architecture is valid!');
end;

procedure TAdditionalSectionManager.Flush();
var
  TempExeStream: TMemoryStream;
begin
  WriteLn('Section table and section data flushing to file image started!');
  if not FModified then
    exit;
  if not FInitialized then
    raise ESectionTableWriteError.Create(
      'Can''t write before initialization or after finalization section table!');
  writeln('Backup executable file into memory started');
  TempExeStream := TMemoryStream.Create;
  FExeFileStream.Position := 0;
  TempExeStream.CopyFrom(FExeFileStream, FExeFileStream.Size);
  writeln(format('Backup executable file into memory completed, size %d bytes',
    [TempExeStream.size]));
  try
    WriteSectionTableToStream(TempExeStream);
    WriteSectionDatasToStream(TempExeStream);
    WriteNewTableOnFileImage(TempExeStream);
  finally
    writeln('Temporary flushing stream freed!');
    FreeAndNil(TempExeStream);
  end;
  FModified := False;
  WriteLn('Section table and section data flushing to file image completed!');
end;

procedure TAdditionalSectionManager.PrepareSectionTableForFlushing;
var
  CurrentSectionDataOffset: SizeInt;
  AdditionalSection: TAdditionalSection;
begin
  CurrentSectionDataOffset := FBaseRawDataOffset;
  for AdditionalSection in FAdditionalSectionTable.AdditionalSectionCollection do
  begin
    CurrentSectionDataOffset :=
      CurrentSectionDataOffset + AdditionalSection.Flush(CurrentSectionDataOffset);
  end;
end;

procedure TAdditionalSectionManager.WriteSectionTableToStream(
  TempExeStream: TMemoryStream);
var
  TableDataStream: TBytesStream;
  MaximumTableSize: SizeInt;
  ActualTableSize: SizeInt;
  ActualSize: SizeInt;
begin
  PrepareSectionTableForFlushing;
  MaximumTableSize := GetSectionTableSize();
  TableDataStream := FAdditionalSectionTable.Flush(MaximumTableSize, ActualTableSize);
  TempExeStream.Position := FBaseSectionTableOffset;
  TempExeStream.WriteDWord(ADDITION_SECTION_TABLE_MARKER);
  TempExeStream.WriteDWord(ActualTableSize);
  try
    TableDataStream.Position := 0;
    TempExeStream.CopyFrom(TableDataStream, TableDataStream.Size);
  finally
    FreeAndNil(TableDataStream);
  end;
  ActualSize := TempExeStream.Size;
  if ActualSize <> FBaseRawDataOffset then
    raise ESectionWriteError.Create(
      'Can''t write section data because raw data offset is wrong!May be section table corrupted!');
end;

procedure TAdditionalSectionManager.WriteSectionDatasToStream(TempExeStream: TStream);
var
  AdditionalSection: TAdditionalSection;
  ActualSize: SizeInt;
  WrittenBytesNumber: SizeInt;
begin
  TempExeStream.Position := FBaseRawDataOffset;
  for AdditionalSection in FAdditionalSectionTable.AdditionalSectionCollection do
  begin
    ActualSize := TempExeStream.Size;
    if ActualSize <> AdditionalSection.CompressedDataOffset then
      raise ESectionWriteError.CreateFmt(
        'Can''t write section ''%s'' data because raw data offset is wrong',
        [AdditionalSection.Name]);
    WrittenBytesNumber := TempExeStream.CopyFrom(AdditionalSection.Data,
      AdditionalSection.CompressedDataSize);
    writeln(format('must be written %d, actually written %d (in bytes)',
      [AdditionalSection.CompressedDataSize, WrittenBytesNumber]));
    if WrittenBytesNumber <> AdditionalSection.CompressedDataSize then
      raise ESectionWriteError.CreateFmt(
        'Can''t write section ''%s'' data because stream corrupted!',
        [AdditionalSection.Name]);
  end;
end;

procedure TAdditionalSectionManager.WriteNewTableOnFileImage(TempExeStream: TStream);
begin
  FExeFileStream.Size := 0;
  FExeFileStream.Position := 0;
  TempExeStream.Position := 0;
  FExeFileStream.CopyFrom(TempExeStream, TempExeStream.Size);
end;

function TAdditionalSectionManager.CheckExecutableFile(): boolean;
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
function TAdditionalSectionManager.CheckExecutableFileWin32x64(): boolean;
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

function TAdditionalSectionManager.GetSectionByName(Name: string): TAdditionalSection;
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

function TAdditionalSectionManager.CheckSectionTableAvailable(): boolean;
begin
  Result := (FExeFileStream.Position < FExeFileStream.Size) and
    (FExeFileStream.ReadDWord = ADDITION_SECTION_TABLE_MARKER);
end;

function TAdditionalSectionManager.ReadSectionTableData(): TBytesStream;
var
  CompressedSectionTableSize: SizeInt;
  ActualCompressedSectionTableSize: SizeInt;
  Data: TBytesStream;
begin
  CompressedSectionTableSize := GetSectionTableSize();
  ActualCompressedSectionTableSize := FExeFileStream.ReadDWord;
  if (ActualCompressedSectionTableSize > CompressedSectionTableSize) then
    raise ESectionTableReadError.CreateFmt(
      'Section table actual size %d greather than excepted max section table size is %d',
      [ActualCompressedSectionTableSize, CompressedSectionTableSize]);
  Data := TBytesStream.Create();
  try
    FExeFileStream.Position := FRawDataSectionTableOffset;
    Data.CopyFrom(FExeFileStream, ActualCompressedSectionTableSize);
    Result := SupportedCompressor.DeCompressDataZip(Data);
  finally
    FreeAndNil(Data);
  end;
end;

function TAdditionalSectionManager.GetSectionNames(): TStringList;
var
  ReadCollectionItem: TCollectionItem;
begin
  Result := TStringList.Create;
  if (FAdditionalSectionTable.AdditionalSectionCollection = nil) then
    exit;
  for ReadCollectionItem in FAdditionalSectionTable.AdditionalSectionCollection do
  begin
    Result.Add((ReadCollectionItem as TAdditionalSection).Name);
  end;
end;

function TAdditionalSectionManager.ExistsSectionByName(Name: string): boolean;
var
  Names: TStringList;
begin
  Names := GetSectionNames();
  Result := Names.IndexOf(Name) > -1;
  FreeAndNil(Names);
end;

function TAdditionalSectionManager.WriteDataSection(Name: string; Data: string): boolean;
begin
  Result := WriteDataSection(Name, TEncoding.UTF8.GetBytes(Data));
end;

function TAdditionalSectionManager.WriteDataSection(Name: string; Data: TBytes): boolean;
var
  Stream: TBytesStream;
  NewSection: TAdditionalSection;
begin
  Result := False;
  if not FInitialized then
    raise ESectionWriteError.CreateFmt(
      'Section ''%s'' can''t be written, because section manipulator is closed or not initialized',
      [Name]);
  if ExistsSectionByName(Name) then
    raise ESectionWriteError.CreateFmt(
      'Section ''%s'' can''t be overwrite exists data', [Name]);
  Stream := TBytesStream.Create(Data);
  try
    try
      NewSection := FAdditionalSectionTable.AdditionalSectionCollection.CreateFrom(Name,
        Stream);
      Result := NewSection <> nil;
    except
      FreeAndNil(NewSection);
    end;
  finally
    FreeAndNil(Stream);
  end;
  FModified := Result;
end;

function TAdditionalSectionManager.ReadSectionDataByName(Name: string): string;
var
  ResultStream: TBytesStream;
begin
  ResultStream := ReadSectionDataAsBytesByName(Name);
  try
    Result := SupportedEncoding.GetString(ResultStream.Bytes);
  finally
    FreeAndNil(ResultStream);
  end;
end;

function TAdditionalSectionManager.ReadSectionDataAsBytesByName(
  Name: string): TBytesStream;
var
  section: TAdditionalSection;
begin
  Result := nil;
  section := GetSectionByName(Name);
  if (section = nil) then
    raise ESectionReadError.CreateFmt('Section with name ''%s'' not found!', [Name]);
  Result := SupportedCompressor.DeCompressDataZip(section.Data);
end;

function TAdditionalSectionManager.GetSectionTableSize(): SizeInt;
begin
  Result := MAX_SECTION_TABLE_COMPRESSED_SIZE;
end;

function TAdditionalSectionManager.GetSectionTableHeadersSize(): SizeInt;
begin
  Result := 2 * SizeOf(DWord);
end;

function TAdditionalSectionManager.GetRawDataOffset(): SizeInt;
begin
  Result := GetSectionTableSize() + GetSectionTableHeadersSize();
end;

{ TAdditionalSectionManagerFactory }

class function TAdditionalSectionManagerFactory.CreateAdditionalSectionManager(
  ExeFilePath: string): IAdditionalSectionManager;
begin
  Result := TAdditionalSectionManager.Create(ExeFilePath);
end;

{ THashDataManager }

class procedure THashDataManager.VerifyHash(Data: TStream; HashAlg: string;
  ExceptedHash: string);
var
  EvaluatedHash: string;
begin
  writeln('Hash validation started');
  Data.Position := 0;
  if (HashAlg <> SupportedHash.GetName) then
    raise ESectionTableReadError.CreateFmt('Unsupported hash algorithm ''%s''',
      [HashAlg]);
  EvaluatedHash := SupportedHash.ComputeStream(Data).ToString();
  if ExceptedHash <> EvaluatedHash then
    raise ESectionTableReadError.CreateFmt('Wrong hash value ''%s''', [EvaluatedHash]);
  Data.Position := 0;
  writeln(format('Hash ''%s'' is valid', [ExceptedHash]));
end;

class procedure THashDataManager.EvaluateHash(Data: TStream;
  out HashAlg: string; out EvaluatedHash: string);
var
  HashValue: IHashResult;
  HashAlgName: string;
  HashValueString: string;
begin
  Data.Position := 0;
  EvaluatedHash := '';
  HashAlg := '';
  HashAlgName := SupportedHash.GetName;
  HashAlg := Copy(HashAlgName, 1, Length(HashAlgName));
  HashValue := SupportedHash.ComputeStream(Data);
  HashValueString := HashValue.ToString();
  EvaluatedHash := Copy(HashValueString, 1, Length(HashValueString));
  Data.Position := 0;
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
