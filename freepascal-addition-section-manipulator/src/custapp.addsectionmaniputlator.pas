unit CustApp.AddSectionManiputlator;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
  TAdditionalSection = class;
  { TAddSectionManiputlator }

  TAddSectionManiputlator = class
  strict private
    FAdditionalSectionCollection: TCollection;
    FExeFileStream: TFileStream;
    procedure ReadSections();
  public
    constructor Create(ExeFilePath: string);
    destructor Destroy; override;
    function WriteDataSection(Data: string): boolean; overload;
    function WriteDataSection(Data: TBytes): boolean; overload;
    function ReadSectionAt(IndexBefore: integer): TAdditionalSection;
    function ReadSectionDataAt(IndexBefore: integer): string;
    function ReadSectionDataAsBytesAt(IndexBefore: integer): TBytes;
  end;

  { TAdditionalSection }

  TAdditionalSection = class(TCollectionItem)

  strict private
    FSectionNumberBefore: DWORD;
    FOriginalSize: DWORD;
    FSectionSize: DWORD;
    FSectionData: TBytes;
    FSectionMarker: DWORD;
    FRawSectionDataSize: DWORD;
    FNameSize: word;
    FName: string;

    {Compress into gzip compressed json file serialized to json MapFileInfo object.
    @param MapInfoJsonData serialized to json MapFileInfo object.
    @returns base64 string of gzipped stream}
    //function CompressMapInfoToGzip(MapInfoJsonData: string): TBytes;
    {Decompress gzip compressed json file and read json string for loading map info in memory.
    @param ExeFilePath executable file with gzip compressed stream.
    @returns serialized to json MapFileInfo object.}
    //function DecompressGzData(Data: TBytes): string;
  public
    property SectionNumberBefore: DWORD read FSectionNumberBefore
      write FSectionNumberBefore;
    property OriginalSize: DWORD read FOriginalSize write FOriginalSize;
    property SectionSize: DWORD read FSectionSize write FSectionSize;
    property SectionData: TBytes read FSectionData write FSectionData;
    property SectionMarker: DWORD read FSectionMarker write FSectionMarker;
    property RawSectionDataSize: DWORD read FRawSectionDataSize
      write FRawSectionDataSize;
    property NameSize: word read FNameSize write FNameSize;
    property Name: string read FName write FName;
    function ToBytes(): TBytes;
    procedure FromBytes(Data: TBytes);
  end;

const
  ADDITION_SECTION_MARKER = $AABAD500;
  SECTION_DATA_DIFF_SIZE = 4 * Sizeof(DWord);

implementation

{ TAdditionalSection }
uses {$IFDEF MSWINDOWS}
  PseMzFile, PseNeFile, PsePeFile,
  {$ELSE}
  PseElfFile,
  {$ENDIF}
  PseRawFile, PseLibFile, PseObjFile,PseFile;

function TAdditionalSection.ToBytes(): TBytes;
var
  BytesStream: TBytesStream;
begin
  BytesStream := TBytesStream.Create;
  try
    BytesStream.WriteData(FSectionMarker);
    BytesStream.WriteData(FOriginalSize);
    BytesStream.WriteData(FSectionSize);
    BytesStream.WriteData(FSectionNumberBefore);
    BytesStream.Write(FSectionData, Length(FSectionData));
    Result := BytesStream.Bytes;
  finally
    FreeAndNil(BytesStream);
  end;
end;

procedure TAdditionalSection.FromBytes(Data: TBytes);
var
  BytesStream: TBytesStream;
begin
  BytesStream := TBytesStream.Create(Data);
  try
    BytesStream.ReadData(FSectionMarker);
    BytesStream.ReadData(FOriginalSize);
    BytesStream.ReadData(FSectionSize);
    BytesStream.ReadData(FSectionNumberBefore);
    SetLength(FSectionData, FSectionSize - SECTION_DATA_DIFF_SIZE);
    BytesStream.Read(FSectionData, Length(FSectionData));
  finally
    FreeAndNil(BytesStream);
  end;
end;

{ TAddSectionManiputlator }

constructor TAddSectionManiputlator.Create(ExeFilePath: string);
begin
  FAdditionalSectionCollection := TCollection.Create(TAdditionalSection);
  if not FileExists(ExeFilePath) then
    raise EFileNotFoundException.Create(ExeFilePath);
  try
    {other program(logwriter i.e.) modifying}
    FExeFileStream := TFileStream.Create(ExeFilePath, fmOpenReadWrite or
      fmShareDenyNone);
  except
    on E: Exception do
      {read sections list from itself.}
      FExeFileStream := TFileStream.Create(ExeFilePath, fmOpenRead or fmShareDenyNone);
  end;
  ReadSections();
end;

procedure TAddSectionManiputlator.ReadSections();
var
  Marker: DWORD;
  ReadingSectionSize, RealSectionOffset: integer;
  Data: TBytes;
  NewSection: TAdditionalSection;
  ImageSize: DWORD;
  pseFile: TPseFile;
begin
  pseFile := TPseFile.GetInstance(FExeFileStream, False);
  ImageSize :=pseFile.GetSizeOfFileImage();
  FExeFileStream.Seek(ImageSize, soBeginning);
  FExeFileStream.ReadData(Marker);
  {no marker - no sections.}
  if Marker <> ADDITION_SECTION_MARKER then
    exit;
  ReadingSectionSize := 0;
  repeat
    FExeFileStream.Seek(SizeOf(Dword), soCurrent);
    {reading section size with marker}
    FExeFileStream.ReadData(ReadingSectionSize);
    {updating position at sectionsize without marker(already located it).}
    RealSectionOffset := -SECTION_DATA_DIFF_SIZE + SizeOf(Dword);
    FExeFileStream.Seek(RealSectionOffset, soCurrent);
    {reading section data}
    SetLength(Data, ReadingSectionSize);
    FExeFileStream.ReadBuffer(Data, ReadingSectionSize);
    {construct new section}
    NewSection := TAdditionalSection(FAdditionalSectionCollection.Add);
    NewSection.FromBytes(Data);
    {reading new marker value}
    Marker := 0;
    if (FExeFileStream.ReadData(Marker) = 0) then
      exit;
  until (Marker = ADDITION_SECTION_MARKER) and
    (FExeFileStream.Position < FExeFileStream.Size);
end;


destructor TAddSectionManiputlator.Destroy;
begin
  FreeAndNil(FAdditionalSectionCollection);
  FreeAndNil(FExeFileStream);
  inherited Destroy;
end;

function TAddSectionManiputlator.WriteDataSection(Data: string): boolean;
begin
  Result := WriteDataSection(TEncoding.UTF8.GetBytes(Data));
end;

function TAddSectionManiputlator.WriteDataSection(Data: TBytes): boolean;
var
  CurrentSectionNumber: integer;
  NewSection: TAdditionalSection;
  Count: integer;
  SectionBytes: TBytes;
  DataLength: Dword;
begin
  Result := False;
  CurrentSectionNumber := FAdditionalSectionCollection.Count;
  NewSection := TAdditionalSection(FAdditionalSectionCollection.Add);
  NewSection.SectionData := Data;
  NewSection.SectionNumberBefore := CurrentSectionNumber;
  DataLength := Length(Data);
  writeln('data length to write:' + IntToStr(DataLength));
  NewSection.SectionSize := DataLength + SECTION_DATA_DIFF_SIZE;
  NewSection.OriginalSize := FExeFileStream.Size;
  NewSection.SectionMarker := ADDITION_SECTION_MARKER;
  FExeFileStream.Seek(0, soEnd);
  SectionBytes := NewSection.ToBytes();
  Count := FExeFileStream.Write(@SectionBytes[0], NewSection.SectionSize);
  writeln('data length written: ' + IntToStr(Count));
  Result := Count = NewSection.SectionSize;
end;

function TAddSectionManiputlator.ReadSectionAt(IndexBefore: integer): TAdditionalSection;
begin
  Result := nil;
  if (IndexBefore >= 0) and (IndexBefore < FAdditionalSectionCollection.Count) then
    Result := TAdditionalSection(FAdditionalSectionCollection.Items[IndexBefore]);
end;

function TAddSectionManiputlator.ReadSectionDataAt(IndexBefore: integer): string;
var
  SectionBytes: TBytes;
begin
  Result := '';
  SectionBytes := ReadSectionDataAsBytesAt(IndexBefore);
  if SectionBytes <> nil then
    Result := TEncoding.UTF8.GetString(sectionBytes);
end;

function TAddSectionManiputlator.ReadSectionDataAsBytesAt(IndexBefore: integer): TBytes;
var
  Section: TAdditionalSection;
begin
  Result := nil;
  Section := ReadSectionAt(IndexBefore);
  if (Section <> nil) then
    Result := Section.SectionData;
end;

initialization
{$IFDEF MSWINDOWS}
  TPseFile.RegisterFile(TPseMzFile);
  TPseFile.RegisterFile(TPseNeFile);
  TPseFile.RegisterFile(TPsePeFile);
{$ELSE}
  TPseFile.RegisterFile(TPseElfFile);
{$ENDIF}
  TPseFile.RegisterFile(TPseObjFile);
  TPseFile.RegisterFile(TPseRawFile);
end.
