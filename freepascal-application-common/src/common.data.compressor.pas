unit common.Data.compressor;

{$mode objfpc}{$H+}

interface

uses SysUtils, zstream;

type
  IDataCompressor = interface
  {Compress into gzip compressed json file serialized to json MapFileInfo object.
  @param data section data for compressing.
  @returns gzipped section data}
    function CompressDataGzip(Data: TBytes;
      CompressionLevel: Tcompressionlevel = Tcompressionlevel.cldefault): TBytes;
  {Decompress gzip compressed json file and read json string for loading map info in memory.
  @param Data compressed data.
  @returns ungzipped section data.}
    function DeCompressDataGzip(Data: TBytes): TBytes;
  end;

  { TDataCompressorFactory }

  TDataCompressorFactory = class
    class function CreateDataCompressor(): IDataCompressor;
  end;

implementation

uses  Classes, PasZLib;

type

  { TDataCompressor }

  TDataCompressor = class(TInterfacedObject, IDataCompressor)
  strict private
    procedure CheckDataArgument(Data: TBytes);
  public
    function CompressDataGzip(Data: TBytes;
      CompressionLevel: Tcompressionlevel = Tcompressionlevel.cldefault): TBytes;
    function DeCompressDataGzip(Data: TBytes): TBytes;
  end;

procedure TDataCompressor.CheckDataArgument(Data: TBytes);
begin
  if (Data = nil) then
    raise EArgumentNilException.Create('Data can''t be nil ');
  if (Length(Data) = 0) then
    raise EArgumentOutOfRangeException.Create(
      'Length of Data argument can''t be zero. ');
end;

function TDataCompressor.CompressDataGzip(Data: TBytes;
  CompressionLevel: Tcompressionlevel): TBytes;
var
  CompressStream: Tcompressionstream;
  TargetFileStream: TBytesStream;
  SourceStringStream: TBytesStream;
begin
  SetLength(Result, 0);
  CheckDataArgument(Data);
  SourceStringStream := TBytesStream.Create(Data);
  TargetFileStream := TBytesStream.Create();
  CompressStream := Tcompressionstream.Create(CompressionLevel, TargetFileStream);
  try
    CompressStream.SourceOwner := True;
    CompressStream.CopyFrom(SourceStringStream, SourceStringStream.Size);
    Result := TargetFileStream.Bytes;
  finally
    FreeAndNil(TargetFileStream);
    FreeAndNil(CompressStream);
    FreeAndNil(SourceStringStream);
  end;
end;

function TDataCompressor.DeCompressDataGzip(Data: TBytes): TBytes;
var
  DeCompressStream: TDeCompressionStream;
  SourceFileStream: TBytesStream;
  TargetStringStream: TBytesStream;
begin
  SetLength(Result, 0);
  CheckDataArgument(Data);
  SourceFileStream := TBytesStream.Create(Data);
  DeCompressStream := TDeCompressionStream.Create(SourceFileStream);
  TargetStringStream := TBytesStream.Create();
  try
    TargetStringStream.LoadFromStream(DeCompressStream);
    Result := TargetStringStream.Bytes;
  finally
    FreeAndNil(TargetStringStream);
    FreeAndNil(DeCompressStream);
    FreeAndNil(SourceFileStream);
  end;
end;

{ TDataCompressorFactory }

class function TDataCompressorFactory.CreateDataCompressor(): IDataCompressor;
begin
  Result := TDataCompressor.Create;
end;


end.
