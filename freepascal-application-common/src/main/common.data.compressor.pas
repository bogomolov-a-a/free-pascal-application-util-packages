unit common.Data.compressor;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, zstream;

type
  IDataCompressor = interface
  {Compress into gzip compressed json file serialized to json MapFileInfo object.
  @param data section data for compressing.
  @returns gzipped section data}
    function CompressDataZip(Data: TStream;
      CompressionLevel: Tcompressionlevel = Tcompressionlevel.clmax): TBytesStream;
  {Decompress gzip compressed json file and read json string for loading map info in memory.
  @param Data compressed data.
  @returns ungzipped section data.}
    function DeCompressDataZip(Data: TStream): TBytesStream;
  end;

  { TDataCompressorFactory }

  TDataCompressorFactory = class
    class function CreateDataCompressor(): IDataCompressor;
  end;

implementation

uses  zipper;

const
  BUFFER_SIZE = 4096;

type

  { TDataCompressor }

  TDataCompressor = class(TInterfacedObject, IDataCompressor)
  strict private
    procedure CheckDataArgument(Data: TStream);
  public
    function CompressDataZip(Data: TStream;
      CompressionLevel: Tcompressionlevel = Tcompressionlevel.clmax): TBytesStream;
    function DeCompressDataZip(Data: TStream): TBytesStream;
  end;

procedure TDataCompressor.CheckDataArgument(Data: TStream);
begin
  if (Data = nil) then
    raise EArgumentNilException.Create('Data can''t be nil ');
  if (Data.Size = 0) then
    raise EArgumentOutOfRangeException.Create(
      'Length of Data argument can''t be zero. ');
end;

function TDataCompressor.CompressDataZip(Data: TStream;
  CompressionLevel: Tcompressionlevel = Tcompressionlevel.clmax): TBytesStream;
var
  Deflator: TDeflater;
  DecompressedStream: TStream;
begin
  CheckDataArgument(Data);
  Result := TBytesStream.Create();
  Data.Position := 0;
  Deflator := TDeflater.Create(Data, Result, BUFFER_SIZE);
  try
    Deflator.CompressionLevel := CompressionLevel;
    Deflator.Compress;
    DecompressedStream := DeCompressDataZip(Result);
  finally
    FreeAndNil(DecompressedStream);
    FreeAndNil(Deflator);
    Result.Position := 0;
    Data.Position := 0;
  end;
end;

function TDataCompressor.DeCompressDataZip(Data: TStream): TBytesStream;
var
  Inflater: TInflater;
begin
  CheckDataArgument(Data);
  Data.Position := 0;
  Result := TBytesStream.Create;
  Inflater := TInflater.Create(Data, Result, BUFFER_SIZE);
  try
    try
      Inflater.DeCompress;
    except
      FreeAndNil(Result);
    end;
  finally
    FreeAndNil(Inflater);
    Data.Position := 0;
    Result.Position := 0;
  end;
end;

{ TDataCompressorFactory }

class function TDataCompressorFactory.CreateDataCompressor(): IDataCompressor;
begin
  Result := TDataCompressor.Create;
end;


end.
