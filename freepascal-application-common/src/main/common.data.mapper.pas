unit common.Data.Mapper;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
  TCreateObjectFunction = function: TPersistent;

  IDataMapper = interface
    function SerializeTo(DataObject: TPersistent; AClass: TPersistentClass): string;
    function DeSerializeFrom(AData: string; AClass: TPersistentClass;
      ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent;
    function DeSerializeFrom(AData: TBytesStream; AEncoding: TEncoding;
      AClass: TPersistentClass;
      ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent;
    function DeSerializeFrom(AData: string; AEncoding: TEncoding;
      AClass: TPersistentClass;
      ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent;
  end;


  MapperType = (mtJSON);

  { TDataMapperFactory }

  TDataMapperFactory = class
    class function CreateDataMapper(AMapperType: MapperType): IDataMapper;
  end;

implementation

uses  fpjsonrtti,
  fpjson;

type

  { TJsonDataMapper }

  TJsonDataMapper = class(TInterfacedObject, IDataMapper)
  strict  private
    FJsonStreamer: TJSONStreamer;
    FJsonDeStreamer: TJSONDeStreamer;
    procedure CheckArguments(AData: string; AEncoding: TEncoding;
      AClass: TPersistentClass);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function SerializeTo(DataObject: TPersistent; AClass: TPersistentClass): string;
    function DeSerializeFrom(AData: string; AClass: TPersistentClass;
      ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent;
      overload;
    function DeSerializeFrom(AData: TBytesStream; AEncoding: TEncoding;
      AClass: TPersistentClass;
      ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent; overload;
    function DeSerializeFrom(AData: string; AEncoding: TEncoding;
      AClass: TPersistentClass;
      ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent; overload;
  end;

{ TDataMapperFactory }

class function TDataMapperFactory.CreateDataMapper(AMapperType: MapperType): IDataMapper;
begin
  case AMapperType of
    mtJSON: Result := TJsonDataMapper.Create;
    else
      Result := nil;
  end;
end;

{ TJsonDataMapper }

procedure TJsonDataMapper.CheckArguments(AData: string; AEncoding: TEncoding;
  AClass: TPersistentClass);
begin
  if trim(AData) = '' then
    raise EArgumentException.Create('AData is empty!');
  if AEncoding = nil then
    raise EArgumentNilException.Create('AEncoding can''t be nil.');
  if AClass = nil then
    raise EArgumentNilException.Create('AClass can''t be nil.');
end;

constructor TJsonDataMapper.Create;
begin
  FJsonStreamer := TJSONStreamer.Create(nil);
  FJsonStreamer.Options := FJsonStreamer.Options +
    [jsoCheckEmptyDateTime, jsoDateTimeAsString];
  FJsonDeStreamer := TJSONDeStreamer.Create(nil);
end;

destructor TJsonDataMapper.Destroy;
begin
  FreeAndNil(FJsonDeStreamer);
  FreeAndNil(FJsonStreamer);
  inherited Destroy;
end;

function TJsonDataMapper.SerializeTo(DataObject: TPersistent;
  AClass: TPersistentClass): string;
begin
  Result := '';
  if not (DataObject is AClass) then
    raise EJSON.CreateFmt('DataObject can''t be serialized as %s', [AClass.ClassName]);
  Result := FJsonStreamer.ObjectToJSONString(DataObject);
end;

function TJsonDataMapper.DeSerializeFrom(AData: string; AClass: TPersistentClass;
  ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent;
begin
  CheckArguments(AData, TEncoding.UTF8, AClass);
  if ACollectionCreateFunction <> nil then
    Result := ACollectionCreateFunction()
  else
    Result := AClass.Create;
  FJsonDeStreamer.JSONToObject(AData, Result);
end;

function TJsonDataMapper.DeSerializeFrom(AData: TBytesStream;
  AEncoding: TEncoding; AClass: TPersistentClass;
  ACollectionCreateFunction: TCreateObjectFunction = nil): TPersistent;
begin
  if AData = nil then
    raise EArgumentNilException.Create('AData array can''t be nil.');
  if AData.size = 0 then
    raise EArgumentException.Create('AData is empty!');
  if AEncoding = nil then
    raise EArgumentException.Create('AEncoding is empty!');
  CheckArguments('EMPTY STRING', AEncoding, AClass);
  Result := DeSerializeFrom(TEncoding.UTF8.GetString(AData.Bytes),
    AEncoding, AClass, ACollectionCreateFunction);
end;

function TJsonDataMapper.DeSerializeFrom(AData: string; AEncoding: TEncoding;
  AClass: TPersistentClass; ACollectionCreateFunction: TCreateObjectFunction =
  nil): TPersistent;
begin
  CheckArguments(AData, AEncoding, AClass);
  Result := DeSerializeFrom(TEncoding.UTF8.GetString(AEncoding.GetBytes(AData)),
    AClass, ACollectionCreateFunction);
end;

end.
