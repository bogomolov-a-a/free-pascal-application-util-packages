unit common.Data.Mapper;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  IDataMapper = interface
    function SerializeTo(DataObject: TObject; AClass: TClass): string;
    function DeSerializeFrom(AData: string; AClass: TClass): TObject;
    function DeSerializeFrom(AData: TBytes; AEncoding: TEncoding;
      AClass: TClass): TObject;
    function DeSerializeFrom(AData: string; AEncoding: TEncoding;
      AClass: TClass): TObject;
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
    procedure CheckArguments(AData: string; AEncoding: TEncoding; AClass: TClass);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function SerializeTo(DataObject: TObject; AClass: TClass): string;
    function DeSerializeFrom(AData: string; AClass: TClass): TObject;
    function DeSerializeFrom(AData: TBytes; AEncoding: TEncoding;
      AClass: TClass): TObject;
    function DeSerializeFrom(AData: string; AEncoding: TEncoding;
      AClass: TClass): TObject;
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
  AClass: TClass);
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
    [jsoLowerPropertyNames, jsoCheckEmptyDateTime, jsoDateTimeAsString];
  FJsonDeStreamer := TJSONDeStreamer.Create(nil);
end;

destructor TJsonDataMapper.Destroy;
begin
  FreeAndNil(FJsonDeStreamer);
  FreeAndNil(FJsonStreamer);
  inherited Destroy;
end;

function TJsonDataMapper.SerializeTo(DataObject: TObject; AClass: TClass): string;
begin
  Result := '';
  if not (DataObject is AClass) then
    raise EJSON.CreateFmt('DataObject can''t be serialized as %s', [AClass.ClassName]);
  Result := FJsonStreamer.ObjectToJSONString(DataObject);
end;

function TJsonDataMapper.DeSerializeFrom(AData: string; AClass: TClass): TObject;
begin
  CheckArguments(AData, TEncoding.UTF8, AClass);
  Result := DeSerializeFrom(TEncoding.UTF8.GetBytes(AData),TEncoding.UTF8, AClass);
end;

function TJsonDataMapper.DeSerializeFrom(AData: TBytes; AEncoding: TEncoding;
  AClass: TClass): TObject;
begin
  if AData = nil then
    raise EArgumentNilException.Create('AData array can''t be nil.');
  if Length(AData) = 0 then
    raise EArgumentException.Create('AData is empty!');
  CheckArguments('EMPTY STRING', TEncoding.UTF8, AClass);
  Result := AClass.Create;
  FJsonDeStreamer.JSONToObject(AEncoding.GetString(AData), Result);
end;

function TJsonDataMapper.DeSerializeFrom(AData: string; AEncoding: TEncoding;
  AClass: TClass): TObject;
begin
  CheckArguments(AData, AEncoding, AClass);
  Result := DeSerializeFrom(AEncoding.GetBytes(AData), AEncoding, AClass);
end;

end.
