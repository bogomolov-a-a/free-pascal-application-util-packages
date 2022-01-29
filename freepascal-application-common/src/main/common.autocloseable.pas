unit common.autocloseable;

{$mode objfpc}{$H+}

interface

uses SysUtils, types;

type

  { IAutoCloseable }

  IAutoCloseable = interface
    ['{4B97C667-F135-4B54-BE5F-EE76188885A7}']
    procedure Close();
  end;

  { TAutoCloseable }

  TAutoCloseable = class abstract(TInterfacedObject, IAutoCloseable)
  strict protected
    procedure DestroyResources; virtual;
  public
    destructor Destroy; override; final;
  public
    procedure Close(); virtual; abstract;
  end;

  { IReferenceArray }

  IReferenceArray = interface
    function GetReferences(): TObjectDynArray;
  end;

  { ITriedMethodData }

  ITriedMethodData = interface
    function GetreferenceArray: IReferenceArray;
    function GetreferenceArraySize: SizeInt;
  end;

  TAutoCloseableCreatorFunction = function(ATriedData: ITriedMethodData): IAutoCloseable;
  TTriedFunction = function(AAutoCloseable: IAutoCloseable;
    ATriedData: ITriedMethodData): TObject;
  TTriedProc = procedure(AAutoCloseable: IAutoCloseable; ATriedData: ITriedMethodData);



  { ESuppressedTryExecutionException }

  ESuppressedTryExecutionException = class(Exception)
  public
    constructor Create(ASuppressedExceptionClassName: string; AMessage: string);
  end;

  { EAutocloseableRunException }

  EAutocloseableRunException = class(Exception)
  private
    FCloseableException: ESuppressedTryExecutionException;
    FRootException: ESuppressedTryExecutionException;
  public
    constructor Create(ARootException: ESuppressedTryExecutionException;
      ACloseableException: ESuppressedTryExecutionException = nil);
    destructor Destroy; override;
  public
    property RootException: ESuppressedTryExecutionException read FRootException;
    property CloseableException: ESuppressedTryExecutionException
      read FCloseableException;
  end;

  { ITriedMethodResult }

  ITriedMethodResult = interface
    function HasResult: boolean;
    function GetResult: TObject;
    function HasException: boolean;
    function GetExceptionMessage: string;
    function GetException: EAutocloseableRunException;
    procedure SuppressException;
  end;

  { TAutoCloseableExecutor }

  TAutoCloseableExecutor = class
  strict private
  public
    class function TryExecuteWithAutocloseable(AAutoCloseableCreatorFunction:
      TAutoCloseableCreatorFunction; ATriedFunction: TTriedFunction;
      AAutoCloseableCreatorFunctionObjectArray: TObjectDynArray = nil;
      ATriedFunctionObjectArray: TObjectDynArray = nil): ITriedMethodResult; static;
    class function TryExecuteWithAutocloseable(AAutoCloseableCreatorFunction:
      TAutoCloseableCreatorFunction; ATriedProc: TTriedProc;
      AAutoCloseableCreatorFunctionObjectArray: TObjectDynArray = nil;
      ATriedFunctionObjectArray: TObjectDynArray = nil): ITriedMethodResult; static;
  end;

  TDummyElement = class
  end;

implementation

type
  { TTriedMethodDataFactory }

  TTriedMethodDataFactory = class
    class function CreateTriedMethodData(AObjectArray: TObjectDynArray = nil):
      ITriedMethodData;
      static;
  end;

  { TTriedMethodData }

  TTriedMethodData = class(TInterfacedObject, ITriedMethodData)
  private
    FReferenceArray: IReferenceArray;
    FReferenceArrayCount: SizeInt;
  public
    constructor Create(AObjectArray: TObjectDynArray);
    destructor Destroy(); override;
    function GetreferenceArray: IReferenceArray;
    function GetreferenceArraySize: SizeInt;
  end;

  { TReferenceArray }

  TReferenceArray = class(TInterfacedObject, IReferenceArray)
  strict private
    FObjectArray: TObjectDynArray;
  public
    constructor Create(AObjectArray: TObjectDynArray);
    destructor Destroy(); override;
    function GetReferences(): TObjectDynArray;
  end;

  { TTriedMethodResult }

  TTriedMethodResult = class(TInterfacedObject, ITriedMethodResult)
  strict private
    FResult: TObject;
    FException: EAutocloseableRunException;
  public
    constructor Create(AResult: TObject; AException: EAutocloseableRunException);
    destructor Destroy; override;
  public
    function HasResult: boolean;
    function GetResult: TObject;
    function HasException: boolean;
    function GetExceptionMessage: string;
    function GetException: EAutocloseableRunException;
    procedure SuppressException;
  end;

{ TAutoCloseableExecutor }

class function TAutoCloseableExecutor.TryExecuteWithAutocloseable(
  AAutoCloseableCreatorFunction: TAutoCloseableCreatorFunction;
  ATriedFunction: TTriedFunction;
  AAutoCloseableCreatorFunctionObjectArray: TObjectDynArray;
  ATriedFunctionObjectArray: TObjectDynArray): ITriedMethodResult;
var
  RootException: ESuppressedTryExecutionException;
  CloseableException: ESuppressedTryExecutionException;
  RunException: EAutocloseableRunException;
  SuccessFulResult: TObject;
  CloseableObject: IAutoCloseable;
  TriedMethodData, CreatorFunctionData: ITriedMethodData;
begin
  Result := nil;
  CloseableObject := nil;
  TriedMethodData := nil;
  CreatorFunctionData := nil;
  SuccessFulResult := nil;
  RootException := nil;
  CloseableException := nil;
  RunException := nil;
  if AAutoCloseableCreatorFunction = nil then
    raise EArgumentNilException.Create('AAutoCloseableCreatorFunction');
  if ATriedFunction = nil then
    raise EArgumentNilException.Create('ATriedFunction');
  CreatorFunctionData := TTriedMethodDataFactory.CreateTriedMethodData(
    AAutoCloseableCreatorFunctionObjectArray);
  CloseableObject := AAutoCloseableCreatorFunction(CreatorFunctionData);
  if CloseableObject = nil then
  begin
    Result := TTriedMethodResult.Create(nil,
      EAutocloseableRunException.Create(ESuppressedTryExecutionException.Create(
      EArgumentNilException.ClassName, 'Autocloseable object creation failed!')));
    exit;
  end;
  CreatorFunctionData := nil;
  TriedMethodData := TTriedMethodDataFactory.CreateTriedMethodData(
    ATriedFunctionObjectArray);
  try
    try
      SuccessFulResult := ATriedFunction(CloseableObject, TriedMethodData);
    except
      on e: Exception do
      begin
        FreeAndNil(SuccessFulResult);
        RootException := ESuppressedTryExecutionException.Create(e.ClassName, e.Message);
      end;
    end;
  finally
    try
      CloseableObject.Close();
    except
      on e: Exception do
        CloseableException := ESuppressedTryExecutionException.Create(
          e.ClassName, e.Message);
    end;
    CloseableObject := nil;
    TriedMethodData := nil;
  end;
  if (RootException <> nil) or (CloseableObject <> nil) then
  begin
    RunException := EAutocloseableRunException.Create(RootException, CloseableException);
  end;
  Result := TTriedMethodResult.Create(SuccessFulResult, RunException);
end;

class function TAutoCloseableExecutor.TryExecuteWithAutocloseable(
  AAutoCloseableCreatorFunction: TAutoCloseableCreatorFunction;
  ATriedProc: TTriedProc; AAutoCloseableCreatorFunctionObjectArray: TObjectDynArray;
  ATriedFunctionObjectArray: TObjectDynArray): ITriedMethodResult;
var
  RootException: ESuppressedTryExecutionException;
  CloseableException: ESuppressedTryExecutionException;
  RunException: EAutocloseableRunException;
  SuccessFulResult: TObject;
  CloseableObject: IAutoCloseable;
  TriedMethodData, CreatorFunctionData: ITriedMethodData;
begin
  Result := nil;
  CloseableObject := nil;
  TriedMethodData := nil;
  CreatorFunctionData := nil;
  SuccessFulResult := nil;
  RootException := nil;
  CloseableException := nil;
  RunException := nil;
  if AAutoCloseableCreatorFunction = nil then
    raise EArgumentNilException.Create('AAutoCloseableCreatorFunction');
  if ATriedProc = nil then
    raise EArgumentNilException.Create('ATriedFunction');
  CreatorFunctionData := TTriedMethodDataFactory.CreateTriedMethodData(
    AAutoCloseableCreatorFunctionObjectArray);
  CloseableObject := AAutoCloseableCreatorFunction(CreatorFunctionData);
  if CloseableObject = nil then
  begin
    Result := TTriedMethodResult.Create(nil,
      EAutocloseableRunException.Create(ESuppressedTryExecutionException.Create(
      EArgumentNilException.ClassName, 'Autocloseable object creation failed!')));
    exit;
  end;
  CreatorFunctionData := nil;
  TriedMethodData := TTriedMethodDataFactory.CreateTriedMethodData(
    ATriedFunctionObjectArray);
  try
    try
      ATriedProc(CloseableObject, TriedMethodData);
    except
      on e: Exception do
      begin
        FreeAndNil(SuccessFulResult);
        RootException := ESuppressedTryExecutionException.Create(e.ClassName, e.Message);
      end;
    end;
  finally
    try
      CloseableObject.Close();
    except
      on e: Exception do
        CloseableException := ESuppressedTryExecutionException.Create(
          e.ClassName, e.Message);
    end;
    CloseableObject := nil;
    TriedMethodData := nil;
  end;
  if (RootException <> nil) or (CloseableObject <> nil) then
  begin
    RunException := EAutocloseableRunException.Create(RootException, CloseableException);
  end;
  Result := TTriedMethodResult.Create(SuccessFulResult, RunException);
end;

{ EAutocloseableRunException }

constructor EAutocloseableRunException.Create(ARootException:
  ESuppressedTryExecutionException;
  ACloseableException: ESuppressedTryExecutionException);
begin
  inherited CreateFmt(
    'Try potential danger code with autocloseable resource execution failed. Root exception message: %s',
    [ARootException.Message]);
  FRootException := ARootException;
  FCloseableException := ACloseableException;
end;

destructor EAutocloseableRunException.Destroy;
begin
  FreeAndNil(FCloseableException);
  FreeAndNil(FRootException);
  inherited Destroy;
end;

{ TTriedMethodDataFactory }

class function TTriedMethodDataFactory.CreateTriedMethodData(
  AObjectArray: TObjectDynArray): ITriedMethodData;
begin
  Result := TTriedMethodData.Create(AObjectArray);
end;

constructor TTriedMethodData.Create(AObjectArray: TObjectDynArray);
var
  Dummy: TObjectDynArray;
  TargetArray: TObjectDynArray;
begin
  if (AObjectArray <> nil) then
    TargetArray := AObjectArray
  else
  begin
    SetLength(Dummy, 1);
    Dummy[0] := TDummyElement.Create;
    TargetArray := Dummy;
  end;
  FReferenceArray := TReferenceArray.Create(TargetArray);
  FReferenceArrayCount := Length(TargetArray);
end;

destructor TTriedMethodData.Destroy();
begin
  FReferenceArray := nil;
  inherited Destroy();
end;

function TTriedMethodData.GetreferenceArray: IReferenceArray;
begin
  Result := FReferenceArray;
end;

function TTriedMethodData.GetreferenceArraySize: SizeInt;
begin
  Result := FReferenceArrayCount;
end;

{ TReferenceArray }

constructor TReferenceArray.Create(AObjectArray: TObjectDynArray);
begin
  FObjectArray := AObjectArray;
end;

destructor TReferenceArray.Destroy();
var
  i: integer;
begin
  for i := 0 to Length(FObjectArray) - 1 do
  begin
    if FObjectArray[i] is TDummyElement then
      FreeAndNil(FObjectArray[i]);
  end;
  SetLength(FObjectArray, 0);
  inherited Destroy();
end;

function TReferenceArray.GetReferences(): TObjectDynArray;
begin
  Result := FObjectArray;
end;

constructor TTriedMethodResult.Create(AResult: TObject;
  AException: EAutocloseableRunException);
begin
  FResult := AResult;
  FException := AException;
end;

destructor TTriedMethodResult.Destroy;
begin
  FreeAndNil(FResult);
  if HasException then SuppressException;
  inherited Destroy;
end;

function TTriedMethodResult.HasResult: boolean;
begin
  Result := FResult <> nil;
end;

function TTriedMethodResult.GetResult: TObject;
begin
  Result := FResult;
end;

function TTriedMethodResult.HasException: boolean;
begin
  Result := FException <> nil;
end;

function TTriedMethodResult.GetExceptionMessage: string;
begin
  Result := '';
  if HasException then
    Result := FException.Message;
end;

function TTriedMethodResult.GetException: EAutocloseableRunException;
begin
  Result := nil;
  if HasException then
    Result := FException;
end;

procedure TTriedMethodResult.SuppressException;
begin
  FreeAndNil(FException);
end;

{ ESuppressedTryExecutionException }

constructor ESuppressedTryExecutionException.Create(
  ASuppressedExceptionClassName: string; AMessage: string);
begin
  inherited CreateFmt('Suppressed exception with class name: ''%s''.Cause: %s',
    [ASuppressedExceptionClassName, AMessage]);
end;

destructor TAutoCloseable.Destroy;
begin
  DestroyResources();
  inherited Destroy;
end;

procedure TAutoCloseable.DestroyResources;
begin
  //no op by default;
end;

end.
