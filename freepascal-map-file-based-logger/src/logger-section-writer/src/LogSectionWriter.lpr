program LogSectionWriter;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
 {Widestring manager needed for widestring support}
  cwstring,
                          {$ENDIF} {$IFDEF WINDOWS}
  Windows, {for setconsoleoutputcp}
                          {$ENDIF}

  Classes,
  CustApp,
  { you can add units after this }
  MapFileBasedLogger.Logger,
  SysUtils,
  MapFileBasedLogger.BasicTypes;

const
  FILE_NAME_OPTION = 'f';
  METHOD_SIGNATURE_FILTER_OPTION = 'method-signature-filter';
type


  { TLogDataSectionWriter }

  TLogDataSectionWriter = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TLogDataSectionWriter }

  procedure TLogDataSectionWriter.DoRun;
  var
    ExeFilePath: string;
    MethodSignatureFilter: string;
    manipulator: IMapFileInfoManager;
    logger: IMapFileBasedLogger;
  begin
    manipulator := nil;
    logger := nil;
    ExeFilePath := EmptyStr;
    CheckOptions(FILE_NAME_OPTION, [METHOD_SIGNATURE_FILTER_OPTION]);
    ExeFilePath := GetOptionValue(FILE_NAME_OPTION);
    MethodSignatureFilter := GetOptionValue(METHOD_SIGNATURE_FILTER_OPTION);
    writeln(Format('try to process file at ''%s''', [ExeFilePath]));
    manipulator := TMapFileInfoManagerFactory.CreateMapFileInfoManager(
      ExeFilePath, MethodSignatureFilter);
    if manipulator <> nil then
    begin
      writeln(format('file at ''%s'' processed. Logger section written!',
        [ExeFilePath]));
      manipulator := nil;
    end;
    {check}
    try
      manipulator := TMapFileInfoManagerFactory.CreateMapFileInfoManager(
        ExeFilePath, '', True);
      logger := TMapFileBasedLoggerFactory.CreateLoggerFrom(manipulator);
    except
      on e: Exception do
      begin
        writeln(e.Message);
        exitCode := -1;
      end;
    end;
    logger := nil;
    manipulator := nil;
    // stop program loop
    Terminate;
  end;

  constructor TLogDataSectionWriter.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
    ExceptionExitCode := -1;
  end;

var
  Application: TLogDataSectionWriter;
begin
  {$IFDEF WINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
  Application := TLogDataSectionWriter.Create(nil);
  Application.Title := 'LogDataSectionWriter';
  Application.Run;
  Application.Free;
end.
