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
  SysUtils;

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
    manipulator: IMapFileInfoManipulator;
    logger: IMapFileBasedLogger;
  begin
    CheckOptions('f', []);
    ExeFilePath := GetOptionValue('f');

    writeln(Format('try to process file at ''%s''', [ExeFilePath]));
    manipulator := TMapFileInfoManipulatorFactory.CreateMapFileInfoManipulator(
      ExeFilePath);
    if manipulator <> nil then
    begin
      writeln(format('file at ''%s'' processed. Logger section written!',
        [ExeFilePath]));
      manipulator := nil;
    end;
    {check}
    try
      manipulator := TMapFileInfoManipulatorFactory.CreateMapFileInfoManipulator(
        ExeFilePath, True);
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
