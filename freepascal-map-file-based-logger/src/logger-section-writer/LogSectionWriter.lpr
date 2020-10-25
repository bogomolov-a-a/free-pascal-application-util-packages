program LogSectionWriter;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
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
  begin
    { add your program here }
    CheckOptions('f', []);
    ExeFilePath := GetOptionValue('f');

    writeln(Format('try to process file at ''%s''', [ExeFilePath]));
    manipulator := TMapFileInfoManipulatorFactory.CreateMapFileInfoManipulator(
      ExeFilePath);
    writeln(format('file at ''%s'' processed. Logger section written!',
      [ExeFilePath]));
    manipulator := nil;
    {check}
    manipulator := TMapFileInfoManipulatorFactory.CreateMapFileInfoManipulator(
      ExeFilePath);
    TMapFileBasedLoggerFactory.CreateLoggerFrom(manipulator);
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
  Application := TLogDataSectionWriter.Create(nil);
  Application.Title := 'Log Data Section Writer';
  Application.Run;
  Application.Free;
end.
