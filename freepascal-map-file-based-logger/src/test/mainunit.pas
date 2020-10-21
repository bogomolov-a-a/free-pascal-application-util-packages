unit mainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MapFileBasedLogger.Appenders;

type

  TMapFileBasedLoggerTestCase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

(**
* @cover(TAppenderFactory.BuildConsoleAppender)
*)
procedure TMapFileBasedLoggerTestCase.TestHookUp;
begin
  TAppenderFactory.BuildConsoleAppender();
end;

procedure TMapFileBasedLoggerTestCase.SetUp;
begin

end;

procedure TMapFileBasedLoggerTestCase.TearDown;
begin

end;

initialization

  RegisterTest(TMapFileBasedLoggerTestCase);
end.
