program custapp.additionalsectionmanipulator.test;

{$mode objfpc}{$H+}

uses
  Classes, custapp.additionalsectionmanipulator.test.main,
  CustApp.test.selfcheckingapp, testcase2;

type

  { TMyTestRunner }

  TMyTestRunner = class(TSelfCheckingTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Additional section manipulator. FPCUnit Console self check test runner';
  Application.Run;
  Application.Free;
end.
