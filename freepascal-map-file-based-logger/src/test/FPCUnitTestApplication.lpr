program FPCUnitTestApplication;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms,fpcunit,GuiTestRunner,
  mainUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUITestRunner, TestRunner);
  Application.Run;
end.

