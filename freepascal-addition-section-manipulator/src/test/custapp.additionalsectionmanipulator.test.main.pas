unit custapp.additionalsectionmanipulator.test.main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, CustApp.AddSectionManiputlator;

type

  TAdditionalSectionManipulatorTest = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWithoutExistingFileManipulator;
  end;

implementation

procedure TAdditionalSectionManipulatorTest.TestWithoutExistingFileManipulator;
begin
  try
    TAddSectionManiputlator.Create('');
  except
    on E: EFileNotFoundException do
       writeln('can''t create section manipulator for empty or not existing file!')
  end;
end;

procedure TAdditionalSectionManipulatorTest.SetUp;
begin

end;

procedure TAdditionalSectionManipulatorTest.TearDown;
begin

end;

initialization

 // RegisterTest(TAdditionalSectionManipulatorTest);
end.

