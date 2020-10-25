unit testcase2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestCase2= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestCase2.TestHookUp;
begin
//  Fail('Напишите ваш тест');
end;

procedure TTestCase2.SetUp;
begin

end;

procedure TTestCase2.TearDown;
begin

end;

initialization

  RegisterTest(TTestCase2);
end.

