unit CustApp.test.selfcheckingapp;

{$mode objfpc}{$H+}

interface

uses consoletestrunner, DOM;

type

  { TSelfCheckingTestRunner }

  TSelfCheckingTestRunner = class(TTestRunner)
  strict private
  const
    NUMBER_TEST_RUN_TESTS_ATTRIBUTE_NAME = 'NumberOfRunTests';
    NUMBER_TEST_IGNORED_TESTS_ATTRIBUTE_NAME = 'NumberOfIgnoredTests';
    NUMBER_TEST_FAILURES_ATTRIBUTE_NAME = 'NumberOfFailures';
    NUMBER_TEST_ERRORS_ATTRIBUTE_NAME = 'NumberOfErrors';
    TEST_LISTING_ELEMENT_XPATH = '/TestResults/TestListing';
    TEST_SUITE_ELEMENT_NAME = 'TestSuite';
    ROOT_TEST_SUITE_ELEMENT_PATH =
{      TEST_LISTING_ELEMENT_XPATH +} '/*[1]/*[1]/*[1]';// + TEST_SUITE_ELEMENT_NAME;
    EACH_CLASS_TEST_SUITE_ELEMENT_PATH =
      ROOT_TEST_SUITE_ELEMENT_PATH + '/' + TEST_SUITE_ELEMENT_NAME;
    ERROR_TEST_RESULT_HAVE_WRONG_FORMAT = 'ERROR: Test results have wrong format!';
    ERROR_HAS_NO_TEST = 'ERROR: Application has no test!';
  private
    function ValidateTestResult(DocumentElement: TDOMElement): boolean;
    function ValidateTestListing(DocumentElement: TDOMElement): boolean;
  protected
    procedure DoRun; override;
  end;

implementation

{ TSelfCheckingTestRunner }
uses xmltestreport, SysUtils, XPath, fpcunitreport;

procedure TSelfCheckingTestRunner.DoRun;
var
  writer: TCustomResultsWriter;
  documentElement: TDOMElement;
begin
  inherited DoRun;
  writer := GetResultsWriter;
  if not (writer is TXmlResultsWriter) then
  begin
    writeln('ERROR: Test results can be only in xml format!');
    FreeAndNil(writer);
    ExitCode := 1;
    exit;
  end;
  documentElement := TXmlResultsWriter(writer).Document.DocumentElement;
  if not ValidateTestResult(documentElement) then
  begin
    FreeAndNil(writer);
    ExitCode := 1;
    exit;
  end;
  if not ValidateTestListing(documentElement) then
  begin
    FreeAndNil(writer);
    ExitCode := 1;
    exit;
  end;
  FreeAndNil(writer);
end;

function TSelfCheckingTestRunner.ValidateTestResult(DocumentElement:
  TDOMElement): boolean;
var
  TestListingExpressionValue: TXPathVariable;
  TestListingNodeSet: TNodeSet;
begin
  Result := False;
  TestListingExpressionValue :=
    EvaluateXPathExpression(TEST_LISTING_ELEMENT_XPATH, DocumentElement);
  try
    TestListingNodeSet := (TestListingExpressionValue as TXPathNodeSetVariable).Value;
    if TestListingNodeSet.Count > 1 then
    begin
      writeln(ERROR_TEST_RESULT_HAVE_WRONG_FORMAT);
      exit;
    end;
    if TestListingNodeSet.Count = 0 then
    begin
      writeln(ERROR_HAS_NO_TEST);
      exit;
    end;
    Result := True;
  finally
    FreeAndNil(TestListingExpressionValue);
  end;
end;

function TSelfCheckingTestRunner.ValidateTestListing(DocumentElement:
  TDOMElement): boolean;
var
  TestSuiteExpressionValue: TXPathVariable;
  TestSuiteNodeSet: TNodeSet;
  node:TDOMNode;
begin
  Result := False;
  TestSuiteExpressionValue :=
    EvaluateXPathExpression(ROOT_TEST_SUITE_ELEMENT_PATH, DocumentElement);
  node:=DocumentElement.FirstChild.FirstChild.FirstChild;
  writeln(node.LocalName);
  try
    TestSuiteNodeSet := (TestSuiteExpressionValue as TXPathNodeSetVariable).Value;
    if TestSuiteNodeSet.Count > 1 then
    begin
      writeln(ERROR_TEST_RESULT_HAVE_WRONG_FORMAT);
      exit;
    end;
    if TestSuiteNodeSet.Count = 0 then
    begin
      writeln(ERROR_HAS_NO_TEST);
      exit;
    end;

    Result := True;
  finally
    FreeAndNil(TestSuiteExpressionValue);
  end;
end;

end.
