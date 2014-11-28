program thelmaTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  testdates in 'testdates.pas',
  testmatrix in 'testmatrix.pas',
  teststat2 in 'teststat2.pas',
  testtsidf in 'testtsidf.pas',
  testtsevap in 'testtsevap.pas',
  testaggregate in 'testaggregate.pas',
  testgenutils in 'testgenutils.pas',
  testmontecarlo in 'testmontecarlo.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

