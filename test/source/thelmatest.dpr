program thelmatest;

uses
  Forms,
  main in 'main.pas' {FrmMain},
  testdates in 'testdates.pas',
  testtsidf in 'testtsidf.pas',
  testmontecarlo in 'testmontecarlo.pas',
  testtsevap in 'testtsevap.pas',
  testmatrix in 'testmatrix.pas',
  testgenutils in 'testgenutils.pas',
  teststat2 in 'teststat2.pas',
  testaggregate in 'testaggregate.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
