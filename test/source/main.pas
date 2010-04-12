unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst;

type
  TFrmMain = class(TForm)
    Panel1: TPanel;
    MemTestLog: TMemo;
    BtnTest: TButton;
    GroupBox1: TGroupBox;
    btnCheckAll: TButton;
    btnUncheckAll: TButton;
    Label1: TLabel;
    chkVerbose: TCheckBox;
    chklbUnits: TCheckListBox;
    procedure BtnTestClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnUncheckAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.DFM}

uses testdates, testtsidf, testmontecarlo, testtsevap, ts, dates, testmatrix,
  testgenutils, teststat2, testaggregate;

type

  TTestFunction = function(Verbose: Boolean): string;

  TTest = record
    name: string;
    proc: TTestFunction;
  end;

const tests: array[1..8] of TTest = (
    (name: 'dates';          proc: testdates.test),
    (name: 'genutils';       proc: testgenutils.test),
    (name: 'matrix';         proc: testmatrix.test),
    (name: 'montecarlo (*)'; proc: testmontecarlo.test),
    (name: 'stat2';          proc: teststat2.test),
    (name: 'tsevap';         proc: testtsevap.test),
    (name: 'tsidf';          proc: testtsidf.test),
    (name: 'aggregation';    proc: testaggregate.test)
  );

procedure TFrmMain.BtnTestClick(Sender: TObject);
var
  OldCursor: TCursor;
  i, j, k: Integer;
begin
  MemTestLog.Clear;
  Application.ProcessMessages;
  OldCursor := Screen.Cursor;
  try try
    Screen.Cursor := crHourGlass;
    for i := 1 to Length(tests) do
      if chklbUnits.Checked[i-1] then
      begin
        k := MemTestLog.Lines.Count;
        j := MemTestLog.Lines.Add('Testing '+tests[i].name+'.pas...');
        MemTestLog.Lines[k] := MemTestLog.Lines[k] + ' ' +
          tests[i].proc(chkVerbose.Checked);
      end;
    MemTestLog.Lines.Add('');
    MemTestLog.Lines.Add('All tests successful.');
  finally
    Screen.Cursor := OldCursor;
  end
  except
    on E: Exception do MemTestLog.Lines.Append(E.Message);
  end;
end;

procedure TFrmMain.btnCheckAllClick(Sender: TObject);
var i: Integer;
begin
  for i  := 0 to chklbUnits.Count - 1 do
    chklbUnits.Checked[i] := True;
end;

procedure TFrmMain.btnUncheckAllClick(Sender: TObject);
var i: Integer;
begin
  for i  := 0 to chklbUnits.Count - 1 do
    chklbUnits.Checked[i] := False;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var i: Integer;
begin
  for i := 1 to Length(tests) do
  begin
    chklbUnits.Items.Add(tests[i].name);
    chklbUnits.Checked[i-1] := True;
  end;

end;

end.
