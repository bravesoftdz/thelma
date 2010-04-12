{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** A string table property editor. }
unit strtabedit;

interface

uses Classes, grids, stdctrls, DesignIntf, DesignEditors,
  Controls, ExtCtrls, Forms;

type
  TStrTabEditDlg = class(TForm)
    Panel1: TPanel;
    StringGrid: TStringGrid;
    Panel2: TPanel;
    BtnOk: TButton;
    BtnCancel: TButton;
    procedure StringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TStringTableProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{$R *.DFM}

uses istrutils, Windows;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStringTable), nil, '', TStringTableProperty);
end;

function TStringTableProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

procedure TStringTableProperty.Edit;
var
  AStringTable: TStringTable;
  i, j: Integer;
begin
  AStringTable := nil;
  with TStrTabEditDlg.Create(Application) do
  try
    AStringTable := TStringTable.Create(1, 1);
    AStringTable.Assign(TStringTable(GetOrdValue));
    StringGrid.RowCount := AStringTable.RowCount;
    StringGrid.ColCount := AStringTable.ColCount;
    for i := AStringTable.RowCount-1 downto 0 do
      for j := AStringTable.ColCount-1 downto 0 do
        StringGrid.Cells[j, i] := AStringTable[j, i];
    ActiveControl := StringGrid;
    if ShowModal=mrOk then
    begin
      AStringTable.RowCount := StringGrid.RowCount;
      for i := AStringTable.RowCount-1 downto 0 do
        for j := AStringTable.ColCount-1 downto 0 do
          AStringTable[j, i] := StringGrid.Cells[j, i];
      SetOrdValue(Longint(AStringTable));
    end;
  finally
    AStringTable.Free;
    Free;
  end;
end;

procedure TStrTabEditDlg.StringGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i, j: Integer;
begin
  with TStringGrid(Sender) do
  begin
    if (Key = VK_DOWN) and (Row = RowCount-1) then
      RowCount := RowCount+1
    else if (Key = VK_TAB) and not (ssShift in Shift) and (Row = RowCount-1)
    and (Col = ColCount-1) then
      RowCount := RowCount+1
    else if (Key = VK_INSERT) and (ssCtrl in Shift) then
    begin
      RowCount := RowCount+1;
      for i := RowCount-1 downto Row+1 do
        for j := ColCount-1 downto 0 do
          Cells[j, i] := Cells[j, i-1];
      for j := ColCount-1 downto 0 do
        Cells[j, Row] := '';
    end else if (Key = VK_DELETE) and (ssCtrl in Shift) then
    begin
      for i := Row to RowCount-2 do
        for j := ColCount-1 downto 0 do
          Cells[j, i] := Cells[j, i+1];
      if Row=RowCount-1 then Row := Row-1;
      RowCount := RowCount-1;
    end;
  end;
end;

end.
