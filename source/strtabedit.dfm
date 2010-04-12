object StrTabEditDlg: TStrTabEditDlg
  Left = 0
  Top = 0
  Caption = 'String Table editor'
  ClientHeight = 206
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 168
    Width = 312
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 103
      Top = 0
      Width = 209
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BtnOk: TButton
        Left = 8
        Top = 8
        Width = 89
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object BtnCancel: TButton
        Left = 112
        Top = 8
        Width = 89
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object StringGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 312
    Height = 168
    Align = alClient
    DefaultRowHeight = 18
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goTabs]
    TabOrder = 1
    OnKeyDown = StringGridKeyDown
  end
end
