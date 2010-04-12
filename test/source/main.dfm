object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Thelma unit testing'
  ClientHeight = 488
  ClientWidth = 621
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    621
    488)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 443
    Width = 621
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BtnTest: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 33
      Caption = 'Run the tests'
      TabOrder = 0
      OnClick = BtnTestClick
    end
  end
  object MemTestLog: TMemo
    Left = 192
    Top = 5
    Width = 421
    Height = 440
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'This application tests the algorithms of thelma. When '
      'you '
      'click the button, a series of tests begins: predefined'
      'input, whose output is known, is given to the '
      'algorithms, '
      'and the output is checked. This way, when you or someone '
      'else changes a unit in thelma, you run the test suite '
      'again and you know you haven'#39't broken something that '
      'worked.'
      ''
      'Whenever you add something in thelma, add a series of '
      'tests here. Whenever you find a bug which were not '
      'trapped by the tests, make the tests more complete.'
      ''
      'This testing suite started in the middle of thelma'#39's '
      'life, and many units may still not have tests.'
      '')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 5
    Width = 178
    Height = 440
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Units'
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 280
      Width = 113
      Height = 26
      Caption = '(*) these tests may take a long time to complete'
      WordWrap = True
    end
    object btnCheckAll: TButton
      Left = 8
      Top = 312
      Width = 73
      Height = 25
      Caption = 'Check All'
      TabOrder = 0
      OnClick = btnCheckAllClick
    end
    object btnUncheckAll: TButton
      Left = 96
      Top = 312
      Width = 73
      Height = 25
      Caption = 'Uncheck All'
      TabOrder = 1
      OnClick = btnUncheckAllClick
    end
    object chkVerbose: TCheckBox
      Left = 8
      Top = 264
      Width = 97
      Height = 17
      Caption = 'Verbose'
      TabOrder = 2
    end
    object chklbUnits: TCheckListBox
      Left = 8
      Top = 16
      Width = 161
      Height = 242
      ItemHeight = 13
      TabOrder = 3
    end
  end
end
