object Form1: TForm1
  Left = 195
  Top = 260
  Width = 805
  Height = 384
  Caption = 'ATTabs demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object chkFlat: TCheckBox
    Left = 72
    Top = 136
    Width = 97
    Height = 17
    Caption = 'flat tabs'
    TabOrder = 1
    OnClick = chkFlatClick
  end
  object chkShowPlus: TCheckBox
    Left = 72
    Top = 112
    Width = 153
    Height = 17
    Caption = 'show plus pseudo tab'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = chkShowPlusClick
  end
  object chkAngled: TCheckBox
    Left = 72
    Top = 160
    Width = 97
    Height = 17
    Caption = 'angled tabs'
    TabOrder = 2
    OnClick = chkAngledClick
  end
  object chkGap: TCheckBox
    Left = 72
    Top = 184
    Width = 153
    Height = 17
    Caption = 'gap between tabs'
    TabOrder = 3
    OnClick = chkGapClick
  end
  object chkVarWidth: TCheckBox
    Left = 280
    Top = 112
    Width = 97
    Height = 17
    Caption = 'var width'
    TabOrder = 4
    OnClick = chkVarWidthClick
  end
  object chkMultiline: TCheckBox
    Left = 280
    Top = 136
    Width = 97
    Height = 17
    Caption = 'multi-line'
    TabOrder = 5
    OnClick = chkMultilineClick
  end
end
