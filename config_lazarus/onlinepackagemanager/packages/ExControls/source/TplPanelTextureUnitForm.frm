object EZShadeDlg: TEZShadeDlg
  Left = 533
  Height = 304
  Top = 276
  Width = 287
  BorderStyle = bsSizeToolWin
  Caption = 'Set shaded objects'
  ClientHeight = 304
  ClientWidth = 287
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  LCLVersion = '6.0'
  object CheckListBox1: TCheckListBox
    Left = 0
    Height = 304
    Top = 0
    Width = 287
    Align = alClient
    ItemHeight = 0
    OnClickCheck = CheckListChange
    TabOrder = 0
  end
end
