object LCDAnimatorCodeEditorForm: TLCDAnimatorCodeEditorForm
  Left = 408
  Height = 468
  Top = 229
  Width = 533
  BorderStyle = bsSizeToolWin
  Caption = 'TplLCDAnimator Code Editor'
  ClientHeight = 468
  ClientWidth = 533
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCloseQuery = FormCloseQuery
  Position = poScreenCenter
  LCLVersion = '6.2'
  object PanelBottom: TPanel
    Left = 0
    Height = 108
    Top = 360
    Width = 533
    Align = alBottom
    ClientHeight = 108
    ClientWidth = 533
    TabOrder = 0
    object SynthaxPanel: TPanel
      Left = 2
      Height = 25
      Top = 36
      Width = 462
      BevelOuter = bvNone
      ClientHeight = 25
      ClientWidth = 462
      Color = clGray
      Font.Color = clActiveCaption
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      object SynthaxLabel2: TLabel
        Left = 120
        Height = 16
        Top = 4
        Width = 326
        Alignment = taCenter
        Caption = ' Ops_1(Param_1)  Ops_2(Param_2)  ...   Ops_x(Param_x)   '
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsItalic]
        ParentColor = False
        ParentFont = False
      end
      object SynthaxLabel1: TLabel
        Left = 118
        Height = 20
        Top = 2
        Width = 332
        Alignment = taCenter
        Caption = '[                   ;                   ;  ;                   ;]'
        Font.Color = clLime
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object SynthaxLabel3: TLabel
        Left = 16
        Height = 17
        Top = 3
        Width = 93
        Caption = 'Line Synthax:'
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
    end
    object CancelButton: TButton
      Left = 120
      Height = 25
      Top = 72
      Width = 75
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object OkButton: TButton
      Left = 24
      Height = 25
      Top = 72
      Width = 75
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
    end
    object PanelEdit: TPanel
      Left = 1
      Height = 26
      Top = 1
      Width = 531
      Align = alTop
      ClientHeight = 26
      ClientWidth = 531
      TabOrder = 3
      object EditLine: TEdit
        Left = 1
        Height = 24
        Top = 1
        Width = 446
        Align = alClient
        Color = clWhite
        TabOrder = 0
      end
      object ctUpdate: TButton
        Left = 447
        Height = 24
        Hint = 'Click To update the Line'
        Top = 1
        Width = 83
        Align = alRight
        Caption = 'Update'
        OnClick = ctUpdateClick
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
    end
    object ctLoad: TButton
      Left = 288
      Height = 25
      Hint = 'Load from file'
      Top = 70
      Width = 59
      Caption = 'Load'
      OnClick = ctLoadClick
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object ctSave: TButton
      Left = 352
      Height = 25
      Top = 70
      Width = 64
      Caption = 'Save'
      OnClick = ctSaveClick
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
  end
  object PanelTop: TPanel
    Left = 0
    Height = 104
    Top = 0
    Width = 533
    Align = alTop
    ClientHeight = 104
    ClientWidth = 533
    TabOrder = 1
    object Label1: TLabel
      Left = 304
      Height = 14
      Top = 19
      Width = 31
      Caption = 'Param'
      ParentColor = False
    end
    object InsertButton: TButton
      Left = 416
      Height = 25
      Top = 33
      Width = 97
      Caption = 'Insert Line'
      OnClick = InsertLineClick
      TabOrder = 0
    end
    object HorzScrollButton: TButton
      Tag = 1
      Left = 7
      Height = 25
      Top = 8
      Width = 90
      Caption = 'HorzScroll('
      OnClick = xxButtonClick
      TabOrder = 1
    end
    object SetIntensityButton: TButton
      Tag = 1
      Left = 97
      Height = 25
      Top = 8
      Width = 90
      Caption = 'SetIntensity('
      OnClick = xxButtonClick
      TabOrder = 2
    end
    object ResetDisplayButton: TButton
      Tag = 1
      Left = 187
      Height = 25
      Top = 8
      Width = 90
      Caption = 'ResetDisplay('
      OnClick = xxButtonClick
      TabOrder = 3
    end
    object GotoLineButton: TButton
      Tag = 1
      Left = 187
      Height = 25
      Top = 33
      Width = 90
      Caption = 'GotoLine('
      OnClick = xxButtonClick
      TabOrder = 4
    end
    object AnimationDelayButton: TButton
      Tag = 1
      Left = 97
      Height = 25
      Top = 33
      Width = 90
      Caption = 'AnimationDelay('
      OnClick = xxButtonClick
      TabOrder = 5
    end
    object VertScrollButton: TButton
      Tag = 1
      Left = 7
      Height = 25
      Top = 33
      Width = 90
      Caption = 'VertScroll('
      OnClick = xxButtonClick
      TabOrder = 6
    end
    object LineBeginningButton: TButton
      Left = 7
      Height = 25
      Top = 72
      Width = 90
      Caption = '['
      OnClick = xxButtonClick
      TabOrder = 7
    end
    object InstructionEndingButton: TButton
      Left = 97
      Height = 25
      Top = 72
      Width = 90
      Caption = '); '
      OnClick = xxButtonClick
      TabOrder = 8
    end
    object LineEndingButton: TButton
      Left = 187
      Height = 25
      Top = 72
      Width = 90
      Caption = ']'
      OnClick = xxButtonClick
      TabOrder = 9
    end
    object AddButton: TButton
      Left = 416
      Height = 25
      Top = 8
      Width = 97
      Caption = 'Add Line'
      OnClick = AddLineClick
      TabOrder = 10
    end
    object DeleteButton: TButton
      Left = 416
      Height = 25
      Top = 72
      Width = 97
      Caption = 'Delete Line'
      OnClick = DeleteLineClick
      TabOrder = 11
    end
    object ctValue: TSpinEdit
      Left = 344
      Height = 23
      Top = 16
      Width = 58
      MaxValue = 2000
      TabOrder = 12
      Value = 1
    end
  end
  object Code: TListBox
    Left = 0
    Height = 256
    Top = 104
    Width = 533
    Align = alClient
    Color = 15466232
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 0
    OnClick = CodeClick
    ParentFont = False
    PopupMenu = CodeCellPUM
    TabOrder = 2
  end
  object CodeCellPUM: TPopupMenu
    left = 408
    top = 125
    object InsertMenu: TMenuItem
      Caption = 'Insert Line'
      ShortCut = 45
      OnClick = InsertLineClick
    end
    object AddMenu: TMenuItem
      Caption = 'Add Line'
      ShortCut = 8237
      OnClick = AddLineClick
    end
    object DeleteMenu: TMenuItem
      Caption = 'Delete Line'
      ShortCut = 8238
      OnClick = DeleteLineClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object DeleteAllMenu: TMenuItem
      Caption = 'Delete All'
      ShortCut = 16430
      OnClick = DeleteAllClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object SyntaxAnalysisMenu: TMenuItem
      Caption = 'Syntax Analysis'
      ShortCut = 16467
      OnClick = SyntaxAnalysisMenuClick
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object MenuItem2: TMenuItem
      Caption = 'Insert Sample'
      object MenuItem6: TMenuItem
        Tag = 1
        Caption = 'Cycling'
        OnClick = xxSampleClick
      end
      object MenuItem5: TMenuItem
        Tag = 2
        Caption = 'Flashing'
        OnClick = xxSampleClick
      end
      object MenuItem4: TMenuItem
        Tag = 3
        Caption = 'Gravity'
        OnClick = xxSampleClick
      end
      object MenuItem7: TMenuItem
        Tag = 4
        Caption = 'Waving'
        OnClick = xxSampleClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Title = 'Open Animation text file'
    DefaultExt = '.txt'
    Filter = 'Animation text file (*.txt)|txt'
    left = 240
    top = 125
  end
  object SaveDialog1: TSaveDialog
    Title = 'Save Animation to text file'
    DefaultExt = '.txt'
    Filter = 'Animation text file (*.txt)|txt'
    Options = [ofOverwritePrompt, ofEnableSizing, ofViewDetail]
    left = 320
    top = 125
  end
end
