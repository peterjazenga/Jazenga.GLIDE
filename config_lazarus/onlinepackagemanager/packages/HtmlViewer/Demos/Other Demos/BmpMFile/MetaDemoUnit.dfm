object Form1: TForm1
  Left = 193
  Top = 163
  Width = 879
  Height = 470
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 871
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 48
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Load File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object MakeButton: TButton
      Left = 168
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Make Metafiles'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = MakeButtonClick
    end
    object PrevButton: TButton
      Left = 520
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Previous'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = PrevButtonClick
    end
    object NextButton: TButton
      Left = 640
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Next'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = NextButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 432
    Height = 398
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BevelWidth = 2
    Caption = 'Panel2'
    TabOrder = 1
    object Viewer: THtmlViewer
      Left = 2
      Top = 2
      Width = 428
      Height = 394
      BorderStyle = htFocused
      DefBackground = clWindow
      DefFontName = 'Times New Roman'
      DefPreFontName = 'Courier New'
      HistoryMaxCount = 0
      HtOptions = [htPrintTableBackground, htPrintBackground, htPrintMonochromeBlack]
      NoSelect = False
      PrintMarginBottom = 2
      PrintMarginLeft = 2
      PrintMarginRight = 2
      PrintMarginTop = 2
      PrintScale = 1
      Align = alClient
      TabOrder = 0
    end
  end
  object ScrollBox: TScrollBox
    Left = 432
    Top = 41
    Width = 439
    Height = 398
    VertScrollBar.Visible = False
    Align = alRight
    AutoScroll = False
    Color = clWindow
    ParentColor = False
    TabOrder = 2
    object PaintBox: TPaintBox
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      OnPaint = PaintBoxPaint
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'htm'
    Filter = 'html files|*.htm;*.html|all files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 329
    Top = 66
  end
end
