object F_CustomizeMenu: TF_CustomizeMenu
  Left = 314
  Top = 209
  Caption = 'F_CustomizeMenu'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object FWClose1: TFWClose
      Left = 520
      Top = 0
      Height = 30
      Caption = 'Fermer'
      TabOrder = 0
      Align = alRight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2171169
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 30
    Width = 600
    Height = 370
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 240
      Top = 0
      Width = 5
      Height = 340
    end
    object vt_MainMenu: TVirtualStringTree
      Left = 245
      Top = 0
      Width = 355
      Height = 340
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Height = 22
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 0
      OnClick = vt_MainMenuClick
      OnGetText = vt_MainMenuGetText
      OnGetImageIndex = vt_MainMenuGetImageIndex
      OnInitNode = vt_MainMenuInitNode
      Columns = <
        item
          Position = 0
          Width = 351
          WideText = 'Menu complet'
        end>
    end
    object Panel3: TPanel
      Left = 0
      Top = 340
      Width = 600
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object FWDelete: TFWDelete
        Left = 0
        Top = 0
        Width = 105
        Height = 30
        Caption = 'Supprimer'
        Enabled = False
        TabOrder = 0
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2171169
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        OnClick = FWDeleteClick
      end
      object FWInsert: TFWInsert
        Left = 519
        Top = 0
        Width = 81
        Height = 30
        Caption = 'Ajouter'
        Enabled = False
        TabOrder = 1
        Align = alRight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2171169
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        OnClick = FWInsertClick
      end
      object Panel4: TPanel
        Left = 105
        Top = 0
        Width = 31
        Height = 30
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
      end
      object ch_ajouteravant: TJvXPCheckbox
        Left = 358
        Top = 0
        Width = 161
        Height = 30
        Caption = 'Ajouter avant'
        TabOrder = 3
        Align = alRight
      end
    end
    object vt_MenuIni: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 240
      Height = 340
      Align = alLeft
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Height = 22
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 2
      OnClick = vt_MenuIniClick
      OnGetText = vt_MainMenuGetText
      OnGetImageIndex = vt_MainMenuGetImageIndex
      OnInitNode = vt_MainMenuInitNode
      Columns = <
        item
          Position = 0
          Width = 236
          WideText = 'Menu personnalis'#233
        end>
    end
  end
  object OnFormInfoIni: TOnFormInfoIni
    SaveForm = [sfSavePos, sfSaveSizes]
    Left = 136
    Top = 232
  end
end
