object F_About: TF_About
  Left = 282
  Top = 193
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #192' propos'
  ClientHeight = 396
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lb_NomApli: TLabel
    Left = 192
    Top = 13
    Width = 249
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'Inconnu'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    IsControl = True
  end
  object lb_Giroux: TLabel
    Left = 528
    Top = 12
    Width = 41
    Height = 20
    Alignment = taCenter
    Caption = '2006'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object im_appli: TImage
    Left = 8
    Top = 8
    Width = 76
    Height = 32
    Center = True
  end
  object SecretPanel1: TSecretPanel
    Left = 112
    Top = 64
    Width = 297
    Height = 137
    Interval = 50
    Lines.Strings = (
      
        '----------------------------------------------------------------' +
        '-------------------------'
      'Design eXperience '#169' 2002  M. Hoffmann'
      'Copyright '#169' 1998, 1999, 2000 GJL Software - April 2000'
      'scExcelExport November 2001 Stefan Cruysberghs'
      
        '----------------------------------------------------------------' +
        '-------------------------'
      ''
      'R'#233'alisation Giroux Juin 2006'
      ''
      
        '----------------------------------------------------------------' +
        '-------------------------')
    TextStyle = bvRaised
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Visible = False
  end
  object Valider: TdxButtons
    Left = 528
    Top = 369
    Caption = '&OK'
    TabOrder = 1
    ModalResult = 1
  end
  object bt_reinit: TdxButtons
    Left = 16
    Top = 369
    Hint = 'R'#233'initialisation des tailles'
    ParentShowHint = False
    ShowHint = True
    OnClick = bt_reinitClick
    Caption = 'R'#233'initialiser'
    TabOrder = 2
  end
  object vt_Versioning: TVirtualStringTree
    Left = 0
    Top = 48
    Width = 617
    Height = 313
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsXPStyle
    PopupMenu = PopupMenu
    TabOrder = 3
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toPopupMode, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnCompareNodes = vt_VersioningCompareNodes
    OnGetText = vt_VersioningGetText
    OnInitNode = vt_VersioningInitNode
    OnMouseUp = vt_VersioningMouseUp
    Columns = <
      item
        Position = 0
        Width = 315
        WideText = 'Composantes'
      end
      item
        Position = 1
        Width = 80
        WideText = 'Version'
      end
      item
        Position = 2
        Width = 190
        WideText = 'R'#233'dacteur'
      end>
  end
  object PopupMenu: TPopupMenu
    Left = 32
    Top = 144
    object Commentaires1: TMenuItem
      Caption = 'Commentaires'
      OnClick = Commentaires1Click
    end
    object Bugsenlevs1: TMenuItem
      Caption = 'Bugs enlev'#233's'
      OnClick = Bugsenlevs1Click
    end
  end
end
