object plLCDScreenLinesEditorForm: TplLCDScreenLinesEditorForm
  Left = 439
  Height = 346
  Top = 338
  Width = 540
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'plLCDScreenLinesEditorForm'
  ClientHeight = 346
  ClientWidth = 540
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '6.2'
  object Panel1: TPanel
    Left = 0
    Height = 138
    Top = 179
    Width = 540
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ClientHeight = 138
    ClientWidth = 540
    TabOrder = 0
    object Panel6: TPanel
      Left = 2
      Height = 23
      Top = 2
      Width = 536
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ClientHeight = 23
      ClientWidth = 536
      TabOrder = 0
      object PreviewLabel: TLabel
        Left = 4
        Height = 17
        Top = 3
        Width = 49
        Caption = 'Preview'
        ParentColor = False
      end
    end
    object Panel3: TPanel
      Left = 2
      Height = 111
      Top = 25
      Width = 536
      Align = alClient
      BevelOuter = bvLowered
      ClientHeight = 111
      ClientWidth = 536
      TabOrder = 1
      object PreviewSB: TScrollBox
        Left = 1
        Height = 109
        Top = 1
        Width = 534
        Align = alClient
        ClientHeight = 88
        ClientWidth = 530
        TabOrder = 0
        object PreviewLCD: TplLCDScreen
          Left = 0
          Height = 61
          Top = 0
          Width = 958
          AnimationDelay = 250
          AnimationEnabled = False
          AnimationRepeating = False
          BitmapCopyMode = cmTransparent
          BitmapAnimMode = amDynamic
          BitmapXOffset = 0
          BitmapYOffset = 0
          BorderSpace = 1
          BorderStyle = bsNone
          Color = clBtnFace
          DisplayMode = dmText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Intensity = 127
          LinesAnimMode = amStatic
          LinesXOffset = 0
          LinesYOffset = 0
          PixelHeight = 2
          PixelOff = 11184810
          PixelShape = psSquare
          PixelSize = pix2x2
          PixelSpacing = 1
          PixelWidth = 2
          SpecialEffects = [spBlink, spBold, spInverse, spItalic, spUnderline, spStrike]
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Height = 29
    Top = 317
    Width = 540
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ClientHeight = 29
    ClientWidth = 540
    TabOrder = 1
    object CancelButton: TButton
      Left = 249
      Height = 22
      Top = 4
      Width = 75
      Cancel = True
      Caption = 'Cancel'
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ModalResult = 2
      ParentFont = False
      TabOrder = 0
    end
    object OkButton: TButton
      Left = 328
      Height = 22
      Top = 4
      Width = 75
      Caption = 'OK'
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ModalResult = 1
      ParentFont = False
      TabOrder = 1
    end
  end
  object Panel4: TPanel
    Left = 0
    Height = 179
    Top = 0
    Width = 540
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ClientHeight = 179
    ClientWidth = 540
    TabOrder = 2
    object LinesMemo: TMemo
      Left = 2
      Height = 152
      Top = 25
      Width = 505
      Align = alClient
      MaxLength = 250
      OnChange = LinesMemoChange
      OnKeyUp = LinesMemoKeyUp
      OnMouseMove = LinesMemoMouseMove
      OnMouseUp = LinesMemoMouseUp
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object SpBPanel: TPanel
      Left = 507
      Height = 152
      Top = 25
      Width = 31
      Align = alRight
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ClientHeight = 152
      ClientWidth = 31
      TabOrder = 1
      object BoldSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'Bold Tag <b>'
        Top = 4
        Width = 23
        Color = clBtnFace
        Enabled = False
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000430B0000430B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006666666666666666666000006666
          6666666666666660000066666666666666666660000066666666666666666660
          0000660000600006660006600000600000600000600000600000600600600600
          6006006000006000006006006006006000006600006006006006666000006666
          0060000060060060000066000060000660000060000060000660006660000060
          0000666666600066660006600000666666600066666666600000666666666666
          666666600000666666666666666666600000666666666666666666600000
        }
        Layout = blGlyphTop
        NumGlyphs = 0
        OnClick = BoldSpBClick
        ShowHint = True
        ParentShowHint = False
      end
      object ItalicSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'Italic Tag <i>'
        Top = 23
        Width = 23
        Color = clBtnFace
        Enabled = False
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000330B0000330B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006666666666666666666000006666
          6666666666666660000066666666666666666660000066666666666666666660
          0000660006660006660006600000600600600600600600600000600660600660
          6006666000006600000600660600666000006666000660600600600000006660
          0066600066600060000066666666660666666660000066666666666066666660
          0000666666666600666666600000666666666666666666600000666666666666
          666666600000666666666666666666600000666666666666666666600000
        }
        Layout = blGlyphTop
        NumGlyphs = 0
        OnClick = ItalicSpBClick
        ShowHint = True
        ParentShowHint = False
      end
      object StrikeSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'Strike Tag <s>'
        Top = 42
        Width = 23
        Color = clBtnFace
        Enabled = False
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000230B0000230B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006666666666666666666000006666
          6666666666666660000066666666666666666660000066666666666666666660
          0000660000660006660006600000600600600600600600600000666666666666
          6666666000006666666666666666666000006666006006006006006000006600
          0660000666000660000066666660066666666660000066666660066666666660
          0000666666000666666666600000666666666666666666600000666666666666
          666666600000666666666666666666600000666666666666666666600000
        }
        Layout = blGlyphTop
        NumGlyphs = 0
        OnClick = StrikeSpBClick
        ShowHint = True
        ParentShowHint = False
      end
      object UnderlineSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'UnderlineTag <u>'
        Top = 61
        Width = 23
        Color = clBtnFace
        Enabled = False
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000330B0000330B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006666666666666666666000006666
          6666666666666660000066000000000000000660000066666666666666666660
          0000660000660006660006600000600600600600600600600000600600600600
          6006666000006600006006006006666000006666006006006006006000006600
          0660000666000660000066666660066666666660000066666660066666666660
          0000666666000666666666600000666666666666666666600000666666666666
          666666600000666666666666666666600000666666666666666666600000
        }
        Layout = blGlyphTop
        NumGlyphs = 0
        OnClick = UnderlineSpBClick
        ShowHint = True
        ParentShowHint = False
      end
      object InverseSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'Inverse Tag <inv>'
        Top = 80
        Width = 23
        Color = clBtnFace
        Enabled = False
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000230B0000230B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006666666666666666666000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000006666006660006660000000066066066066066066000000066066066066
          0660000000000066660660660660000000000000660660660660660000000066
          6006666000666000000000000006600000000000000000000006600000000000
          0000000000666000000000000000000000000000000000000000000000000000
          000000000000666666666666666666600000666666666666666666600000
        }
        Layout = blGlyphTop
        NumGlyphs = 0
        OnClick = InverseSpBClick
        ShowHint = True
        ParentShowHint = False
      end
      object BlinkSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'Blinking Tag <bl>'
        Top = 99
        Width = 23
        Color = clBtnFace
        Enabled = False
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000430B0000430B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006666666660666666666000006066
          6666666666666060000066066666606666660660000066666666666666666660
          0000660000660006660006600000600600600600600600600000600600600600
          6006666000006600006006006006666000006666006006006006006000006600
          0660000666000660000066666660066666666660000066666660066666666660
          0000660666000666666606600000606666666066666660600000666666666666
          666666600000666666666066666666600000666666666666666666600000
        }
        Layout = blGlyphTop
        NumGlyphs = 0
        OnClick = BlinkSpBClick
        ShowHint = True
        ParentShowHint = False
      end
      object DeleteTagSpB: TSpeedButton
        Left = 4
        Height = 20
        Hint = 'Remove Tags'
        Top = 126
        Width = 23
        Color = clBtnFace
        Flat = True
        Glyph.Data = {
          5A010000424D5A01000000000000760000002800000013000000130000000100
          040000000000E4000000430B0000430B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
          6666666000006666666666666666666000006996666666666666996000006699
          6666666666099660000066699666666660990060000066669966666669960060
          0000666669966666996600600000666006990009960000600000660060699699
          6006006000006600666009006006006000006600666990996006006000006600
          6699660990060060000060000996000699000660000066009966666669966660
          0000666996666666669966600000669966666666666996600000699666666666
          666699600000666666666666666666600000666666666666666666600000
        }
        Layout = blGlyphBottom
        NumGlyphs = 0
        OnClick = DeleteLCDTagSpBClick
        ShowHint = True
        ParentShowHint = False
      end
    end
    object Panel5: TPanel
      Left = 2
      Height = 23
      Top = 2
      Width = 536
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ClientHeight = 23
      ClientWidth = 536
      TabOrder = 2
      object BrutLabel: TLabel
        Left = 4
        Height = 16
        Top = 3
        Width = 87
        Caption = 'Brut Text Lines'
        ParentColor = False
      end
    end
  end
end
