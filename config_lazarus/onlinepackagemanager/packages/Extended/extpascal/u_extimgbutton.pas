unit u_extimgbutton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{$IFDEF UseRuntime}
  Ext, ExtPascal, ExtForm,
  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,
  ExtMenu,  ExtState;

type
  {$M+}
  TExtPanel_Tab = TExtPanel;
  TExtFormTextField_Grid = TExtFormTextField;
  TExtFormNumberField_Grid = TExtFormNumberField;
  TExtFormDateField_Grid = TExtFormDateField;
  TExtFormTimeField_Grid = TExtFormTimeField;
  TExtFormCheckbox_Grid = TExtFormCheckbox;
  TExtFormComboBox_Grid = TExtFormComboBox;
  {$M-}

{$ELSE}
  ExtP_Design_Ctrls;
{$ENDIF}
type
  TExtImgButton = class(TExtButton)
  private
    FBtnImg:String;
    FGlyphSize: Integer;
  protected
    {$IFDEF UseRuntime}
    htm
    {$ELSE}
    {$ENDIF}
  public
    property Glyph: String read FBtnImg write FBtnImg;
    property GlyphSize : Integer read FGlyphSize write FGlyphSize;
  end;


implementation

end.

