unit spkt_Const;

{$mode delphi}

(*******************************************************************************
*                                                                              *
*  Plik: spkt_Const.pas                                                        *
*  Opis: Sta³e wykorzystywane do obliczania geometrii toolbara                 *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*******************************************************************************)

interface

procedure SpkInitLayoutConsts(FromDPI: Integer; ToDPI: Integer = 0);
function SpkScaleX(Size: Integer; FromDPI: Integer; ToDPI: Integer = 0): integer;
function SpkScaleY(Size: Integer; FromDPI: Integer; ToDPI: Integer = 0): integer;

const
  // ****************
  // *** Elements ***
  // ****************

  LARGEBUTTON_DROPDOWN_FIELD_SIZE = 29;
  LARGEBUTTON_GLYPH_MARGIN = 1;
  LARGEBUTTON_CAPTION_HMARGIN = 3;
  LARGEBUTTON_MIN_WIDTH = 24;
  LARGEBUTTON_RADIUS = 4;
  LARGEBUTTON_BORDER_SIZE = 2;
  LARGEBUTTON_CHEVRON_HMARGIN = 4;
  LARGEBUTTON_CAPTION_TOP_RAIL = 45;
  LARGEBUTTON_CAPTION_BOTTOM_RAIL = 58;

  SMALLBUTTON_GLYPH_WIDTH = 16; //was: 20; //16;
  SMALLBUTTON_BORDER_WIDTH = 2;
  SMALLBUTTON_HALF_BORDER_WIDTH = 1;
  SMALLBUTTON_PADDING = 2;
  SMALLBUTTON_DROPDOWN_WIDTH = 11;
  SMALLBUTTON_RADIUS = 4;

  // ***********************
  // *** Tab page layout ***
  // ***********************

  /// <summary>Maximum area height that can be used by an element</summary>
  MAX_ELEMENT_HEIGHT = 67;

  /// <summary>Maximum row height</summary>
  PANE_ROW_HEIGHT = 22;

  /// <summary>Single row top margin</summary>
  PANE_ONE_ROW_TOPPADDING = 22;
  /// <summary>Single row bottom margin</summary>
  PANE_ONE_ROW_BOTTOMPADDING = 23;

  /// <summary>Space between rows in a double row layout</summary>
  PANE_TWO_ROWS_VSPACER = 7;
  /// <summary>Double row layout top margin</summary>
  PANE_TWO_ROWS_TOPPADDING = 8;
  /// <summary>Double row layout bottom margin</summary>
  PANE_TWO_ROWS_BOTTOMPADDING = 8;

  /// <summary>Space between rows in triple row layout</summary>
  PANE_THREE_ROWS_VSPACER = 0;
  /// <summary>Triple row layout top margin</summary>
  PANE_THREE_ROWS_TOPPADDING = 0;
  /// <summary>Triple row layout bottom margin</summary>
  PANE_THREE_ROWS_BOTTOMPADDING = 1;

  /// <summary>Pane left padding, space between left pane border and left element border</summary>
  PANE_LEFT_PADDING = 2;
  /// <summary>Pane right padding, space between right pane border and right element border</summary>
  PANE_RIGHT_PADDING = 2;
  /// <summary>Space between two columns inside the pane</summary>
  PANE_COLUMN_SPACER = 4;
  /// <summary>Space between groups on a row in pane</summary>
  PANE_GROUP_SPACER = 4;


  // *******************
  // *** Pane layout ***
  // *******************

  /// <summary>Pane caption height</summary>
  PANE_CAPTION_HEIGHT = 15;
  /// <summary>Pane corner radius</summary>
  PANE_CORNER_RADIUS = 3;
  /// <summary>Pane border size.</summary>
  /// <remarks>Do not change?</remarks>
  PANE_BORDER_SIZE = 2;
  /// <summary>Half width of pane border?</summary>
  /// <remarks>Do not change?</remarks>
  PANE_BORDER_HALF_SIZE = 1;
  /// <summary>Pane caption horizontal padding</summary>
  PANE_CAPTION_HMARGIN = 6;


  // ************
  // *** Tabs ***
  // ************

  /// <summary>Tab corner radius</summary>
  TAB_CORNER_RADIUS = 4;
  /// <summary>Tab page left margin</summary>
  TAB_PANE_LEFTPADDING = 2;
  /// <summary>Tab page right margin</summary>
  TAB_PANE_RIGHTPADDING = 2;
  /// <summary>Tab page top margin</summary>
  TAB_PANE_TOPPADDING = 2;
  /// <summary>Tab page bottom margin</summary>
  TAB_PANE_BOTTOMPADDING = 1;
  /// <summary>Space between panes</summary>
  TAB_PANE_HSPACING = 3;
  /// <summary>Tab border size</summary>
  TAB_BORDER_SIZE = 1;


  // ***************
  // *** Toolbar ***
  // ***************

  /// <summary>Pane padding?</summary>
  TOOLBAR_BORDER_WIDTH = 1;
  TOOLBAR_CORNER_RADIUS = 3;
  /// <summary>Tab caption height</summary>
  TOOLBAR_TAB_CAPTIONS_HEIGHT = 22;
  /// <summary>Tab caption horizontal padding</summary>
  TOOLBAR_TAB_CAPTIONS_TEXT_HPADDING = 4;
  /// <summary>Min tab caption width</summary>
  TOOLBAR_MIN_TAB_CAPTION_WIDTH = 32;

var
  // ****************
  // *** Elements ***
  // ****************
  LargeButtonDropdownFieldSize: Integer;
  LargeButtonGlyphMargin: Integer;
  LargeButtonCaptionHMargin: Integer;
  LargeButtonMinWidth: Integer;
  LargeButtonRadius: Integer;
  LargeButtonBorderSize: Integer;
  LargeButtonChevronHMargin: Integer;
  LargeButtonCaptionTopRail: Integer;
  LargeButtonCaptionButtomRail: Integer;

  SmallButtonGlyphWidth: Integer;
  SmallButtonBorderWidth: Integer;
  SmallButtonHalfBorderWidth: Integer;
  SmallButtonPadding: Integer;
  SmallButtonDropdownWidth: Integer;
  SmallButtonRadius: Integer;
  SmallButtonMinWidth: Integer;


  // ***********************
  // *** Tab page layout ***
  // ***********************

  /// <summary>Maximum area height that can be used by an element</summary>
  MaxElementHeight: Integer;

  /// <summary>Maximum row height</summary>
  PaneRowHeight: Integer;
  PaneFullRowHeight: Integer;

  /// <summary>Single row top margin</summary>
  PaneOneRowTopPadding: Integer;
  /// <summary>Single row bottom margin</summary>
  PaneOneRowBottomPadding: Integer;

  /// <summary>Space between rows in a double row layout</summary>
  PaneTwoRowsVSpacer: Integer;
  /// <summary>Double row layout top margin</summary>
  PaneTwoRowsTopPadding: Integer;
  /// <summary>Double row layout bottom margin</summary>
  PaneTwoRowsBottomPadding: Integer;

  /// <summary>Space between rows in triple row layout</summary>
  PaneThreeRowsVSpacer: Integer;
  /// <summary>Triple row layout top margin</summary>
  PaneThreeRowsTopPadding: Integer;
  /// <summary>Triple row layout bottom margin</summary>
  PaneThreeRowsBottomPadding: Integer;

  PaneFullRowTopPadding: Integer;
  PaneFullRowBottomPadding: Integer;

  /// <summary>Pane left padding, space between left pane border and left element border</summary>
  PaneLeftPadding: Integer;
  /// <summary>Pane right padding, space between right pane border and right element border</summary>
  PaneRightPadding: Integer;
  /// <summary>Space between two columns inside the pane</summary>
  PaneColumnSpacer: Integer;
  /// <summary>Space between groups on a row in pane</summary>
  PaneGroupSpacer: Integer;


  // *******************
  // *** Pane layout ***
  // *******************

  /// <summary>Pane caption height</summary>
  PaneCaptionHeight: Integer;
  /// <summary>Pane corner radius</summary>
  PaneCornerRadius: Integer;
  /// <summary>Pane border size</summary>
  /// <remarks>Do not change?</remarks>
  PaneBorderSize: Integer;
  /// <summary>Half width of pane border?</summary>
  /// <remarks>Do not change?</remarks>
  PaneBorderHalfSize: Integer;
  /// <summary>Height of pane</summary>
  PaneHeight: Integer;
  /// <summary>Pane caption horizontal padding</summary>
  PaneCaptionHMargin: Integer;


  // ************
  // *** Tabs ***
  // ************

  /// <summary>Tab corner radius</summary>
  TabCornerRadius: Integer;
  /// <summary>Tab page left margin</summary>
  TabPaneLeftPadding: Integer;
  /// <summary>Tab page right margin/summary>
  TabPaneRightPadding: Integer;
  /// <summary>Tab page top margin</summary>
  TabPaneTopPadding: Integer;
  /// <summary>Tab page bottom margin</summary>
  TabPaneBottomPadding: Integer;
  /// <summary>Space between panes</summary>
  TabPaneHSpacing: Integer;
  /// <summary>Tab border size</summary>
  TabBorderSize: Integer;
  /// <summary>Tab height</summary>
  TabHeight: Integer;


  // ***************
  // *** Toolbar ***
  // ***************

  /// <summary>Pane padding?</summary>
  ToolbarBorderWidth: Integer;
  ToolbarCornerRadius: Integer;
  /// <summary>Tab caption height</summary>
  ToolbarTabCaptionsHeight: Integer;
  /// <summary>Tab caption horizontal padding</summary>
  ToolbarTabCaptionsTextHPadding: Integer;
  ToolbarMinTabCaptionWidth: Integer;
  /// <summary>Toolbar total height</summary>
  ToolbarHeight: Integer;


const
  DPI_AWARE = true;


implementation

uses
  Graphics, LCLType;

procedure SpkInitLayoutConsts(FromDPI: Integer; ToDPI: Integer = 0);
begin

  if not(DPI_AWARE) then
    ToDPI := FromDPI;

  {$IfDef Darwin}
    ToDPI := FromDPI; //macOS raster scales by itself
  {$EndIf}

  LargeButtonDropdownFieldSize := SpkScaleX(LARGEBUTTON_DROPDOWN_FIELD_SIZE, FromDPI, ToDPI);
  LargeButtonGlyphMargin := SpkScaleX(LARGEBUTTON_GLYPH_MARGIN, FromDPI, ToDPI);
  LargeButtonCaptionHMargin := SpkScaleX(LARGEBUTTON_CAPTION_HMARGIN, FromDPI, ToDPI);
  LargeButtonMinWidth := SpkScaleX(LARGEBUTTON_MIN_WIDTH, FromDPI, ToDPI);
  LargeButtonRadius := LARGEBUTTON_RADIUS;
  LargeButtonBorderSize := SpkScaleX(LARGEBUTTON_BORDER_SIZE, FromDPI, ToDPI);
  LargeButtonChevronHMargin := SpkScaleX(LARGEBUTTON_CHEVRON_HMARGIN, FromDPI, ToDPI);
  LargeButtonCaptionTopRail := SpkScaleY(LARGEBUTTON_CAPTION_TOP_RAIL, FromDPI, ToDPI);
  LargeButtonCaptionButtomRail := SpkScaleY(LARGEBUTTON_CAPTION_BOTTOM_RAIL, FromDPI, ToDPI);

  SmallButtonGlyphWidth := SpkScaleX(SMALLBUTTON_GLYPH_WIDTH, FromDPI, ToDPI);
  SmallButtonBorderWidth := SpkScaleX(SMALLBUTTON_BORDER_WIDTH, FromDPI, ToDPI);
  SmallButtonHalfBorderWidth := SpkScaleX(SMALLBUTTON_HALF_BORDER_WIDTH, FromDPI, ToDPI);
  SmallButtonPadding := SpkScaleX(SMALLBUTTON_PADDING, FromDPI, ToDPI);
  SmallButtonDropdownWidth := SpkScaleX(SMALLBUTTON_DROPDOWN_WIDTH, FromDPI, ToDPI);
  SmallButtonRadius := SMALLBUTTON_RADIUS;
  SmallButtonMinWidth := 2 * SmallButtonPadding + SmallButtonGlyphWidth;

  MaxElementHeight := SpkScaleY(MAX_ELEMENT_HEIGHT, FromDPI, ToDPI);
  PaneRowHeight := SpkScaleY(PANE_ROW_HEIGHT, FromDPI, ToDPI);
  PaneFullRowHeight := 3 * PaneRowHeight;
  PaneOneRowTopPadding := SpkScaleY(PANE_ONE_ROW_TOPPADDING, FromDPI, ToDPI);
  PaneOneRowBottomPadding := SpkScaleY(PANE_ONE_ROW_BOTTOMPADDING, FromDPI, ToDPI);
  PaneTwoRowsVSpacer := SpkScaleY(PANE_TWO_ROWS_VSPACER, FromDPI, ToDPI);
  PaneTwoRowsTopPadding := SpkScaleY(PANE_TWO_ROWS_TOPPADDING, FromDPI, ToDPI);
  PaneTwoRowsBottomPadding := SpkScaleY(PANE_TWO_ROWS_BOTTOMPADDING, FromDPI, ToDPI);
  PaneThreeRowsVSpacer := SpkScaleY(PANE_THREE_ROWS_VSPACER, FromDPI, ToDPI);
  PaneThreeRowsTopPadding := SpkScaleY(PANE_THREE_ROWS_TOPPADDING, FromDPI, ToDPI);
  PaneThreeRowsBottomPadding := SpkScaleY(PANE_THREE_ROWS_BOTTOMPADDING, FromDPI, ToDPI);
  PaneFullRowTopPadding := PaneThreeRowsTopPadding;
  PaneFullRowBottomPadding := PaneThreeRowsBottomPadding;
  PaneLeftPadding := SpkScaleX(PANE_LEFT_PADDING, FromDPI, ToDPI);
  PaneRightPadding := SpkScaleX(PANE_RIGHT_PADDING, FromDPI, ToDPI);
  PaneColumnSpacer := SpkScaleX(PANE_COLUMN_SPACER, FromDPI, ToDPI);
  PaneGroupSpacer := SpkScaleX(PANE_GROUP_SPACER, FromDPI, ToDPI);

  PaneCaptionHeight := SpkScaleY(PANE_CAPTION_HEIGHT, FromDPI, ToDPI);
  PaneCornerRadius := PANE_CORNER_RADIUS;
  PaneBorderSize := SpkScaleX(PANE_BORDER_SIZE, FromDPI, ToDPI);
  PaneBorderHalfSize := SpkScaleX(PANE_BORDER_HALF_SIZE, FromDPI, ToDPI);
  PaneHeight := MaxElementHeight + PaneCaptionHeight + 2 * PaneBorderSize;
  PaneCaptionHMargin := SpkScaleX(PANE_CAPTION_HMARGIN, FromDPI, ToDPI);

  TabCornerRadius := TAB_CORNER_RADIUS;
  TabPaneLeftPadding := SpkScaleX(TAB_PANE_LEFTPADDING, FromDPI, ToDPI);
  TabPaneRightPadding := SpkScaleX(TAB_PANE_RIGHTPADDING, FromDPI, ToDPI);
  TabPaneTopPadding := SpkScaleY(TAB_PANE_TOPPADDING, FromDPI, ToDPI);
  TabPaneBottomPadding := SpkScaleY(TAB_PANE_BOTTOMPADDING, FromDPI, ToDPI);
  TabPaneHSpacing := SpkScaleX(TAB_PANE_HSPACING, FromDPI, ToDPI);
  TabBorderSize := SpkScaleX(TAB_BORDER_SIZE, FromDPI, ToDPI);
  TabHeight := PaneHeight + TabPaneTopPadding + TabPaneBottomPadding + TabBorderSize;

  ToolbarBorderWidth := SpkScaleX(TOOLBAR_BORDER_WIDTH, FromDPI, ToDPI);
  ToolbarCornerRadius := TOOLBAR_CORNER_RADIUS;
  ToolbarTabCaptionsHeight := SpkScaleY(TOOLBAR_TAB_CAPTIONS_HEIGHT, FromDPI, ToDPI);
  ToolbarTabCaptionsTextHPadding := SpkScaleX(TOOLBAR_TAB_CAPTIONS_TEXT_HPADDING, FromDPI, ToDPI);
  ToolbarMinTabCaptionWidth := SpkScaleX(TOOLBAR_MIN_TAB_CAPTION_WIDTH, FromDPI, ToDPI);
  ToolbarHeight := ToolbarTabCaptionsHeight + TabHeight;

  // scaling radius if not square
  if LargeButtonRadius > 1 then
    LargeButtonRadius := SpkScaleX(LargeButtonRadius, FromDPI, ToDPI);

  if SmallButtonRadius > 1 then
    SmallButtonRadius := SpkScaleX(SmallButtonRadius, FromDPI, ToDPI);

  if PaneCornerRadius > 1 then
    PaneCornerRadius := SpkScaleX(PaneCornerRadius, FromDPI, ToDPI);

  if TabCornerRadius > 1 then
    TabCornerRadius := SpkScaleX(TabCornerRadius, FromDPI, ToDPI);

  if ToolbarCornerRadius > 1 then
    ToolbarCornerRadius := SpkScaleX(ToolbarCornerRadius, FromDPI, ToDPI);
end;

function SpkScaleX(Size: Integer; FromDPI: Integer; ToDPI: Integer): integer;
begin
  if ToDPI = 0 then
    ToDPI := ScreenInfo.PixelsPerInchX;

  if (not DPI_AWARE) or (ToDPI = FromDPI) then
    Result := Size
  else
    begin
      if (ToDPI/FromDPI <= 1.5) and (Size = 1) then
        Result := 1 //maintaining 1px on 150% scale for crispness
      else
        Result := MulDiv(Size, ToDPI, FromDPI);
    end;

end;

function SpkScaleY(Size: Integer; FromDPI: Integer; ToDPI: Integer): integer;
begin
  if ToDPI = 0 then
    ToDPI := ScreenInfo.PixelsPerInchY;

  if (not DPI_AWARE) or (ToDPI = FromDPI) then
    Result := Size
  else
    begin
      if (ToDPI/FromDPI <= 1.5) and (Size = 1) then
        Result := 1 //maintaining 1px on 150% scale for crispness
      else
        Result := MulDiv(Size, ToDPI, FromDPI);
    end;

end;


initialization

{$IFDEF DEBUG}
// Sprawdzanie poprawnoœci

// £uk du¿ego przycisku
assert(LARGEBUTTON_RADIUS * 2 <= LARGEBUTTON_DROPDOWN_FIELD_SIZE);

// Tafla, wersja z jednym wierszem
assert(PANE_ROW_HEIGHT +
       PANE_ONE_ROW_TOPPADDING +
       PANE_ONE_ROW_BOTTOMPADDING <= MAX_ELEMENT_HEIGHT);

// Tafla, wersja z dwoma wierszami
assert(2*PANE_ROW_HEIGHT +
       PANE_TWO_ROWS_TOPPADDING +
       PANE_TWO_ROWS_VSPACER +
       PANE_TWO_ROWS_BOTTOMPADDING <= MAX_ELEMENT_HEIGHT);

// Tafla, wersja z trzema wierszami
assert(3*PANE_ROW_HEIGHT +
       PANE_THREE_ROWS_TOPPADDING +
       2*PANE_THREE_ROWS_VSPACER +
       PANE_THREE_ROWS_BOTTOMPADDING <= MAX_ELEMENT_HEIGHT);
{$ENDIF}

end.
