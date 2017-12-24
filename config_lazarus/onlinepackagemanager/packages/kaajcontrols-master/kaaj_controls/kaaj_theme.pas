unit kaaj_theme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_main, fpg_base, fpg_stylemanager, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGraphics;

type

  { TKaajStyle }

  TKaajStyle = class(TfpgStyle)
    constructor Create; override;
    procedure _DrawButtonString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
      AText: string; AEnabled: boolean = True);
    procedure _DrawEditString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
      AText: string; AEnabled: boolean = True);
    procedure DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); override;
    function GetControlFrameBorders: TRect; override;
    procedure DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
      ARaised: boolean = True); override;
    procedure DrawDirectionArrow(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
      direction: TArrowDirection); override;
    procedure DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
      AText: string; AEnabled: boolean = True); override;
    procedure DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect); override;
    procedure DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
      AFlags: TfpgButtonFlags); override;
    function GetButtonBorders: TRect; override;
    function GetButtonShift: TPoint; override;
    function HasButtonHoverEffect: boolean; override;
    procedure DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect;
      ABackgroundColor: TfpgColor); override;
    procedure DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect;
      AFlags: TfpgMenuItemFlags); override;
    procedure DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect); override;
  end;

  function ARGB(const R, G, B, A: byte): TfpgColor; overload;
  function ARGB(const R, G, B: byte): TfpgColor; overload;
  procedure AddImages();

Const
  stdimg_checkboxes : Array[0..2601] of byte = (
      66, 77, 42, 10,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0, 65,  0,  0,  0, 13,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
     244,  9,  0,  0,196, 14,  0,  0,196, 14,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,  0, 40, 40, 40, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40,
      40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40,
      40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      40, 40, 40,  0, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40,
      40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,  0, 40, 40, 40, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40,
      40, 40, 40, 40, 99, 99, 99, 99, 99, 99,216,216,216,216,216,216, 99,
      99, 99, 99, 99, 99, 99, 99, 99,216,216,216,216,216,216, 99, 99, 99,
      99, 99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56,
      56, 56, 56,  0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56, 56, 56,
      56,  0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40,
      40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 40, 40, 40,  0, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99,
      99, 99,216,216,216,216,216,216,216,216,216, 99, 99, 99,216,216,216,
     216,216,216,216,216,216, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40,
      40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56,  0,  0,  0,  0,  0,
       0,  0,  0,  0, 56, 56, 56,  0,  0,  0,  0,  0,  0,  0,  0,  0, 56,
      56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,  0, 40, 40, 40,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40,
      40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99,216,216,216,
     216,216,216,216,216,216,216,216,216,216,216,216, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56,
      56, 56, 56, 56, 56, 56, 56,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,
      40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 40, 40, 40,  0, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99,216,216,216,216,216,216,216,216,
     216, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40,
      40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56,  0,  0,  0,  0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,  0, 40, 40,
      40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99,216,216,
     216,216,216,216,216,216,216,216,216,216,216,216,216, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56,
      56, 56, 56, 56, 56, 56, 56, 56,  0,  0,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40,
      40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 40, 40, 40,  0, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99, 99,
      99, 99, 99, 99,216,216,216,216,216,216,216,216,216, 99, 99, 99,216,
     216,216,216,216,216,216,216,216, 99, 99, 99, 99, 99, 99, 40, 40, 40,
      40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56,  0,  0,  0,
       0,  0,  0,  0,  0,  0, 56, 56, 56,  0,  0,  0,  0,  0,  0,  0,  0,
       0, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,  0, 40,
      40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99,216,216,216,216,
     216,216, 99, 99, 99, 99, 99, 99, 99, 99, 99,216,216,216,216,216,216,
      99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40,
      56, 56, 56, 56, 56, 56,  0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56,
      56, 56, 56, 56,  0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56, 40,
      40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 40, 40, 40,  0, 40, 40, 40, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40,
      40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,  0,
      40, 40, 40,145,145,145,145,145,145,145,145,145,145,145,145,145,145,
     145,145,145,145,145,145,145,145,145,145,145,145,145,145,145,145,145,
     145,145, 40, 40, 40, 40, 40, 40,145,145,145,145,145,145,145,145,145,
     145,145,145,145,145,145,145,145,145,145,145,145,145,145,145,145,145,
     145,145,145,145,145,145,145, 40, 40, 40, 40, 40, 40, 51, 51, 51, 51,
      51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51,
      51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 40, 40, 40, 40, 40,
      40, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51,
      51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51,
      40, 40, 40, 40, 40, 40, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51,
      51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51,
      51, 51, 51, 51, 51, 40, 40, 40,  0, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
       0);

Const
  stdimg_radiobuttons : Array[0..2213] of byte = (
      66, 77,166,  8,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0, 60,  0,  0,  0, 12,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
     112,  8,  0,  0,196, 14,  0,  0,196, 14,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
      40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 40, 40, 40, 40, 40, 40,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40,255,  0,255,255,
       0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40,
     255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 40, 40, 40,255,  0,255,255,  0,255, 40, 40, 40, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 40, 40, 40,255,  0,255,255,  0,255, 40, 40, 40, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,255,255,  0,255, 40, 40,
      40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,255,255,  0,255,
      40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,255,255,
       0,255, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40,255,  0,
     255,255,  0,255, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40,
     255,  0,255,255,  0,255, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40,
      40, 40,255,  0,255,255,  0,255, 40, 40, 40, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 40, 40, 40,255,  0,255,255,  0,255, 40, 40, 40, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 40, 40, 40,255,  0,255, 40, 40, 40, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99,216,216,216,216,216,216, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40,
      40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,  0,  0,  0,  0,
       0,  0, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,
      40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40,
      40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99,216,
     216,216,216,216,216,216,216,216,216,216,216, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56,
      56, 56, 56,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40,
      99, 99, 99, 99, 99, 99, 99, 99, 99,216,216,216,216,216,216,216,216,
     216,216,216,216, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40,
      40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40,
      40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56,  0,  0,  0,  0,
       0,  0,  0,  0,  0,  0,  0,  0, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99,216,216,216,216,216,216, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56,  0,  0,  0,  0,  0,  0, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40, 40, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,
     255, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40,255,  0,255,
     255,  0,255, 40, 40, 40, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40,255,
       0,255,255,  0,255, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40,
      40,255,  0,255,255,  0,255, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      40, 40, 40,255,  0,255,255,  0,255, 40, 40, 40, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 40, 40, 40,255,  0,255,255,  0,255, 40, 40, 40, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 40, 40, 40,255,  0,255,255,  0,255, 40, 40, 40, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 40, 40, 40,255,  0,255,255,  0,255, 40, 40,
      40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,255,255,  0,255,
      40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,255,255,
       0,255, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,255,  0,
     255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 40, 40, 40, 40, 40, 40,255,
       0,255,255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40, 40, 40,
      40,255,  0,255,255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 40,
      40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40, 40, 40,
      40, 40, 40,255,  0,255,255,  0,255,255,  0,255,255,  0,255, 40, 40,
      40, 40, 40, 40, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 40,
      40, 40, 40, 40, 40,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255, 40, 40, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255, 40, 40, 40,
      40, 40, 40, 40, 40, 40, 40, 40, 40,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255);

implementation

function ARGB(const R, G, B, A: byte): TfpgColor; overload;
begin
  Result := B or (G shl 8) or (R shl 16) or (A shl 24);
end;

function ARGB(const R, G, B: byte): TfpgColor; overload;
begin
  Result := ARGB(R, G, B, 255);
end;

procedure AddImages;
begin
  fpgImages.AddBMP(  // 65x13 pixels. 5 images of 13x13 each.
            'sys.checkboxes',
            @stdimg_checkboxes,
      sizeof(stdimg_checkboxes));
  fpgImages.AddMaskedBMP(  // 60x12 in total.  5 images of 12x12 each.
            'sys.radiobuttons',
            @stdimg_radiobuttons,
      sizeof(stdimg_radiobuttons), 0,0);
end;

const
  CarbonBaseColors: array [0..15] of TfpgColor = (
    $FF333333, $FF191919, $FF616161,
    $FF202020, $FF474747, $FFC0C0C0,
    $FF6E6E6E, $FF3399FF, $FFEAEAEA,
    $FF2D2D2D, $FF494949, $FF24617A,
    $FF353535, $FF434343, $FF313131,
    $FF27546A);

function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
begin
  Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
end;

procedure DrawBar(FBGRA: TBGRABitmap; bounds: TRect);
var
  lCol: TBGRAPixel;
begin
  lCol := BGRA(102, 163, 226);

  DoubleGradientAlphaFill(FBGRA, bounds,
    ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
    ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
    BGRAGraphics.gdVertical, BGRAGraphics.gdVertical, BGRAGraphics.gdVertical, 0.53);

  InflateRect(bounds, -1, -1);

  DoubleGradientAlphaFill(FBGRA, bounds,
    ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
    ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
    BGRAGraphics.gdVertical, BGRAGraphics.gdVertical, BGRAGraphics.gdVertical, 0.53);
end;

{ TKaajStyle }

constructor TKaajStyle.Create;
begin
  inherited Create;
  fpgSetNamedColor(clWindowBackground, $FF535353);
  fpgSetNamedColor(clBoxColor, CarbonBaseColors[1]);
  fpgSetNamedColor(clShadow1, CarbonBaseColors[2]);
  fpgSetNamedColor(clShadow2, CarbonBaseColors[1]);
  fpgSetNamedColor(clHilite1, CarbonBaseColors[3]);
  fpgSetNamedColor(clHilite2, CarbonBaseColors[4]);
  fpgSetNamedColor(clText1, CarbonBaseColors[5]);
  fpgSetNamedColor(clText4, CarbonBaseColors[6]);
  fpgSetNamedColor(clSelection, CarbonBaseColors[7]);
  fpgSetNamedColor(clSelectionText, CarbonBaseColors[8]);
  fpgSetNamedColor(clInactiveSel, CarbonBaseColors[7]);
  fpgSetNamedColor(clInactiveSelText, CarbonBaseColors[8]);
  fpgSetNamedColor(clScrollBar, CarbonBaseColors[9]);
  fpgSetNamedColor(clButtonFace, CarbonBaseColors[0]);
  fpgSetNamedColor(clListBox, CarbonBaseColors[1]);
  fpgSetNamedColor(clGridLines, CarbonBaseColors[2]);
  fpgSetNamedColor(clGridHeader, CarbonBaseColors[0]);
  fpgSetNamedColor(clWidgetFrame, CarbonBaseColors[2]);
  fpgSetNamedColor(clInactiveWgFrame, CarbonBaseColors[10]);
  fpgSetNamedColor(clUnset, CarbonBaseColors[11]);
  fpgSetNamedColor(clMenuText, CarbonBaseColors[5]);
  fpgSetNamedColor(clMenuDisabled, CarbonBaseColors[0]);
  fpgSetNamedColor(clHintWindow, CarbonBaseColors[0]);
  fpgSetNamedColor(clGridSelection, CarbonBaseColors[7]);
  fpgSetNamedColor(clGridSelectionText, CarbonBaseColors[8]);
  fpgSetNamedColor(clGridInactiveSel, CarbonBaseColors[7]);
  fpgSetNamedColor(clGridInactiveSelText, CarbonBaseColors[8]);
  fpgSetNamedColor(clSplitterGrabBar, CarbonBaseColors[7]);
end;

function TKaajStyle.HasButtonHoverEffect: boolean;
begin
  Result := True;
end;

procedure TKaajStyle._DrawButtonString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit;
  ACanvas.SetTextColor($FF2F2F2F);
  ACanvas.DrawString(x, y - 1, AText);
  if AEnabled then
    ACanvas.SetTextColor($FFE5E5E5)
  else
    ACanvas.SetTextColor($FFAAAAAA);
  ACanvas.DrawString(x, y, AText);
end;

procedure TKaajStyle._DrawEditString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit;
  ACanvas.DrawString(x, y, AText);
end;

procedure TKaajStyle.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.DrawRectangle(r);
end;

function TKaajStyle.GetControlFrameBorders: TRect;
begin
  Result := Rect(1, 1, 1, 1);
end;

procedure TKaajStyle.DrawBevel(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
  ARaised: boolean);
begin
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.GradientFill(fpgRect(x, y, w, h), clUnset, $FF27546A, fpg_base.gdVertical);
  ACanvas.SetColor(clHilite1);
  ACanvas.DrawRectangle(x, y, w, h);
end;

procedure TKaajStyle.DrawDirectionArrow(ACanvas: TfpgCanvas;
  x, y, w, h: TfpgCoord; direction: TArrowDirection);
begin
  inherited DrawDirectionArrow(ACanvas, x, y, w, h, direction);
end;

procedure TKaajStyle.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect;
  ABackgroundColor: TfpgColor);
begin
  ACanvas.Clear(clWindowBackground);
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom - 1, r.Right + 1, r.Bottom - 1);
  ACanvas.SetColor(clBoxColor);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right + 1, r.Bottom);
end;

procedure TKaajStyle.DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect;
  AFlags: TfpgMenuItemFlags);
var
  FBGRA: TBGRABitmap;
begin
  inherited DrawMenuRow(ACanvas, r, AFlags);
  FBGRA := TBGRABitmap.Create(r.Width, r.Height, BGRAPixelTransparent);
  if (mifSelected in AFlags) and not (mifSeparator in AFlags) then
    DrawBar(FBGRA, Rect(0, 0, r.Width, r.Height));
  FBGRA.Draw(ACanvas, r.Left, r.Top, False);
  FBGRA.Free;
end;

procedure TKaajStyle.DrawMenuItemSeparator(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor(clBoxColor);
  ACanvas.DrawLine(r.Left + 1, r.Top + 2, r.Right, r.Top + 2);
end;

procedure TKaajStyle.DrawString(ACanvas: TfpgCanvas; x, y: TfpgCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit;
  if AEnabled then
    ACanvas.SetTextColor($FFE5E5E5)
  else
    ACanvas.SetTextColor($FFAAAAAA);
  ACanvas.DrawString(x, y, AText);
end;

procedure TKaajStyle.DrawFocusRect(ACanvas: TfpgCanvas; r: TfpgRect);
begin
  ACanvas.SetColor($FF506FAC);
  ACanvas.SetLineStyle(1, lsSolid);
  //InflateRect(r, 1, 1);
  ACanvas.DrawRectangle(r);
end;

procedure TKaajStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
  AFlags: TfpgButtonFlags);
var
  FBGRA: TBGRABitmap;
  Width, Height: integer;
begin
  FBGRA := TBGRABitmap.Create(w, h);
  Width := w;
  Height := h;
  if not (btfDisabled in AFlags) then
  begin
    if (btfIsPressed in AFlags) then
    begin
      { Button Down }
      FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      FBGRA.Rectangle(1, 1, Width - 1, Height - 2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end
    else
    begin
      if not (btfHover in AFlags) then
      begin
        { Button Normal }
        FBGRA.GradientFill(0, 0, Width, Height, BGRA(107, 107, 107),
          BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
        FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
        FBGRA.SetHorizLine(1, 1, Width - 2, BGRA(130, 130, 130));
        FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
        { Button Focused }
        if (btfHasFocus in AFlags) then
        begin
          FBGRA.Rectangle(1, 2, Width - 1, Height - 2, BGRA(80, 111, 172), dmSet);
        end;
      end
      else
      begin
        { Button Hovered }
        FBGRA.GradientFill(0, 0, Width, Height, BGRA(132, 132, 132),
          BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
        FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
        FBGRA.SetHorizLine(1, 1, Width - 2, BGRA(160, 160, 160));
        FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
      end;
    end;
  end
  else
  begin
    { Button Disabled }
    FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
      BGRA(61, 61, 61), dmSet);
    FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
  end;
  FBGRA.Draw(ACanvas, x, y);
  FBGRA.Free;
end;

function TKaajStyle.GetButtonBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TKaajStyle.GetButtonShift: TPoint;
begin
  Result := Point(0, 0);
end;

initialization
  fpgStyleManager.RegisterClass('Kaaj', TKaajStyle);


end.
