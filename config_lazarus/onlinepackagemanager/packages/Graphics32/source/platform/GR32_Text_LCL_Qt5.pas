unit GR32_Text_LCL_Qt5;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Vectorial Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
    Qt5, Qt5Objects, Types, GR32, GR32_Paths;

procedure TextToPath(Font: HFONT; Path: TCustomPath;
  const ARect: TFloatRect; const Text: WideString; Flags: Cardinal); overload;

function MeasureTextDC(DC: HDC; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect; overload;
function MeasureText(Font: HFONT; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect;

type
  TTextHinting = (thNone, thNoHorz, thHinting);

procedure SetHinting(Value: TTextHinting);
function GetHinting: TTextHinting;

const
  DT_LEFT       = 0;   //See also Window's DrawText() flags ...
  DT_CENTER     = 1;   //http://msdn.microsoft.com/en-us/library/ms901121.aspx
  DT_RIGHT      = 2;
  DT_WORDBREAK  = $10;
  DT_VCENTER    = 4;
  DT_BOTTOM     = 8;
  DT_SINGLELINE = $20;
  DT_JUSTIFY         = 3;  //Graphics32 additions ...
  DT_HORZ_ALIGN_MASK = 3;

implementation

uses
  GR32_LowLevel;

var
  UseHinting: Boolean;
  HorzStretch: Integer; // stretching factor when calling GetGlyphOutline()
  HorzStretch_Inv: single;

  VertFlip_mat2: tmat2;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

  TT_PRIM_CSPLINE = 3;

  MaxSingle   =  3.4e+38;

function PointFXtoPointF(const Point: tagPointFX): TFloatPoint;
begin
  Result.X := Point.X.Value + Point.X.Fract * FixedToFloat;
  Result.Y := Point.Y.Value + Point.Y.Fract * FixedToFloat;
end;


function GlyphOutlineToPath(Handle: HDC; Path: TCustomPath;
  DstX, MaxX, DstY: Single;
  const Glyph: Integer; out Metrics: TGlyphMetrics): Boolean;
begin

end;

procedure InternalTextToPath(DC: HDC; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal);
begin
end;

procedure TextToPath(Font: HFONT; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal); overload;
begin
end;

function MeasureTextDC(DC: HDC; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect;
begin

end;

function MeasureText(Font: HFONT; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal): TFloatRect;

begin
end;

procedure SetHinting(Value: TTextHinting);
begin
end;

function GetHinting: TTextHinting;
begin
end;

procedure InitHinting;
begin
end;


end.
