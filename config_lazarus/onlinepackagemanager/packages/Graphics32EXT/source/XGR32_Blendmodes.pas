
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


unit XGR32_Blendmodes;

interface

uses
  Sysutils, Classes, GR32;



type
  TBlendMode32 = (bbmNormal32,
                  bbmMultiply32,
                  bbmScreen32,
                  bbmOverlay32,
                  bbmBrightLight32,
                  bbmSoftLightXF32,
                  bbmHardLight32,
                  bbmColorDodge32,
                  bbmColorBurn32,
                  bbmDarken32,
                  bbmLighten32,
                  bbmDifference32,
                  bbmNegation32,
                  bbmExclusion32,
                  bbmHue32,
                  bbmSaturation32,
                  bbmColor32,
                  bbmLuminosity32,
                  bbmAverage32,
                  bbmInverseColorDodge32,
                  bbmInverseColorBurn32,
                  bbmSoftColorDodge32,
                  bbmSoftColorBurn32,
                  bbmReflect32,
                  bbmGlow32,
                  bbmFreeze32,
                  bbmHeat32,
                  bbmAdditive32,
                  bbmSubtractive32,
                  bbmInterpolation32,
                  bbmStamp32,
                  bbmXOR32,
                  bbmAND32,
                  bbmOR32,
                  bbmRed32,
                  bbmGreen32,
                  bbmBlue32,
                  bbmDissolve32);
                  
  {Blendmap - lookup table for Custom}
  TBlendmap = array [0..65535] of TColor32;

  {Just a wrapper to make procedures compatible - creation of object
   not neccessary, access via blendmode variable}


  TBlendMode = class
    procedure NormalBlend           (F: TColor32; var B: TColor32; M: TColor32);
    procedure MultiplyBlend         (F: TColor32; var B: TColor32; M: TColor32);
    procedure ScreenBlend           (F: TColor32; var B: TColor32; M: TColor32);
    procedure OverlayBlend          (F: TColor32; var B: TColor32; M: TColor32);
    procedure SoftLightBlend        (F: TColor32; var B: TColor32; M: TColor32);
    procedure HardLightBlend        (F: TColor32; var B: TColor32; M: TColor32);
    procedure BrightLightBlend      (F: TColor32; var B: TColor32; M: TColor32);
    procedure ColorDodgeBlend       (F: TColor32; var B: TColor32; M: TColor32);
    procedure ColorBurnBlend        (F: TColor32; var B: TColor32; M: TColor32);
    procedure DarkenBlend           (F: TColor32; var B: TColor32; M: TColor32);
    procedure LightenBlend          (F: TColor32; var B: TColor32; M: TColor32);
    procedure DifferenceBlend       (F: TColor32; var B: TColor32; M: TColor32);
    procedure NegationBlend         (F: TColor32; var B: TColor32; M: TColor32);
    procedure ExclusionBlend        (F: TColor32; var B: TColor32; M: TColor32);
    procedure HueBlend              (F: TColor32; var B: TColor32; M: TColor32);
    procedure SaturationBlend       (F: TColor32; var B: TColor32; M: TColor32);
    procedure ColorBlend            (F: TColor32; var B: TColor32; M: TColor32);
    procedure LuminosityBlend       (F: TColor32; var B: TColor32; M: TColor32);
    procedure AverageBlend          (F: TColor32; var B: TColor32; M: TColor32);
    procedure InverseColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
    procedure InverseColorBurnBlend (F: TColor32; var B: TColor32; M: TColor32);
    procedure SoftColorDodgeBlend   (F: TColor32; var B: TColor32; M: TColor32);
    procedure SoftColorBurnBlend    (F: TColor32; var B: TColor32; M: TColor32);
    procedure ReflectBlend          (F: TColor32; var B: TColor32; M: TColor32);
    procedure GlowBlend             (F: TColor32; var B: TColor32; M: TColor32);
    procedure FreezeBlend           (F: TColor32; var B: TColor32; M: TColor32);
    procedure HeatBlend             (F: TColor32; var B: TColor32; M: TColor32);
    procedure AdditiveBlend         (F: TColor32; var B: TColor32; M: TColor32);
    procedure SubtractiveBlend      (F: TColor32; var B: TColor32; M: TColor32);
    procedure InterpolationBlend    (F: TColor32; var B: TColor32; M: TColor32);
    procedure StampBlend            (F: TColor32; var B: TColor32; M: TColor32);
    procedure xorBlend              (F: TColor32; var B: TColor32; M: TColor32);
    procedure andBlend              (F: TColor32; var B: TColor32; M: TColor32);
    procedure orBlend               (F: TColor32; var B: TColor32; M: TColor32);
    procedure RedBlend              (F: TColor32; var B: TColor32; M: TColor32);
    procedure GreenBlend            (F: TColor32; var B: TColor32; M: TColor32);
    procedure BlueBlend             (F: TColor32; var B: TColor32; M: TColor32);
    procedure DissolveBlend         (F: TColor32; var B: TColor32; M: TColor32);
  end;

  function  BlendModeList: TStringList;
  procedure GetBlendModeList(AList: TStrings);
  function  GetBlendMode(Index: Integer): TPixelCombineEvent;
  function  GetBlendIndex(Mode: TPixelCombineEvent): Integer;
  function  GetBlendModeString(const Mode: TBlendMode32): string;  
  function  ARGBBlendByMode(const F, B: TColor32; const M: TColor32; const Mode: TBlendMode32): TColor32;

  function  RenderBlendmap(Blend: TPixelCombineEvent): TBlendmap;
  procedure BitmapToBlendmap(Src: TBitmap32; out Dst:TBlendmap);
  procedure BlendmapToBitmap(Src: TBlendmap; Dst: TBitmap32);

var
  Blendmode: TBlendMode;
  BlendMap : TBlendMap;

implementation

uses
  Math;

var
  SqrtTable: array [0 .. 65535] of Byte;
  x, y, I  : Integer;
  CosineTab: array [0 .. 255] of Integer;
  ProbTable: array [0..100, 0..99] of Boolean;
const
  SEmptySource = 'The source is nil';

procedure FitBitmap(var B: TBitmap32);
var Tmp: TBitmap32;
begin
 Tmp:= TBitmap32.Create;
 Tmp.SetSize(256,256);
 Tmp.Clear;
 Tmp.Draw(Rect(0,0,256,256),Rect(0,0,B.Width,B.Height),B);
 B.Assign(Tmp);
 Tmp.Free;
end;

//---Utils-----------------------------

{Note: this only works for RGB channel processing events - not HSL and such modes}
function RenderBlendmap(Blend: TPixelCombineEvent): TBlendmap;
var x,y,m,B,F: cardinal;
begin
 m:= $FF;
 for x:=0 to 255 do for y:=0 to 255 do
  begin
   F:= Color32(x,x,x);
   B:= Color32(y,y,y);
   Blend(TColor32(F),TColor32(B),TColor32(m));
   result[y + x shl 8]:= B;
  end;
end;

procedure BlendmapToBitmap(Src: TBlendmap; Dst: TBitmap32);
var x,y: cardinal;
begin
 for x:=0 to 255 do for y:=0 to 255 do Dst.Pixel[x,y]:= Src[y + x shl 8];
end;

procedure BitmapToBlendmap(Src: TBitmap32; out Dst:TBlendmap);
var x,y: cardinal;
begin
 if  Src.PixelPtr[0,0]=nil then raise Exception.Create(SEmptySource);
 if (Src.Width<>256)or(Src.Height<>256)then FitBitmap(Src);
 for x:=0 to 255 do for y:=0 to 255 do Dst[y + x shl 8]:= Src.Pixel[x,y];
end;

function BlendModeList: TStringList;
begin
  Result := TStringList.Create;
  GetBlendModeList(Result);
end;

procedure GetBlendModeList(AList: TStrings);
begin
  with AList do
  begin
    BeginUpdate();
    try
    Add('Normal');
    Add('Multiply');
    Add('Screen');
    Add('Overlay');
    Add('Soft Light');
    Add('Hard Light');
    Add('Bright Light');
    Add('Color Dodge');
    Add('Color Burn');
    Add('Darken');
    Add('Lighten');
    Add('Difference');
    Add('Negation');
    Add('Exclusion');
    Add('Hue');
    Add('Saturation');
    Add('Color');
    Add('Luminosity');
    Add('Average');
    Add('Inverse Color Dodge');
    Add('Inverse Color Burn');
    Add('Soft Color Dodge');
    Add('Soft Color Burn');
    Add('Reflect');
    Add('Glow');
    Add('Freeze');
    Add('Heat');
    Add('Additive');
    Add('Subtractive');
    Add('Interpolation');
    Add('Stamp');
    Add('XOR');
    Add('AND');
    Add('OR');
    Add('Red');
    Add('Green');
    Add('Blue');
    Add('Dissolve');
    finally
      EndUpdate;
    end;
  end;
end;

function GetBlendIndex(Mode: TPixelCombineEvent): Integer;
var
  PM, PB: Pointer;
  Tmp   : TPixelCombineEvent;
begin
  // Workaround here.. couldn't find any other way to do this... }
  PM := @Mode;
  with BlendMode do
  begin
    Tmp := @NormalBlend;            PB := @Tmp;
    if PM = PB then begin Result := 0;  Exit; end;
    Tmp := @MultiplyBlend;          PB := @Tmp;
    if PM = PB then begin Result := 1;  Exit; end;
    Tmp := @ScreenBlend;            PB := @Tmp;
    if PM = PB then begin Result := 2;  Exit; end;
    Tmp := @OverlayBlend;           PB := @Tmp;
    if PM = PB then begin Result := 3;  Exit; end;
    Tmp := @SoftLightBlend;         PB := @Tmp;
    if PM = PB then begin Result := 4;  Exit; end;
    Tmp := @HardLightBlend;         PB := @Tmp;
    if PM = PB then begin Result := 5;  Exit; end;
    Tmp := @BrightLightBlend;       PB := @Tmp;
    if PM = PB then begin Result := 6;  Exit; end;
    Tmp := @ColorDodgeBlend;        PB := @Tmp;
    if PM = PB then begin Result := 7;  Exit; end;
    Tmp := @ColorBurnBlend;         PB := @Tmp;
    if PM = PB then begin Result := 8;  Exit; end;
    Tmp := @DarkenBlend;            PB := @Tmp;
    if PM = PB then begin Result := 9;  Exit; end;
    Tmp := @LightenBlend;           PB := @Tmp;
    if PM = PB then begin Result := 10; Exit; end;
    Tmp := @DifferenceBlend;        PB := @Tmp;
    if PM = PB then begin Result := 11; Exit; end;
    Tmp := @NegationBlend;          PB := @Tmp;
    if PM = PB then begin Result := 12; Exit; end;
    Tmp := @ExclusionBlend;         PB := @Tmp;
    if PM = PB then begin Result := 13; Exit; end;
    Tmp := @HueBlend;               PB := @Tmp;
    if PM = PB then begin Result := 14; Exit; end;
    Tmp := @SaturationBlend;        PB := @Tmp;
    if PM = PB then begin Result := 15; Exit; end;
    Tmp := @ColorBlend;             PB := @Tmp;
    if PM = PB then begin Result := 16; Exit; end;
    Tmp := @LuminosityBlend;        PB := @Tmp;
    if PM = PB then begin Result := 17; Exit; end;
    Tmp := @AverageBlend;           PB := @Tmp;
    if PM = PB then begin Result := 18; Exit; end;
    Tmp := @InverseColorDodgeBlend; PB := @Tmp;
    if PM = PB then begin Result := 19; Exit; end;
    Tmp := @InverseColorBurnBlend;  PB := @Tmp;
    if PM = PB then begin Result := 20; Exit; end;
    Tmp := @SoftColorDodgeBlend;    PB := @Tmp;
    if PM = PB then begin Result := 21; Exit; end;
    Tmp := @SoftColorBurnBlend;     PB := @Tmp;
    if PM = PB then begin Result := 22; Exit; end;
    Tmp := @ReflectBlend;           PB := @Tmp;
    if PM = PB then begin Result := 23; Exit; end;
    Tmp := @GlowBlend;              PB := @Tmp;
    if PM = PB then begin Result := 24; Exit; end;
    Tmp := @FreezeBlend;            PB := @Tmp;
    if PM = PB then begin Result := 25; Exit; end;
    Tmp := @HeatBlend;              PB := @Tmp;
    if PM = PB then begin Result := 26; Exit; end;
    Tmp := @AdditiveBlend;          PB := @Tmp;
    if PM = PB then begin Result := 27; Exit; end;
    Tmp := @SubtractiveBlend;       PB := @Tmp;
    if PM = PB then begin Result := 28; Exit; end;
    Tmp := @InterpolationBlend;     PB := @Tmp;
    if PM = PB then begin Result := 29; Exit; end;
    Tmp := @StampBlend;             PB := @Tmp;
    if PM = PB then begin Result := 30; Exit; end;
    Tmp := @xorBlend;               PB := @Tmp;
    if PM = PB then begin Result := 31; Exit; end;
    Tmp := @andBlend;               PB := @Tmp;
    if PM = PB then begin Result := 32; Exit; end;
    Tmp := @orBlend;                PB := @Tmp;
    if PM = PB then begin Result := 33; Exit; end;
    Tmp := @RedBlend;               PB := @Tmp;
    if PM = PB then begin Result := 34; Exit; end;
    Tmp := @GreenBlend;             PB := @Tmp;
    if PM = PB then begin Result := 35; Exit; end;
    Tmp := @BlueBlend;              PB := @Tmp;
    if PM = PB then begin Result := 36; Exit; end;
    Tmp := @DissolveBlend;          PB := @Tmp;
    if PM = PB then begin Result := 37; Exit; end;
    Result := -1; { Unknown adress }
  end;
end; { GetBlendIndex }

function GetBlendMode(Index: Integer): TPixelCombineEvent;
begin
  with BlendMode do
  case Index of
    -1: Result := nil;
     0: Result := @NormalBlend;
     1: Result := @MultiplyBlend;
     2: Result := @ScreenBlend;
     3: Result := @OverlayBlend;
     4: Result := @SoftLightBlend;
     5: Result := @HardLightBlend;
     6: Result := @BrightLightBlend;
     7: Result := @ColorDodgeBlend;
     8: Result := @ColorBurnBlend;
     9: Result := @DarkenBlend;
    10: Result := @LightenBlend;
    11: Result := @DifferenceBlend;
    12: Result := @NegationBlend;
    13: Result := @ExclusionBlend;
    14: Result := @HueBlend;
    15: Result := @SaturationBlend;
    16: Result := @ColorBlend;
    17: Result := @LuminosityBlend;
    18: Result := @AverageBlend;
    19: Result := @InverseColorDodgeBlend;
    20: Result := @InverseColorBurnBlend;
    21: Result := @SoftColorDodgeBlend;
    22: Result := @SoftColorBurnBlend;
    23: Result := @ReflectBlend;
    24: Result := @GlowBlend;
    25: Result := @FreezeBlend;
    26: Result := @HeatBlend;
    27: Result := @AdditiveBlend;
    28: Result := @SubtractiveBlend;
    29: Result := @InterpolationBlend;
    30: Result := @StampBlend;
    31: Result := @xorBlend;
    32: Result := @andBlend;
    33: Result := @orBlend;
    34: Result := @RedBlend;
    35: Result := @GreenBlend;
    36: Result := @BlueBlend;
    37: Result := @DissolveBlend;
  end;
end; { GetBlendMode }



function GetBlendModeString(const Mode: TBlendMode32): string;
var
  s: string;
begin
  case Mode of
    bbmNormal32           : s := 'Normal';
    bbmMultiply32         : s := 'Multiply';
    bbmScreen32           : s := 'Screen';
    bbmOverlay32          : s := 'Overlay';
    bbmBrightLight32      : s := 'Bright Light';
    bbmSoftLightXF32      : s := 'Soft Light XF';
    bbmHardLight32        : s := 'Hard Light';
    bbmColorDodge32       : s := 'Color Dodge';
    bbmColorBurn32        : s := 'Color Burn';
    bbmDarken32           : s := 'Darken';
    bbmLighten32          : s := 'Lighten';
    bbmDifference32       : s := 'Difference';
    bbmNegation32         : s := 'Negation';
    bbmExclusion32        : s := 'Exclusion';
    bbmHue32              : s := 'Hue';
    bbmSaturation32       : s := 'Saturation';
    bbmColor32            : s := 'Color';
    bbmLuminosity32       : s := 'Luminosity';
    bbmAverage32          : s := 'Average';
    bbmInverseColorDodge32: s := 'Inverse Color Dodge';
    bbmInverseColorBurn32 : s := 'Inverse Color Burn';
    bbmSoftColorDodge32   : s := 'Soft Color Dodge';
    bbmSoftColorBurn32    : s := 'Soft Color Burn';
    bbmReflect32          : s := 'Reflect';
    bbmGlow32             : s := 'Glow';
    bbmFreeze32           : s := 'Freeze';
    bbmHeat32             : s := 'Heat';
    bbmAdditive32         : s := 'Additive';
    bbmSubtractive32      : s := 'Subtractive';
    bbmInterpolation32    : s := 'Interpolation';
    bbmStamp32            : s := 'Stamp';
    bbmXOR32              : s := 'XOR';
    bbmAND32              : s := 'AND';
    bbmOR32               : s := 'OR';
    bbmRed32              : s := 'Red';
    bbmGreen32            : s := 'Green';
    bbmBlue32             : s := 'Blue';
    bbmDissolve32         : s := 'Dissolve';
  end;
  
  Result := s;
end;

function ARGBBlendByMode(const F, B: TColor32; const M: TColor32;
  const Mode: TBlendMode32): TColor32;
begin
  Result := B;

  case Mode of
    bbmNormal32           : BlendMode.NormalBlend(F, Result, M);
    bbmMultiply32         : BlendMode.MultiplyBlend(F, Result, M);
    bbmScreen32           : BlendMode.ScreenBlend(F, Result, M);
    bbmOverlay32          : BlendMode.OverlayBlend(F, Result, M);
    bbmBrightLight32      : BlendMode.BrightLightBlend(F, Result, M);
    bbmSoftLightXF32      : BlendMode.SoftLightBlend(F, Result, M);
    bbmHardLight32        : BlendMode.HardLightBlend(F, Result, M);
    bbmColorDodge32       : BlendMode.ColorDodgeBlend(F, Result, M);
    bbmColorBurn32        : BlendMode.ColorBurnBlend(F, Result, M);
    bbmDarken32           : BlendMode.DarkenBlend(F, Result, M);
    bbmLighten32          : BlendMode.LightenBlend(F, Result, M);
    bbmDifference32       : BlendMode.DifferenceBlend(F, Result, M);
    bbmNegation32         : BlendMode.NegationBlend(F, Result, M);
    bbmExclusion32        : BlendMode.ExclusionBlend(F, Result, M);
    bbmHue32              : BlendMode.HueBlend(F, Result, M);
    bbmSaturation32       : BlendMode.SaturationBlend(F, Result, M);
    bbmColor32            : BlendMode.ColorBlend(F, Result, M);
    bbmLuminosity32       : BlendMode.LuminosityBlend(F, Result, M);
    bbmAverage32          : BlendMode.AverageBlend(F, Result, M);
    bbmInverseColorDodge32: BlendMode.InverseColorDodgeBlend(F, Result, M);
    bbmInverseColorBurn32 : BlendMode.InverseColorBurnBlend(F, Result, M);
    bbmSoftColorDodge32   : BlendMode.SoftColorDodgeBlend(F, Result, M);
    bbmSoftColorBurn32    : BlendMode.SoftColorBurnBlend(F, Result, M);
    bbmReflect32          : BlendMode.ReflectBlend(F, Result, M);
    bbmGlow32             : BlendMode.GlowBlend(F, Result, M);
    bbmFreeze32           : BlendMode.FreezeBlend(F, Result, M);
    bbmHeat32             : BlendMode.HeatBlend(F, Result, M);
    bbmAdditive32         : BlendMode.AdditiveBlend(F, Result, M);
    bbmSubtractive32      : BlendMode.SubtractiveBlend(F, Result, M);
    bbmInterpolation32    : BlendMode.InterpolationBlend(F, Result, M);
    bbmStamp32            : BlendMode.StampBlend(F, Result, M);
    bbmXOR32              : BlendMode.xorBlend(F, Result, M);
    bbmAND32              : BlendMode.andBlend(F, Result, M);
    bbmOR32               : BlendMode.orBlend(F, Result, M);
    bbmRed32              : BlendMode.RedBlend(F, Result, M);
    bbmGreen32            : BlendMode.GreenBlend(F, Result, M);
    bbmBlue32             : BlendMode.BlueBlend(F, Result, M);
    bbmDissolve32         : BlendMode.DissolveBlend(F, Result, M);
  end;
end;

//--- Blendmodes ---------------------------------------------------------------

procedure TBlendMode.NormalBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255; // bR + (fR - bR) * aM div 255; //shr 8;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24 and $FF;
  Alpha2 := B shr 24 and $FF;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { NormalBlend }

procedure TBlendMode.MultiplyBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR * fR div 255;
    fG := bG * fG div 255;
    fB := bB * fB div 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { MultiplyBlend }

procedure TBlendMode.ScreenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if ( (F shr 24) and $FF ) <> 0 then
  begin
    { Combine }
    fR := 255 - (255 - fR) * (255 - bR) div 255;
    fG := 255 - (255 - fG) * (255 - bG) div 255;
    fB := 255 - (255 - fB) * (255 - bB) div 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { ScreenBlend }

procedure TBlendMode.OverlayBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if   bR < 128
    then fR := bR * fR div 128
    else fR := 255 - (255 - bR) * (255 - fR) div 128;

    if   bG < 128
    then fG := bG * fG div 128
    else fG := 255 - (255 - bG) * (255 - fG) div 128;

    if   bB < 128
    then fB := bB * fB div 128 //shr 7
    else fB := 255 - (255 - bB) * (255 - fB) div 128;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { OverlayBlend }

{ Soft Light - formula by Jens Gruschel }
procedure TBlendMode.SoftLightBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB, C: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    C  := bR * fR div 255;
    fR := C + bR * (  255 - ( (255 - bR) * (255 - fR) div 255 ) - C  ) div 255;

    C  := bG * fG div 255;
    fG := C + bG * (  255 - ( (255 - bG) * (255 - fG) div 255 ) - C  ) div 255;

    C  := bB * fB div 255;
    fB := C + bB * (  255 - ( (255 - bB) * (255 - fB) div 255 ) - C  ) div 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { SoftLightBlend }

procedure TBlendMode.HardLightBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if   fR < 128
    then fR := bR * fR div 128
    else fR := 255 - (255 - bR) * (255 - fR) div 128;

    if   fG < 128
    then fG := bG * fG div 128
    else fG := 255 - (255 - bG) * (255 - fG) div 128;

    if   fB < 128
    then fB := bB * fB div 128
    else fB := 255 - (255 - bB) * (255 - fB) div 128;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { HardLightBlend }

{ Bright Light - Introduced by Michael Hansen -  much like average }
procedure TBlendMode.BrightLightBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := SqrtTable[fR * bR];
    fG := SqrtTable[fG * bG];
    fB := SqrtTable[fB * bB];
    {Blend}
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { BrightLightBlend }

procedure TBlendMode.ColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if fR < 255 then
    begin
      fR := bR * 255 div (255 - fR);
      if fR > 255 then fR := 255;
    end;

    if fG < 255 then
    begin
      fG := bG * 255 div (255 - fG);
      if fG > 255 then fG := 255;
    end;

    if fB < 255 then
    begin
      fB := bB * 255 div (255 - fB);
      if fB > 255 then fB := 255;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; {ColorDodgeBlend }

procedure TBlendMode.ColorBurnBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if fR > 0 then
    begin
      fR := 255 - ( (255 - bR) * 255 div fR );
      if fR < 0 then fR := 0;
    end;

    if fG > 0 then
    begin
      fG := 255 - ( (255 - bG) * 255 div fG );
      if fG < 0 then fG := 0;
    end;

    if fB > 0 then
    begin
      fB := 255 - ( (255 - bB) * 255 div fB );
      if fB < 0 then fB := 0;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { ColorBurnBlend }

procedure TBlendMode.DarkenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if ( (F shr 24) and $FF ) <> 0 then
  begin
    { Combine }
    if fR > bR then fR := bR;
    if fG > bG then fG := bG;
    if fB > bB then fB := bB;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { DarkenBlend }

procedure TBlendMode.LightenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if fR < bR then fR := bR;
    if fG < bG then fG := bG;
    if fB < bB then fB := bB;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { LightenBlend }

procedure TBlendMode.DifferenceBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := abs(bR - fR);
    fG := abs(bG - fG);
    fB := abs(bB - fB);
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { Difference }

{ Negation - introduced by Jens Gruschel }
procedure TBlendMode.NegationBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := 255 - abs(255 - bR - fR);
    fG := 255 - abs(255 - bG - fG);
    fB := 255 - abs(255 - bB - fB);
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { NegationBlend }

procedure TBlendMode.ExclusionBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR + fR - bR * fR div 128;
    fG := bG + fG - bG * fG div 128;
    fB := bB + fB - bB * fB div 128;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { ExclusionBlend }

procedure TBlendMode.HueBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
  fH, fS, fL, bH, bS, bL   : Single;
  NewHSL                   : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Invert Channel To HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, BL);
  { Combine HSL and invert it to RGB }
  NewHSL := HSLToRGB(fH, bS, bL);
  { Channel separation }
  fR := NewHSL shr 16 and $FF;
  fG := NewHSL shr 8  and $FF;
  fB := NewHSL        and $FF;
  bR := B      shr 16 and $FF;
  bG := B      shr 8  and $FF;
  bB := B             and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { HueBlend }

procedure TBlendMode.SaturationBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
  fH, fS, fL, bH, bS, bL   : Single;
  NewHSL                   : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Invert Channel To HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, BL);
  { Combine HSL and invert it to RGB}
  NewHSL := HSLToRGB(bH, fS, bL);
  { Channel separation }
  fR := NewHSL shr 16 and $FF;
  fG := NewHSL shr 8  and $FF;
  fB := NewHSL        and $FF;
  bR := B      shr 16 and $FF;
  bG := B      shr 8  and $FF;
  bB := B             and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { SaturationBlend }

procedure TBlendMode.ColorBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
  fH, fS, fL, bH, bS, bL   : Single;
  NewHSL                   : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Invert channel to HLS }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, BL);
  { Combine HSL and invert it to RGB}
  NewHSL := HSLToRGB(fH, fS, bL);
  { Channel separation }
  fR := NewHSL shr 16 and $FF;
  fG := NewHSL shr 8  and $FF;
  fB := NewHSL        and $FF;
  bR := B      shr 16 and $FF;
  bG := B      shr 8  and $FF;
  bB := B             and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { ColorBlend }

procedure TBlendMode.LuminosityBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
  fH, fS, fL, bH, bS, bL   : Single;
  NewHSL                   : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Invert channel To HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, BL);
  { Combine HSL and invert it to RGB}
  NewHSL := HSLToRGB(bH, bS, fL);
  { Channel separation }
  fR := NewHSL shr 16 and $FF;
  fG := NewHSL shr 8  and $FF;
  fB := NewHSL        and $FF;
  bR := B      shr 16 and $FF;
  bG := B      shr 8  and $FF;
  bB := B             and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { LuminosityBlend }

{ Average - useful in some cases - but the same as Normal with MasterAlpha = 128 }
procedure TBlendMode.AverageBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := (fR + bR) div 2;
    fG := (fg + bG) div 2;
    fB := (fB + bB) div 2;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { AverageBlend }

procedure TBlendMode.InverseColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if   bR = 255
    then fR := 255
    else
    begin
      fR := fR * 255 div (255 - bR);
      if   fR > 255
      then fR := 255;
    end;

    if   bG = 255
    then fG := 255
    else
    begin
      fG := fG * 255 div (255 - bG);
      if   fG > 255
      then fG := 255;
    end;

    if   bB = 255
    then fB := 255
    else
    begin
      fB := fB * 255 div (255 - bB);
      if   fB > 255
      then fB := 255;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { InverseColorDodgeBlend }

procedure TBlendMode.InverseColorBurnBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if   bR = 0
    then fR := 0
    else
    begin
      fR := 255 - (255 - fR) * 255 div bR;
      if   fR < 0
      then fR := 0;
    end;

    if   bG = 0
    then fG := 0
    else
    begin
      fG := 255 - (255 - fG) * 255 div bG;
      if   fG < 0
      then fG := 0;
    end;

    if   bB = 0
    then fB := 0
    else
    begin
      fB := 255 - (255 - fB) * 255 div bB;
      if   fB < 0
      then fB := 0;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { InverseColorBurnBlend }

procedure TBlendMode.SoftColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if (bR + fR) < 256 then
    begin
      if fR <> 255 then
      begin
        fR := bR * 128 div (255 - fR);
        if   fR > 255
        then fR := 255;
      end;
    end
    else
    begin
      fR := 255 - (255 - fR) * 128 div bR;
      if   fR < 0
      then fR := 0;
    end;

    if (bG + fG) < 256 then
    begin
      if fG <> 255 then
      begin
        fG := bG * 128 div (255 - fG);
        if   fG > 255
        then fG := 255;
      end;
    end
    else
    begin
      fG := 255 - (255 - fG) * 128 div bG;
      if   fG < 0
      then fG := 0;
    end;

    if (bB + fB) < 256 then
    begin
      if fB <> 255 then
      begin
        fB := bB * 128 div (255 - fB);
        if   fB > 255
        then fB := 255;
      end;
    end
    else
    begin
      fB := 255 - (255 - fB) * 128 div bB;
      if   fB < 0
      then fB := 0;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { SoftColorDodgeBlend }

procedure TBlendMode.SoftColorBurnBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if (bR + fR) < 256 then
    begin
      if   bR = 255
      then fR := 255
      else
      begin
        fR := fR * 128 div (255 - bR);
        if   fR > 255
        then fR := 255;
      end;
    end
    else
    begin
      fR := 255 - (255 - bR) * 128  div fR;
      if   fR < 0
      then fR := 0;
    end;

    if (bG + fG) < 256 then
    begin
      if   bG = 255
      then fG := 255
      else
      begin
        fG := fG * 128 div (255 - bG);
        if   fG > 255
        then fG := 255;
      end;
    end
    else
    begin
      fG := 255 - (255 - bG) * 128 div fG;
      if   fG < 0
      then fG := 0;
    end;

    if (bB + fB) < 256 then
    begin
      if   bB = 255
      then fB := 255
      else
      begin
        fB := fB * 128 div (255 - bB);
        if   fB > 255
        then fB := 255;
      end;
    end
    else
    begin
      fB := 255 - (255 - bB) * 128 div fB;
      if   fB < 0
      then fB := 0;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { SoftColorBurnBlend }

{ Reflect - introduced by Michael Hansen }
procedure TBlendMode.ReflectBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if fR <> 255 then
    begin
      fR := Sqr(bR) div (255 - fR);
      if   fR > 255
      then fR := 255;
    end;

    if fG <> 255 then
    begin
      fG := Sqr(bG) div (255 - fG);
      if   fG > 255
      then fG := 255;
    end;

    if fB <> 255 then
    begin
      fB := Sqr(bB) div (255 - fB);
      if   fB > 255
      then fB := 255;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { ReflectBlend }

procedure TBlendMode.GlowBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if bR < 255 then
    begin
      fR := sqr(fR) div (255 - bR);
      if fR > 255 then fR := 255;
    end
    else fR := 255;

    if bG < 255 then
    begin
      fG := sqr(fG) div (255 - bG);
      if fG > 255 then fG := 255;
    end
    else fG := 255;

    if bB < 255 then
    begin
      fB := sqr(fB) div (255 - bB);
      if fB > 255 then fB := 255;
    end
    else fB := 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { GlowBlend }

procedure TBlendMode.FreezeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if fR > 0 then
    begin
      fR := 255 - Sqr(255 - bR) div fR;
      if   fR < 0
      then fR := 0;
    end;

    if fG > 0 then
    begin
      fG := 255 - Sqr(255 - bG) div fG;
      if   fG < 0
      then fG := 0;
    end;

    if fB > 0 then
    begin
      fB := 255 - Sqr(255 - bB) div fB;
      if   fB < 0
      then fB := 0;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { FreezeBlend }

procedure TBlendMode.HeatBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    if   bR = 0
    then fR := 0
    else
    begin
      fR := 255 - Sqr(255 - fR) div bR;
      if   fR < 0
      then fR := 0;
    end;

    if   bG = 0
    then fG := 0
    else
    begin
      fG := 255 - Sqr(255 - fG) div bG;
      if   fG < 0
      then fG := 0;
    end;

    if   bB = 0
    then fB := 0
    else
    begin
      fB := 255 - Sqr(255 - fB) div bB;
      if   fB < 0
      then fB := 0;
    end;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { HeatBlend }

procedure TBlendMode.AdditiveBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := fR + bR;
    fG := fG + bG;
    fB := fB + bB;
    if fR > 255 then fR := 255;
    if fG > 255 then fG := 255;
    if fB > 255 then fB := 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { AdditiveBlend }

procedure TBlendMode.SubtractiveBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB      : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR + fR - 256;
    fG := bG + fG - 256;
    fB := bB + fB - 256;
    if fR < 0 then fR := 0;
    if fG < 0 then fG := 0;
    if fB < 0 then fB := 0;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { SubtractiveBlend }

procedure TBlendMode.InterpolationBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := CosineTab[fR] + CosineTab[bR];
    fG := CosineTab[fG] + CosineTab[bG];
    fB := CosineTab[fB] + CosineTab[bB];
    if   fR > 255
    then fR := 255;
    if   fG > 255
    then fG := 255;
    if   fB > 255
    then fB := 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { InterpolationBlend }

procedure TBlendMode.StampBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR + fR * 2 - 255; //256;
    fG := bG + fG * 2 - 255; //256;
    fB := bB + fB * 2 - 255; //256;
    if   fR < 0
    then fR := 0
    else
    if   fR > 255
    then fR := 255;

    if   fG < 0
    then fG := 0
    else
    if   fG > 255
    then fG := 255;

    if   fB < 0
    then fB := 0
    else
    if   fB > 255
    then fB := 255;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { StampBlend }

procedure TBlendMode.xorBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR xor fR;
    fG := bG xor fG;
    fB := bB xor fB;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { xorBlend }

procedure TBlendMode.andBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR and fR;
    fG := bG and fG;
    fB := bB and fB;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { andBlend }

procedure TBlendMode.orBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR or fR;
    fG := bG or fG;
    fB := bB or fB;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { orBlend }

procedure TBlendMode.RedBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fG := bG;
    fB := bB;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { RedBlend }

procedure TBlendMode.GreenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if (F shr 24 and $FF) <> 0 then
  begin
    { Combine }
    fR := bR;
    fB := bB;
    {Blend}
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { GreenBlend }

procedure TBlendMode.BlueBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, Alpha1, Alpha2, Alpha: Cardinal;
  fR, fG, fB, bR, bG, bB   : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  aM := F shr 24 * aM;
  aM := aM div 255; //shr 8;
  if aM = 0 then Exit; //exit if nothing changes ...
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  { Transparent process }
  if ( (F shr 24) and $FF ) <> 0 then
  begin
    { Combine }
    fR := bR;
    fG := bG;
    { Blend }
    bR := ( fR * aM + bR * (255 - aM) ) div 255;
    bG := ( fG * aM + bG * (255 - aM) ) div 255;
    bB := ( fB * aM + bB * (255 - aM) ) div 255;
  end;
  {Reset - keeping B alpha}
  //B  := (B shl 24) or (bR shl 16) or (bG shl 8) or bB;
  Alpha1 := F shr 24;
  Alpha2 := B shr 24;
  Alpha  := Alpha1 or Alpha2;
  B      := (Alpha shl 24) or (bR shl 16) or (bG shl 8) or bB;
end; { BlueBlend }


procedure TBlendMode.DissolveBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  LProbIndex  : Cardinal;
  LRandomIndex: Integer;
begin
  LProbIndex   := Round( (M and $FF) / 255 * 100 );
  LRandomIndex := Random(100);

  if not ProbTable[LProbIndex, LRandomIndex] then
  begin
    F := F and $00FFFFFF;
  end;

  BlendMode.NormalBlend(F, B, 255);  
end; { DissolveBlend }

//== initialization part =======================================================

initialization
  { Init SqrtTable }
  for x := 0 to 65535 do SqrtTable[x] := Round( Sqrt(x) );
  { Init Custom Blendmap - like normal blend }
  for x := 0 to 255 do for y := 0 to 255 do FillChar(BlendMap[y + x shl 8],3,x);
  { Init CosineTable }
  for I := 0 to 255 do CosineTab[I] := Round( 64 - Cos(I * Pi / 255) * 64 );

end.
