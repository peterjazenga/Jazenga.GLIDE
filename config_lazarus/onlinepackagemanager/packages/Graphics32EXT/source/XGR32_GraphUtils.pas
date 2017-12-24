
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_GraphUtils;

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Graphics,
  GR32;
const
  // Some predefined color constants
  clDKGray32 = TColor32($FF808080);

type

  PColor32Rec =^TColor32Rec;
  TColor32Rec = packed record
    case Cardinal of
      0: (Color: Cardinal);
      2: (HiWord, LoWord: Word);
      3: (rgbBlue, rgbGreen, rgbRed, rgbAlpha: Byte);  //Blue, Green, Red, Alpha
    end;
  PColor32RecArray = ^TColor32RecArray;
  TColor32RecArray = array [0..0] of TColor32Rec;


  TFrameStyle = (fsNone, fsFlat, fsGroove, fsBump, fsLowered, fsButtonDown,
                  fsRaised, fsButtonUp, fsStatus, fsPopup, fsFlatBold, fsImage);
  TFrameSide = (sdLeft, sdTop, sdRight, sdBottom);
  TFrameSides = set of TFrameSide;

type
  TRectProperty = class;
  TRectProperty = class(TPersistent)
  private
    FBottom: Integer;
    FLeft: Integer;
    FOnChanged: TNotifyEvent;
    FOwner: TPersistent;
    FRight: Integer;
    FTop: Integer;
    function GetValue: TRect;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
  protected
    procedure Changed;
    function GetOwner: TPersistent; override;
  public
    constructor Create(const aOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    property Owner: TPersistent read FOwner;
    property Value: TRect read GetValue;
  published
    property Bottom: Integer read FBottom write SetBottom;
    property Left: Integer read FLeft write SetLeft;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Right: Integer read FRight write SetRight;
    property Top: Integer read FTop write SetTop;
  end;
  
  { Summary The Custom Graphic Property }
  { Description
  You must override the InternalUpdate Method.
  and if you wanna get notifiction when property changed.
  you should assign the OnChanged Event.
  }
  TCustomGraphicProperty = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FOwner: TPersistent;
    procedure SetEnabled(Value: Boolean);
  protected
    FEnabled: Boolean;
    FUpdateCount: Integer;
    UpdatePended: Boolean;
    procedure DoChanged; virtual;
    function GetOwner: TPersistent; override;
    procedure iAssign(Source: TPersistent); virtual;
    procedure InternalUpdate; virtual;
    function IsUpdating: Boolean;
    { Summary Abondon now! }
    procedure SetUpdating(Updating: Boolean);
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    { Description
    @param R the aDst's rect to draw(general it's a ClientRect(0,0,Width,
    Hieight)).
    @parma aDstX, aDstY 
    
    sometime we need the aSrc to do sth.
    eg, the ShadowEffect:
      GenerateShadow(aSrc, aShadow, aR);
      aShadow.DrawTo(aDst, aDstX+OffsetX, aDstY+OffsetY);
    }
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; virtual;
    { Summary Paint it to the aDst at the aR rect. }
    procedure PaintTo(aDst: TBitmap32; aR: TRect); overload;
    procedure Update;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Owner: TPersistent read FOwner;
  end;
  

const
  NULLRect: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  ALLFRAME_SIDES = [ sdLeft, sdTop, sdRight, sdBottom ];

const                                                           { Color Arrays }
  { Summary Frame Style Color constant arrays }
  ULFrameColor: array[ TFrameStyle ] of TColor = ( clWindow,
                                                   clWindowFrame,
                                                   clBtnShadow,
                                                   clBtnHighlight,
                                                   clBtnShadow,
                                                   clLime,
                                                   clBlack,
                                                   clRed,
                                                   clBtnShadow,
                                                   clBtnHighlight,
                                                   clBtnShadow, 
                                                   clBtnShadow //fake fsIamge
  );

  LRFrameColor: array[ TFrameStyle ] of TColor = ( clWindow,
                                                   clWindowFrame,
                                                   clBtnHighlight,
                                                   clBtnShadow,
                                                   clBtnHighlight,
                                                   clTeal,
                                                   clBtnFace,
                                                   clYellow,
                                                   clBtnHighlight,
                                                   clBtnShadow,
                                                   clBtnShadow, 
                                                   clBtnShadow //fake fsIamge
  );


// Region function

{ Summary the Black(R=0,G=0,B=0) color or (alpha = 0) means trasparent}
//function CreateRgnFromBmp(B: TBitmap32; var RgnData: PRgnData): integer;

{ Summary Convert RGB color to Hue(Ι«µχ), Saturation(±¥ΊΝ¶Θ) and Luminance(ΑΑ¶Θ)}
procedure ColorToHSL(C: TColor; var H, S, L: Byte);
{ Summary Convert Hue, Saturation and Luminance to RGB color }
function HSLtoColor(H, S, L: Byte): TColor;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The border's upper and left sides will be colored with the ULColor
value, while the lower and right sides will be colored with the LRColor value.
The Sides set is used to specify which sides of the border should be drawn.
The function returns the interior rectangle bounded by the border.}
function DrawSides(bmp: TBitmap32; Bounds: TRect; ULColor, LRColor: TColor32;
  Sides: TFrameSides; const Alpha: Byte = 255; const aWidth: Integer = 1):
  TRect;

function DrawBevel(bmp: TBitmap32; Bounds: TRect; ULColor, LRColor: TColor32;
  Width: Integer; Sides: TFrameSides = ALLFRAME_SIDES; const Alpha: Byte =
  255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
  The border is drawn using the fsRaised or fsLowered frame style depending on the Lowered value.
  The function returns the interior rectangle bounded by the border.
}
function DrawCtl3DBorder(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean; const
  Alpha: Byte = 255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
  The border is drawn using the fsRaised or fsLowered frame style depending on the Lowered value.
  The Sides set is used to specify which sides of the border should be drawn. The function
  returns the interior rectangle bounded by the border.
}
function DrawCtl3DBorderSides(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean;
  Sides: TFrameSides; const Alpha: Byte = 255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas.}
{ Description
The border is drawn using the fsButtonUp or fsButtonDown frame style depending on the Lowered value.
The function returns the interior rectangle bounded by the border.
}
function DrawButtonBorder(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean):
  TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas.}
{ Description
The border is drawn using the fsButtonUp or fsButtonDown frame style depending on the Lowered value.
The Sides set is used to specify which sides of the border should be drawn. The function returns the
interior rectangle bounded by the border.
}
function DrawButtonBorderSides(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean;
  Sides: TFrameSides; const Alpha: Byte = 255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The border is drawn using the fsButtonUp or fsButtonDown frame style depending on the Lowered value.
The function returns the interior rectangle bounded by the border. This function differs from the
DrawButtonBorder function in that the shading used for the borders is calculated from the FaceColor
parameter rather than using the clBtnHighlight and clBtnShadow colors.}
function DrawColorButtonBorder(bmp: TBitmap32; Bounds: TRect; FaceColor:
  TColor32; Lowered: Boolean;  const aWidth: integer = 1; const aAlpha: Byte =
  255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The border is drawn using the fsButtonUp or fsButtonDown frame style depending on the Lowered value.
The function returns the interior rectangle bounded by the border. This function differs from the
DrawButtonBorderSides function in that the shading used for the borders is calculated from the
FaceColor parameter rather than using the clBtnHighlight and clBtnShadow colors.}
function DrawColorButtonBorderSides(bmp: TBitmap32; Bounds: TRect; FaceColor:
  TColor32; Lowered: Boolean; Sides: TFrameSides; const aWidth: integer = 1;
  const Alpha: byte =255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The style of the border drawn is controlled by the Style parameter. The function returns
the interior rectangle bounded by the border.}
function DrawBorder(bmp: TBitmap32; Bounds: TRect; Style: TFrameStyle; const
  Alpha: Byte = 255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The style of the border drawn is controlled by the Style parameter. The Sides set is used
to specify which sides of the border should be drawn. The function returns the interior
rectangle bounded by the border.}
function DrawBorderSides(bmp: TBitmap32; Bounds: TRect; Style: TFrameStyle;
  Sides: TFrameSides; const Alpha: Byte = 255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The style of the border drawn is controlled by the Style parameter. The function returns
the interior rectangle bounded by the border. This function differs from the DrawBorder
function in that the shading used for the borders is calculated from the FaceColor parameter
rather than using the clBtnHighlight and clBtnShadow colors.}
function DrawColorBorder(bmp: TBitmap32; Bounds: TRect; FaceColor: TColor32;
  Style: TFrameStyle; const Alpha: Byte = 255): TRect;

{ Summary This function draws a rectangular border defined by the Bounds parameter on the Canvas. }
{ Description
The style of the border drawn is controlled by the Style parameter. The Sides set is used
to specify which sides of the border should be drawn. The function returns the interior
rectangle bounded by the border. This function differs from the DrawBorder function in
that the shading used for the borders is calculated from the FaceColor parameter rather
than using the clBtnHighlight and clBtnShadow colors.}
function DrawColorBorderSides(bmp: TBitmap32; Bounds: TRect; FaceColor:
  TColor32; Style: TFrameStyle; Sides: TFrameSides; const Alpha: Byte = 255):
  TRect;
function DrawColorBorderSidesWithWidth(bmp: TBitmap32; Bounds: TRect;
  FaceColor: TColor32; Style: TFrameStyle; Sides: TFrameSides; const aWidth:
  integer; const Alpha: Byte = 255): TRect;

procedure LoadPicture(src: TBitmap32; const aFileName: string; aTransparent: Boolean = True);
procedure LoadGraphic(src: TBitmap32; const aGraphic: TGraphic; const
  aDstRect:TRect);

implementation

uses
  Math;

procedure LoadPicture(src: TBitmap32; const aFileName: string; aTransparent: Boolean = True);
var
  LPic: TPicture;
  tc: TColor32;
  x, y: Integer;
  p: PByteArray;
begin
  LPic := TPicture.Create;
  try
   LPic.LoadFromFile(aFileName);
   LPic.Graphic.Transparent := aTransparent;
   src.Assign(LPic);

  {$IFDEF PNG_Supports}
   src.ResetAlpha;
   if LPic.Graphic is TPNGObject then
   with LPic.Graphic as TPNGObject do
   begin
    case TransparencyMode of
      ptmBit:
        begin
          tc := Color32(TransparentColor);
          for y := 0 to src.Height - 1 do
            for x := 0 to src.Width - 1 do
              if src.Pixel[x, y] = tc then
                PColor32Rec(src.PixelPtr[x, y])^.rgbAlpha := 0;
        end;
      ptmPartial:
        begin
          if (Header.ColorType = COLOR_GRAYSCALEALPHA) or
            (Header.ColorType = COLOR_RGBALPHA) then
          begin
            for y := 0 to src.Height - 1 do
            begin
              p := AlphaScanline[y];
              for x := 0 to src.Width - 1 do
                PColor32Rec(src.PixelPtr[x, y])^.rgbAlpha := p[x];
            end;
          end;
        end;
    end;
   end;
  {$ENDIF}
  finally
    LPic.Free;
  end;
end;

{procedure BlendToDC(src: TBitmap32; DstDC: HDC; DstX, DstY: Integer; 
  const SrcRect: TRect);
const
  MaxBufferSz = 64;
var
  DstRect, R: TRect;
  SrcIndex, BufferIndex, HCount, VCount: Integer;
  Y: Integer;
  Buffer: TDIB32;
begin
end;//}

function DrawSides(bmp: TBitmap32; Bounds: TRect; ULColor, LRColor: TColor32;
  Sides: TFrameSides; const Alpha: Byte = 255; const aWidth: Integer = 1):
  TRect;
var
  i: integer;
  //LC: TColor32Rec;
begin
  Result := Bounds;
  with Bounds do
  begin
    //LC.Color := Color32(ULColor);
    TColor32Rec(ULColor).rgbAlpha := Alpha;
    bmp.PenColor := ULColor;

    if (sdLeft in Sides) then
    begin
      for i := 1 to aWidth do
      begin 
        bmp.VertLineS(Result.Left, Top, Bottom, bmp.PenColor);
        Inc(Result.Left);
      end;
    end;

    if (sdTop in Sides)  then
    begin
      for i := 1 to aWidth do
      begin 
        bmp.HorzLineS(Left, Result.Top, Right, bmp.PenColor);
        Inc(Result.Top);
      end;
    end;

    TColor32Rec(LRColor).rgbAlpha := Alpha;
    //LC.Color := Color32(LRColor);
    //LC.rgbAlpha := Alpha;
    bmp.PenColor := LRColor;
    if (sdRight in Sides)  then
    begin
      for i := 1 to aWidth do
      begin 
        bmp.VertLineS(Result.Right, Top, Bottom, bmp.PenColor);
        Dec(Result.Right);
      end;
    end;

    if (sdBottom in Sides) then
    begin
      for i := 1 to aWidth do
      begin 
        bmp.HorzLineS(Left, Result.Bottom, Right, bmp.PenColor);
        Dec(Result.Bottom);
      end;
    end;
  end;
end; {= DrawSides =}

function DrawBevel(bmp: TBitmap32; Bounds: TRect; ULColor, LRColor: TColor32;
  Width: Integer; Sides: TFrameSides = ALLFRAME_SIDES; const Alpha: Byte =
  255): TRect;
var
  I: Integer;
begin
  //Canvas.Pen.Width := Width;
  for I := 1 to Width do                         { Loop through width of bevel }
  begin
    Bounds := DrawSides(bmp, Bounds, ULColor, LRColor, Sides, Alpha );
  end;
  Result := Bounds;
end;


{=======================================}
{== Generic DrawCtl3DBorder Procedure ==}
{=======================================}

function DrawCtl3DBorder(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean; const
  Alpha: Byte = 255): TRect;
begin
  Result := DrawCtl3DBorderSides( bmp, Bounds, Lowered, ALLFRAME_SIDES, Alpha );
end;


function DrawCtl3DBorderSides(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean;
  Sides: TFrameSides; const Alpha: Byte = 255): TRect;
const
  Colors: array[ 1..4, Boolean ] of TColor = ( ( cl3DLight, clBtnShadow ),
                                                ( clBtnText, clBtnHighlight ),
                                                ( clBtnHighlight, clBtnText ),
                                                ( clBtnShadow, cl3DLight ) );
begin
  Result := DrawSides( bmp, Bounds, Color32(Colors[ 1, Lowered ]),
                       Color32(Colors[ 2, Lowered ]), Sides, Alpha );
  Result := DrawSides( bmp, Result, Color32(Colors[ 3, Lowered ]),
                       Color32(Colors[ 4, Lowered ]), Sides, Alpha );
end;


{========================================}
{== Generic DrawButtonBorder Procedure ==}
{========================================}

function DrawButtonBorder(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean):
  TRect;
begin
  Result := DrawButtonBorderSides( bmp, Bounds, Lowered, ALLFRAME_SIDES );
end;


function DrawButtonBorderSides(bmp: TBitmap32; Bounds: TRect; Lowered: Boolean;
  Sides: TFrameSides; const Alpha: Byte = 255): TRect;
const
  Colors: array[ 1..4, Boolean ] of TColor = ( ( clBtnHighlight, clBtnText ),
                                                ( cl3DDkShadow, clBtnText ),
                                                ( cl3DLight, clBtnShadow ),
                                                ( clBtnShadow, clBtnShadow ) );
begin
  Bounds := DrawBevel( bmp, Bounds, Color32(Colors[ 1, Lowered ]),
                       Color32(Colors[ 2, Lowered ]), 1, Sides, Alpha );
  Result := DrawBevel( bmp, Bounds, Color32(Colors[ 3, Lowered ]),
                       Color32(Colors[ 4, Lowered ]), 1, Sides, Alpha );
end;



function DrawColorButtonBorder(bmp: TBitmap32; Bounds: TRect; FaceColor:
  TColor32; Lowered: Boolean;  const aWidth: integer = 1; const aAlpha: Byte =
  255): TRect;
begin
  Result := DrawColorButtonBorderSides( bmp, Bounds, FaceColor, Lowered
  , ALLFRAME_SIDES, aWidth, aAlpha );
end;


function DrawColorButtonBorderSides(bmp: TBitmap32; Bounds: TRect; FaceColor:
  TColor32; Lowered: Boolean; Sides: TFrameSides; const aWidth: integer = 1;
  const Alpha: byte =255): TRect;
var
  ULColor, LRColor: TColor32;
  H, S, L: Byte;
begin
  RGBToHSL( FaceColor, H, S, L );
  if Lowered then
  begin
    ULColor := HSLtoRGB( H, S, Max( L - 100, 0 ) );
    LRColor := HSLtoRGB( H, S, Min( L + 100, 255 ) );
  end
  else
  begin
    ULColor := HSLtoRGB( H, S, Min( L + 100, 255 ) );
    LRColor := HSLtoRGB( H, S, Max( L - 100, 0 ) );
  end;

  Bounds := DrawSides( bmp, Bounds, ULColor, LRColor, Sides, Alpha );

  if Lowered then
  begin
    ULColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
    LRColor := HSLtoRGB( H, S, Min( L + 50, 255 ) );
  end
  else
  begin
    ULColor := HSLtoRGB( H, S, Min( L + 40, 255 ) );
    LRColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
  end;

  Result := DrawSides( bmp, Bounds, ULColor, LRColor, Sides, Alpha, aWidth );
end;

function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

procedure ColorToHSL(C: TColor; var H, S, L: Byte);
var
  Dif, CCmax, CCmin, RC, GC, BC, TempH, TempS, TempL: Double;
begin
  { Convert RGB color to Hue, Saturation and Luminance }

  { Convert Color to RGB color value. This is necessary if Color specifies
    a system color such as clHighlight }
  C := ColorToRGB( C );

  { Determine a percent (as a decimal) for each colorant }
  RC := GetRValue( C ) / 255;
  GC := GetGValue( C ) / 255;
  BC := GetBValue( C ) / 255;

  if RC > GC then
    CCmax := RC
  else
    CCmax := GC;
  if BC > CCmax then
    CCmax := BC;

  if RC < GC then
    CCmin := RC
  else
    CCmin := GC;

  if BC < CCmin then
    CCmin := BC;

  { Calculate Luminance }
  TempL := (CCmax + CCmin) / 2.0;

  if CCmax = CCmin then
  begin
    TempS := 0;
    TempH := 0;
  end
  else
  begin
    Dif := CCmax - CCmin;

    { Calculate Saturation }
    if TempL < 0.5 then
      TempS := Dif / (CCmax + CCmin)
    else
      TempS := Dif / ( 2.0 - CCmax - CCmin );

    { Calculate Hue }
    if RC = CCmax then
      TempH := (GC - BC) / Dif
    else if GC = CCmax then
      TempH := 2.0 + (BC - RC) / Dif
    else
      TempH := 4.0 + (RC - GC) / Dif;

    TempH := TempH / 6;
    if TempH < 0 then
      TempH := TempH + 1;
  end;

  H := Round( 240 * TempH );
  S := Round( 240 * TempS );
  L := Round( 240 * TempL );
end; {= ColorToHSL =}



function HSLtoColor(H, S, L: Byte): TColor;
var
  HN, SN, LN, RD, GD, BD, V, M, SV, Fract, VSF, Mid1, Mid2: Double;
  R, G, B: Byte;
  Sextant: Integer;
begin
  { Hue, Saturation, and Luminance must be normalized to 0..1 }

  HN := H / 239;
  SN := S / 240;
  LN := L / 240;

  if LN < 0.5 then
    V := LN * ( 1.0 + SN )
  else
    V := LN + SN - LN * SN;
  if V <= 0 then
  begin
    RD := 0.0;
    GD := 0.0;
    BD := 0.0;
  end
  else
  begin
    M := LN + LN - V;
    SV := (V - M ) / V;
    HN := HN * 6.0;
    Sextant := Trunc( HN );
    Fract := HN - Sextant;
    VSF := V * SV * Fract;
    Mid1 := M + VSF;
    Mid2 := V - VSF;

    case Sextant of
      0:
      begin
        RD := V;
        GD := Mid1;
        BD := M;
      end;

      1:
      begin
        RD := Mid2;
        GD := V;
        BD := M;
      end;

      2:
      begin
        RD := M;
        GD := V;
        BD := Mid1;
      end;

      3:
      begin
        RD := M;
        GD := Mid2;
        BD := V;
      end;

      4:
      begin
        RD := Mid1;
        GD := M;
        BD := V;
      end;

      5:
      begin
        RD := V;
        GD := M;
        BD := Mid2;
      end;

      else
      begin
        RD := V;
        GD := Mid1;
        BD := M;
      end;
    end;
  end;

  if RD > 1.0 then
    RD := 1.0;
  if GD > 1.0 then
    GD := 1.0;
  if BD > 1.0 then
    BD := 1.0;
  R := Round( RD * 255 );
  G := Round( GD * 255 );
  B := Round( BD * 255 );
  Result := RGB( R, G, B );
end; {= HSLtoColor =}

{ RGB Routings }

const

  FColCount = $FF;

type
  TRGB = record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

function RGBChange(A: Longint; Rx, Gx, Bx: ShortInt): Longint;
var
  RGB: TRGB;
begin
  RGB := TRGB(A);
  // Change Red Part
  If Rx > 0 Then
    If RGB.R + Rx < FColCount Then Inc(RGB.R, Rx) else RGB.R := FColCount;
  If Rx < 0 Then
    If RGB.R > Abs(Rx) Then Dec(RGB.R, Abs(Rx)) else RGB.R := 0;
  // Change Green Part
  If Gx > 0 Then
    If RGB.G + Gx < FColCount Then Inc(RGB.G, Gx) else RGB.G := FColCount;
  If Gx < 0 Then
    If RGB.G > Abs(Gx) Then Dec(RGB.G, Abs(Gx)) else RGB.G := 0;
  // Change Blue Part
  If Bx > 0 Then
    If RGB.B + Bx < FColCount Then Inc(RGB.B, Bx) else RGB.B := FColCount;
  If Bx < 0 Then
    If RGB.B > Abs(Bx) Then Dec(RGB.B, Abs(Bx)) else RGB.B := 0;
  Result := Longint(RGB);
end;

{ Blur algoritms }

function Red(Color: Tcolor): Byte;
begin
  result := GetRValue(ColorToRGB(Color));
end;

function Green(Color: Tcolor): Byte;
begin
  result := GetGValue(ColorToRGB(Color));
end;

function Blue(Color: Tcolor): Byte;
begin
  result := GetBValue(ColorToRGB(Color));
end;

function Compose(r,g,b: integer): TColor;
begin
  Result := RGB(r,g,b);
end;

function MiddleColor(color1, color2: TColor): Tcolor;
begin
  result := Compose(
    trunc((Red(Color1) + Red(Color2))/2),
    trunc((green(Color1) + green(Color2))/2),
    trunc((Blue(Color1) + Blue(Color2))/2))
end;

{**************************************************************************}


function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

// Region convert
           {
function CreateRgnFromBmp(B: TBitmap32; var RgnData: PRgnData): integer;
const
  max    = 10000;
var
  j, i, i1: integer;
  C: TColor32Rec;
  Rts: array [0..max] of TRect;
  Count: integer;
begin
  Result := 0;
  If B.Empty Then Exit;
  Count := 0;
  for j := 0 to B.Height-1 do
  begin
    i := 0;
    while i < B.Width do
    begin
      C := TColor32Rec(B.Pixel[i, j]);
      If (C.rgbAlpha=0) or (C.rgbRed + C.rgbGreen + C.rgbBlue = 0) Then
      begin
        i1 := i;
        C := TColor32Rec(B.Pixel[i1, j]);
        while (C.rgbAlpha=0) or (C.rgbRed + C.rgbGreen + C.rgbBlue = 0) do
        begin
          Inc(i1);
          C := TColor32Rec(B.Pixel[i1, j]);
          If i1 >= B.Width Then Break;
        end;
        Rts[Count] := Rect(i, j, i1, j+1);
        Inc(Count);
        i := i1;
        Continue;
      end;
      Inc(i);
    end;
  end;
  // Make Region data
  Result := Count*SizeOf(TRect);
  GetMem(Rgndata, SizeOf(TRgnDataHeader)+Result);
  FillChar(Rgndata^, SizeOf(TRgnDataHeader)+Result, 0);
  RgnData^.rdh.dwSize := SizeOf(TRgnDataHeader);
  RgnData^.rdh.iType := RDH_RECTANGLES;
  RgnData^.rdh.nCount := Count;
  RgnData^.rdh.nRgnSize := 0;
  RgnData^.rdh.rcBound := Rect(0, 0, B.Width, B.Height);
  // Update New Region
  Move(Rts, RgnData^.Buffer, Result);
  Result := SizeOf(TRgnDataHeader)+Count*SizeOf(TRect);
end;
       }

{==================================}
{== Generic DrawBorder Procedure ==}
{==================================}

function DrawBorder(bmp: TBitmap32; Bounds: TRect; Style: TFrameStyle; const
  Alpha: Byte = 255): TRect;
begin
  Result := DrawBorderSides( bmp, Bounds, Style, ALLFRAME_SIDES, Alpha );
end;


function DrawBorderSides(bmp: TBitmap32; Bounds: TRect; Style: TFrameStyle;
  Sides: TFrameSides; const Alpha: Byte = 255): TRect;
var
  ULColor, LRColor: TColor;
  ULColor32, LRColor32: TColor32;
  R: TRect;
begin
  ULColor := ULFrameColor[ Style ];
  LRColor := LRFrameColor[ Style ];

  ULColor32 := Color32(ULColor);
  LRColor32 := Color32(LRColor);
  TColor32Rec(ULColor32).rgbAlpha := Alpha;
  TColor32Rec(LRColor32).rgbAlpha := Alpha;

  { Draw the Frame }
  if Style <> fsNone then
  begin
    if Style in [ fsFlat, fsStatus, fsPopup ] then
      Bounds := DrawSides( bmp, Bounds, ULColor32, LRColor32, Sides, Alpha )
    else if Style in [ fsLowered, fsRaised ] then
      Bounds := DrawCtl3DBorderSides( bmp, Bounds, Style = fsLowered, Sides, Alpha )
    else if Style in [ fsButtonDown, fsButtonUp ] then
      Bounds := DrawButtonBorderSides( bmp, Bounds, Style = fsButtonDown, Sides, Alpha )
    else
    begin
      { Style must be fsGroove or fsBump }
      R := Bounds;
      { Fill in the gaps created by offsetting the rectangle }
      { Upper Right Gap }
      if sdRight in Sides then
        bmp.Pixels[ R.Right - 1, R.Top ] := LRColor32;
      if ( sdTop in Sides ) and not ( sdRight in Sides ) then
        bmp.Pixels[ R.Right - 1, R.Top ] := ULColor32;

      { Lower Left Gap }
      if sdBottom in Sides then
        bmp.Pixels[ R.Left, R.Bottom - 1 ] := LRColor32;
      if ( sdLeft in Sides ) and not ( sdBottom in Sides ) then
        bmp.Pixels[ R.Left, R.Bottom - 1 ] := ULColor32;

      { Upper Left Gaps }
      if ( sdTop in Sides ) and not ( sdLeft in Sides ) then
        bmp.Pixels[ R.Left, R.Top + 1 ] := LRColor32;
      if not ( sdTop in Sides ) and ( sdLeft in Sides ) then
        bmp.Pixels[ R.Left + 1, R.Top ] := LRColor32;

      { Lower Right Gaps }
      if ( sdBottom in Sides ) and not ( sdRight in Sides ) then
        bmp.Pixels[ R.Right - 1, R.Bottom - 2 ] := ULColor32;
      if not ( sdBottom in Sides ) and ( sdRight in Sides ) then
        bmp.Pixels[ R.Right - 2, R.Bottom - 1 ] := ULColor32;

      Inc( R.Left );
      Inc( R.Top );
      DrawSides( bmp, R, LRColor32, LRColor32, Sides, Alpha );
      OffsetRect( R, -1, -1 );
      DrawSides( bmp, R, ULColor32, ULColor32, Sides, Alpha );
      if sdLeft in Sides then
        Inc( Bounds.Left, 2 );
      if sdTop in Sides then
        Inc( Bounds.Top, 2 );
      if sdRight in Sides then
        Dec( Bounds.Right, 2 );
      if sdBottom in Sides then
        Dec( Bounds.Bottom, 2 );
    end;
  end;
  Result := Bounds;
end; {= DrawBorderSides =}




function DrawColorBorder(bmp: TBitmap32; Bounds: TRect; FaceColor: TColor32;
  Style: TFrameStyle; const Alpha: Byte = 255): TRect;
begin
  Result := DrawColorBorderSides( bmp, Bounds, FaceColor, Style, ALLFRAME_SIDES, Alpha );
end;


function DrawColorBorderSidesWithWidth(bmp: TBitmap32; Bounds: TRect;
  FaceColor: TColor32; Style: TFrameStyle; Sides: TFrameSides; const aWidth:
  integer; const Alpha: Byte = 255): TRect;
var
  I: integer;
begin
  Result := Bounds;
  for i := 1 to aWidth do
  begin
    Result := DrawColorBorderSides(bmp, Result, FaceColor,
    Style, Sides, Alpha);
  end;
end;

function DrawColorBorderSides(bmp: TBitmap32; Bounds: TRect; FaceColor:
  TColor32; Style: TFrameStyle; Sides: TFrameSides; const Alpha: Byte = 255):
  TRect;
var
  ULColor, LRColor: TColor32; 
  //ULColor32, LRColor32: TColor32; 
  C1, C2, C3, C4: TColor32;
  R: TRect;
  H, S, L: Byte;
begin
  RGBToHSL( FaceColor, H, S, L );

  { Draw the Frame }
  if Style <> fsNone then
  begin
    if Style in [ fsFlat, fsStatus, fsPopup ] then
    begin
      case Style of
        fsStatus:
        begin
          ULColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
          LRColor := HSLtoRGB( H, S, Min( L + 100, 255 ) );
        end;

        fsPopup:
        begin
          ULColor := HSLtoRGB( H, S, Min( L + 100, 255 ) );
          LRColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
        end;

        else { Style = fsFlat }
        begin
          ULColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
          LRColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
        end;
      end;
      Bounds := DrawSides( bmp, Bounds, ULColor, LRColor, Sides, Alpha)
    end
    else if Style in [ fsLowered, fsRaised ] then
    begin
      C1 := HSLtoRGB( H, S, Max( L - 50, 0 ) );     { Gray }
      C2 := HSLtoRGB( H, S, Max( L - 100, 0 ) );    { Black }
      C3 := HSLtoRGB( H, S, Min( L + 50, 255 ) );   { Silver }
      C4 := HSLtoRGB( H, S, Min( L + 100, 255 ) );  { White }
      if Style = fsLowered then
      begin
        Bounds := DrawSides( bmp, Bounds, C1, C4, Sides, Alpha );
        Bounds := DrawSides( bmp, Bounds, C2, C3, Sides, Alpha );
      end
      else
      begin
        Bounds := DrawSides( bmp, Bounds, C3, C2, Sides, Alpha );
        Bounds := DrawSides( bmp, Bounds, C4, C1, Sides, Alpha );
      end;
    end
    else if Style in [ fsButtonDown, fsButtonUp ] then
    begin
      Bounds := DrawColorButtonBorderSides( bmp, Bounds, FaceColor, 
        ( Style = fsButtonDown ), Sides, 1, Alpha );
    end
    else
    begin
      { Style must be fsGroove or fsBump }
      if Style = fsGroove then
      begin
        ULColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
        LRColor := HSLtoRGB( H, S, Min( L + 100, 255 ) );
      end
      else
      begin
        ULColor := HSLtoRGB( H, S, Min( L + 100, 255 ) );
        LRColor := HSLtoRGB( H, S, Max( L - 50, 0 ) );
      end;
      R := Bounds;
      //ULColor32 := Color32(ULColor);
      //LRColor32 := Color32(LRColor);
      TColor32Rec(ULColor).rgbAlpha := Alpha; 
      TColor32Rec(LRColor).rgbAlpha := Alpha; 

      { Fill in the gaps created by offsetting the rectangle }
      { Upper Right Gap }
      if sdRight in Sides then
        bmp.Pixels[ R.Right - 1, R.Top ] := LRColor;
      if ( sdTop in Sides ) and not ( sdRight in Sides ) then
        bmp.Pixels[ R.Right - 1, R.Top ] := ULColor;

      { Lower Left Gap }
      if sdBottom in Sides then
        bmp.Pixels[ R.Left, R.Bottom - 1 ] := LRColor;
      if ( sdLeft in Sides ) and not ( sdBottom in Sides ) then
        bmp.Pixels[ R.Left, R.Bottom - 1 ] := ULColor;

      { Upper Left Gaps }
      if ( sdTop in Sides ) and not ( sdLeft in Sides ) then
        bmp.Pixels[ R.Left, R.Top + 1 ] := LRColor;
      if not ( sdTop in Sides ) and ( sdLeft in Sides ) then
        bmp.Pixels[ R.Left + 1, R.Top ] := LRColor;

      { Lower Right Gaps }
      if ( sdBottom in Sides ) and not ( sdRight in Sides ) then
        bmp.Pixels[ R.Right - 1, R.Bottom - 2 ] := ULColor;
      if not ( sdBottom in Sides ) and ( sdRight in Sides ) then
        bmp.Pixels[ R.Right - 2, R.Bottom - 1 ] := ULColor;

      Inc( R.Left );
      Inc( R.Top );
      DrawSides( bmp, R, LRColor, LRColor, Sides, Alpha );
      OffsetRect( R, -1, -1 );
      DrawSides( bmp, R, ULColor, ULColor, Sides, Alpha );
      if sdLeft in Sides then
        Inc( Bounds.Left, 2 );
      if sdTop in Sides then
        Inc( Bounds.Top, 2 );
      if sdRight in Sides then
        Dec( Bounds.Right, 2 );
      if sdBottom in Sides then
        Dec( Bounds.Bottom, 2 );
    end;
  end;
  Result := Bounds;
end;

procedure LoadGraphic(src: TBitmap32; const aGraphic: TGraphic; const
  aDstRect:TRect);
var
  LBitmap32: TBitmap32;
begin
  LBitmap32 := TBitmap32.Create;
  try
    LBitmap32.Assign(aGraphic);
    LBitmap32.DrawTo(src, aDstRect);
  finally
    LBitmap32.Free;
  end;
end;

constructor TRectProperty.Create(const aOwner: TPersistent);
begin
  inherited Create;
  FOwner := aOwner;
end;

procedure TRectProperty.Assign(Source: TPersistent);
begin
  if Source is TRectProperty then
    with Source as TRectProperty do
    begin
        Self.FLeft := FLeft;
        Self.FRight := FRight;
        Self.FTop := FTop;
        Self.FBottom := FBottom;
        //Self.FTransparent := Transparent;
        //Self.FPictureTop.Assign(PictureTop);
        Self.Changed;
    end
  else
    inherited Assign(Source);
end;

procedure TRectProperty.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TRectProperty.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TRectProperty.GetValue: TRect;
begin
  Result.Left := FLeft;
  Result.Right := FRight;
  Result.Top := FTop;
  Result.Bottom := FBottom;
end;

procedure TRectProperty.SetBottom(const Value: Integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TRectProperty.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TRectProperty.SetRight(const Value: Integer);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TRectProperty.SetTop(const Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

constructor TCustomGraphicProperty.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := aOwner;
end;

procedure TCustomGraphicProperty.Assign(Source: TPersistent);
begin
  if Source is TCustomGraphicProperty then
  begin
    Self.BeginUpdate;
    try
      iAssign(Source);
    finally
      Self.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomGraphicProperty.BeginUpdate;
begin
  Inc(FUpdateCount);
  //if FUpdateCount = 1 then SetUpdating(True);
end;

procedure TCustomGraphicProperty.DoChanged;
begin
  if (FUpdateCount <= 0) and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TCustomGraphicProperty.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then
  begin
    if UpdatePended then
      Update
    else
      DoChanged;
  end;
  //if (FUpdateCount = 0) then
    //SetUpdating(False);
end;

function TCustomGraphicProperty.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TCustomGraphicProperty.iAssign(Source: TPersistent);
begin
  if Source is TCustomGraphicProperty then
    with Source as TCustomGraphicProperty do
    begin
      Self.Enabled := Enabled;
    end
end;

procedure TCustomGraphicProperty.InternalUpdate;
begin
end;

function TCustomGraphicProperty.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TCustomGraphicProperty.PaintTo(aSrc, aDst: TBitmap32; aR: TRect;
  aDstX, aDstY: integer);
begin
  //PaintTo(aBitmap32.Canvas, R);
end;

procedure TCustomGraphicProperty.PaintTo(aDst: TBitmap32; aR: TRect);
begin
  PaintTo(nil, aDst, aR, 0, 0);
end;

procedure TCustomGraphicProperty.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Update;
  end;
end;

procedure TCustomGraphicProperty.SetUpdating(Updating: Boolean);
begin
  if not Updating and UpdatePended then
    DoChanged;
end;

procedure TCustomGraphicProperty.Update;
begin
  UpdatePended := True;
  
  if FUpdateCount <> 0 then Exit;
  BeginUpdate;
  try
    UpdatePended := False;
    InternalUpdate;
  finally
    EndUpdate;
  end;
end;


end.
