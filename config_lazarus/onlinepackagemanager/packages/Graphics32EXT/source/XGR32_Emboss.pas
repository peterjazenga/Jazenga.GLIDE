
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Emboss;

interface

uses
  SysUtils, GR32;

type
  TColorChannel = (ccRed, ccGreen, ccBlue, ccIntensity);

  TFunctionMode = (fmBumpmap, fmEmboss);

  TEmbossFilter = class(TObject)
  private
    FLx  : Double;
    FLy  : Double;
    FLz  : Double;
    FNz  : Double;
    FNz2 : Double;
    FNzLz: Double;
    Fbg  : Double;
    procedure EmbossInit(const Azimuth, Elevation: Double; const Width45: Word);
    procedure EmbossRow(s1, s2, s3, Texture, Dst: PColor32Array;  const xSize: Cardinal);
  public
    FAzimuth  : Double;
    FElevation: Double;
    FDepth    : Integer;
    FEmbossP  : TFunctionMode;
    FPreview  : Boolean;
    FChannel  : TColorChannel;
    constructor Create;
    procedure Emboss(Dest: TBitmap32);
  end;

implementation

uses
  Math;

const
  PixelScale = 255.9;

function ARGBToGray(const AColor: TColor32): TColor32;
var
  a   : Cardinal;
  Gray: Integer;
begin
  a      := AColor and $FF000000;
  Gray   := Intensity(AColor);
  Result := a or (Gray shl 16) or (Gray shl 8) or Gray;
end; { ARGB }

constructor TEmbossFilter.Create;
begin
  inherited Create;

  FAzimuth   := 30.0;
  FElevation := 45.0;
  FDepth     := 20;
  FEmbossP   := fmEmboss;
  FPreview   := True;
  FChannel   := ccRed;
end; { Create }

procedure TEmbossFilter.EmbossInit(const Azimuth, Elevation: Double; const Width45: Word);
begin
  { compute the light vector from the input parameters.
    normalize the length to PixelScale for fast shading calculation. }

  FLx := Cos(Azimuth) * Cos(Elevation) * PixelScale;
  FLy := Sin(Azimuth) * Cos(Elevation) * PixelScale;
  FLz := Sin(Elevation) * PixelScale;

  { constant z component of image surface normal - this depends on the
    image slope we wish to associate with an angle of 45 degrees, which
    depends on the width of the filter used to produce the source image. }

  FNz   := (6 * 255) / Width45;
  FNz2  := FNz * FNz;
  FNzLz := FNz * FLz;

  // optimization for vertical normals: L.[0 0 1]
  Fbg := FLz;
end; { EmbossInit }

{ ANSI C code from the article
  "Fast Embossing Effects on Raster Image Data"
  by John Schlag, jfs@kerner.com
  in "Graphics Gems IV", Academic Press, 1994


  Emboss - shade 24-bit pixels using a single distant light source.
  Normals are obtained by differentiating a monochrome 'bump' image.
  The unary case ('texture' == NULL) uses the shading result as output.
  The binary case multiples the optional 'texture' image by the shade.
  Images are in row major order with interleaved color components (rgbrgb...).
  E.g., component c of pixel x,y of 'dst' is dst[3*(y*xSize + x) + c]. }

procedure TEmbossFilter.EmbossRow(s1, s2, s3, Texture, Dst: PColor32Array; const xSize: Cardinal);
var
  Nx, Ny, NdotL, x, Shade: Integer;
  a                      : Cardinal;
  r, g, b                : Byte;
begin
  // grayscale the first and last pixels of the scanline
  if FEmbossP = fmEmboss then
  begin
    Dst^[0]         := ARGBToGray(Dst^[0]);
    Dst^[xSize - 1] := ARGBToGray(Dst^[xSize - 1]);
  end;

  // mung pixels, avoiding edge pixels
  for x := 1 to xSize - 2 do
  begin
    { compute the normal from the src map. the type of the
      expression before the cast is compiler dependent. in
      some cases the sum is unsigned, in others it is
      signed. ergo, cast to signed. }

    case FChannel of
      ccRed: // the defualt
        begin
          Nx := RedComponent(s1^[x - 1]) +
                RedComponent(s2^[x - 1]) +
                RedComponent(s3^[x - 1]) -
                RedComponent(s1^[x + 1]) -
                RedComponent(s2^[x + 1]) -
                RedComponent(s3^[x + 1]);

          Ny := RedComponent(s3^[x - 1]) +
                RedComponent(s3^[x])     +
                RedComponent(s3^[x + 1]) -
                RedComponent(s1^[x - 1]) -
                RedComponent(s1^[x])     -
                RedComponent(s1^[x + 1]);
        end;

      ccGreen:
        begin
          Nx := GreenComponent(s1^[x - 1]) +
                GreenComponent(s2^[x - 1]) +
                GreenComponent(s3^[x - 1]) -
                GreenComponent(s1^[x + 1]) -
                GreenComponent(s2^[x + 1]) -
                GreenComponent(s3^[x + 1]);

          Ny := GreenComponent(s3^[x - 1]) +
                GreenComponent(s3^[x])     +
                GreenComponent(s3^[x + 1]) -
                GreenComponent(s1^[x - 1]) -
                GreenComponent(s1^[x])     -
                GreenComponent(s1^[x + 1]);
        end;

      ccBlue:
        begin
          Nx := BlueComponent(s1^[x - 1]) +
                BlueComponent(s2^[x - 1]) +
                BlueComponent(s3^[x - 1]) -
                BlueComponent(s1^[x + 1]) -
                BlueComponent(s2^[x + 1]) -
                BlueComponent(s3^[x + 1]);

          Ny := BlueComponent(s3^[x - 1]) +
                BlueComponent(s3^[x])     +
                BlueComponent(s3^[x + 1]) -
                BlueComponent(s1^[x - 1]) -
                BlueComponent(s1^[x])     -
                BlueComponent(s1^[x + 1]);
        end;

      ccIntensity:
        begin
          Nx := Intensity(s1^[x - 1]) +
                Intensity(s2^[x - 1]) +
                Intensity(s3^[x - 1]) -
                Intensity(s1^[x + 1]) -
                Intensity(s2^[x + 1]) -
                Intensity(s3^[x + 1]);

          Ny := Intensity(s3^[x - 1]) +
                Intensity(s3^[x])     +
                Intensity(s3^[x + 1]) -
                Intensity(s1^[x - 1]) -
                Intensity(s1^[x])     -
                Intensity(s1^[x + 1]);
        end;
    end;

    NdotL := Round(Nx * FLx + Ny * FLy + FNzLz);

    // shade with distant light source
    if   (Nx = 0) and (Ny = 0)
    then Shade := Round(Fbg)
    else
    if   NdotL < 0
    then Shade := 0
    else Shade := Round( NdotL / Sqrt(Nx * Nx + Ny * Ny + FNz2) );

    if   Shade < 0
    then Shade := 0
    else
    if   Shade > 255
    then Shade := 255;


    // preserve the alpha
    a := dst^[x] and $FF000000;

    // do something with the shading result
    if Assigned(Texture) then
    begin
      r := Texture^[x] shr 16 and $FF;
      g := Texture^[x] shr  8 and $FF;
      b := Texture^[x]        and $FF;

      r := r * Shade shr 8;
      g := g * Shade shr 8;
      b := b * Shade shr 8;
      
      dst^[x] := a or (r shl 16) or (g shl 8) or b;
    end
    else dst^[x] := a or (Shade shl 16) or (Shade shl 8) or Shade;
  end;
end; { EmbossRow }

procedure TEmbossFilter.Emboss(Dest: TBitmap32);
var
  SourceBmp                : TBitmap32;
  x, y                     : Integer;
  Width, Height            : Cardinal;
  SrcRow1, SrcRow2, SrcRow3: PColor32Array;
  DstRow, TextureRow       : PColor32Array;
begin
  Width  := Dest.Width;
  Height := Dest.Height;

  SourceBmp := TBitmap32.Create;
  try
    SourceBmp.Assign(Dest);
    SourceBmp.DrawMode := dmBlend;

    EmbossInit(DegToRad(FAzimuth), DegToRad(FElevation), FDepth);

    // first row
    SrcRow1 := SourceBmp.ScanLine[0];
    SrcRow2 := SourceBmp.ScanLine[1];
    SrcRow3 := SourceBmp.ScanLine[2];

    for x := 0 to Width - 1 do
      SrcRow1[x] := SrcRow2[x];

    case FEmbossP of
      fmBumpmap: TextureRow := SrcRow1;
      fmEmboss : TextureRow := nil;
    end;

    DstRow := Dest.ScanLine[0];

    EmbossRow(SrcRow1, SrcRow2, SrcRow3, TextureRow, DstRow, Width);

    // last row
    SrcRow1 := SourceBmp.ScanLine[Height - 3];
    SrcRow2 := SourceBmp.ScanLine[Height - 2];
    SrcRow3 := SourceBmp.ScanLine[Height - 1];

    for x := 0 to Width - 1 do
      SrcRow3[x] := SrcRow2[x];

    case FEmbossP of
      fmBumpmap: TextureRow := SrcRow1;
      fmEmboss : TextureRow := nil;
    end;

    DstRow := Dest.ScanLine[Height - 1];

    EmbossRow(SrcRow1, SrcRow2, SrcRow3, TextureRow, DstRow, Width);

    for y := 0 to Height - 3 do
    begin
      SrcRow1 := SourceBmp.ScanLine[y];
      SrcRow2 := SourceBmp.ScanLine[y + 1];
      SrcRow3 := SourceBmp.ScanLine[y + 2];

      DstRow  := Dest.ScanLine[y + 1];

      case FEmbossP of
        fmBumpmap: TextureRow := SrcRow1;
        fmEmboss : TextureRow := nil;
      end;

      EmbossRow(SrcRow1, SrcRow2, SrcRow3, TextureRow, DstRow, Width);
    end;

  finally
    SourceBmp.Free;
  end;
end; 

end.
