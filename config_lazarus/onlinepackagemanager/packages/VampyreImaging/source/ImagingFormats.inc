{%MainUnit ImagingCore.pas}



type
  TImageFormatInfoArray = array[TImageFormat] of PImageFormatInfo;
  PImageFormatInfoArray = ^TImageFormatInfoArray;


{ Additional image manipulation functions (usually used internally by Imaging unit) }

type
  { Color reduction operations.}
  TReduceColorsAction = (raCreateHistogram, raUpdateHistogram, raMakeColorMap,
    raMapImage);
  TReduceColorsActions = set of TReduceColorsAction;
const
  AllReduceColorsActions = [raCreateHistogram, raUpdateHistogram,
    raMakeColorMap, raMapImage];
{ Reduces the number of colors of source. Src is bits of source image
  (ARGB or floating point) and Dst is in some indexed format. MaxColors
  is the number of colors to which reduce and DstPal is palette to which
  the resulting colors are written and it must be allocated to at least
  MaxColors entries. ChannelMask is 'anded' with every pixel's channel value
  when creating color histogram. If $FF is used all 8bits of color channels
  are used which can be slow for large images with many colors so you can
  use  lower masks to speed it up.}
procedure ReduceColorsMedianCut(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; MaxColors: LongInt; ChannelMask: Byte;
  DstPal: PPalette32; Actions: TReduceColorsActions = AllReduceColorsActions);
{ Stretches rectangle in source image to rectangle in destination image
  using nearest neighbor filtering. It is fast but results look blocky
  because there is no interpolation used. SrcImage and DstImage must be
  in the same data format. Works for all data formats except special formats.}
procedure StretchNearest(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt);
type
  { Built-in sampling filters.}
  TSamplingFilter = (sfNearest, sfLinear, sfCosine, sfHermite, sfQuadratic,
    sfGaussian, sfSpline, sfLanczos, sfMitchell, sfCatmullRom);
  { Type of custom sampling function}
  TFilterFunction = function(Value: Single): Single;
const
  { Default resampling filter used for bicubic resizing.}
  DefaultCubicFilter = sfCatmullRom;
var
  { Built-in filter functions.}
  SamplingFilterFunctions: array[TSamplingFilter] of TFilterFunction;
  { Default radii of built-in filter functions.}
  SamplingFilterRadii: array[TSamplingFilter] of Single;

{ Stretches rectangle in source image to rectangle in destination image
  with resampling. One of built-in resampling filters defined by
  Filter is used. Set WrapEdges to True for seamlessly tileable images.
  SrcImage and DstImage must be in the same data format.
  Works for all data formats except special and indexed formats.}
procedure StretchResample(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TSamplingFilter; WrapEdges: Boolean = False); overload;
{ Stretches rectangle in source image to rectangle in destination image
  with resampling. You can use custom sampling function and filter radius.
  Set WrapEdges to True for seamlessly tileable images. SrcImage and DstImage
  must be in the same data format.
  Works for all data formats except special and indexed formats.}
procedure StretchResample(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TFilterFunction; Radius: Single;
  WrapEdges: Boolean = False); overload;
{ Helper for functions that create mipmap levels. BiggerLevel is
  valid image and SmallerLevel is empty zeroed image. SmallerLevel is created
  with Width and Height dimensions and it is filled with pixels of BiggerLevel
  using resampling filter specified by ImagingMipMapFilter option.
  Uses StretchNearest and StretchResample internally so the same image data format
  limitations apply.}
procedure FillMipMapLevel(const BiggerLevel: TImageData; Width, Height: LongInt;
  var SmallerLevel: TImageData);


{ Various helper & support functions }

{ Copies Src pixel to Dest pixel. It is faster than System.Move procedure.}
procedure CopyPixel(Src, Dest: Pointer; BytesPerPixel: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Compares Src pixel and Dest pixel. It is faster than SysUtils.CompareMem function.}
function ComparePixels(PixelA, PixelB: Pointer; BytesPerPixel: LongInt): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Translates pixel color in SrcFormat to DstFormat.}
procedure TranslatePixel(SrcPixel, DstPixel: Pointer; SrcFormat,
  DstFormat: TImageFormat; SrcPalette, DstPalette: PPalette32);
{ Clamps floating point pixel channel values to [0.0, 1.0] range.}
procedure ClampFloatPixel(var PixF: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Helper function that converts pixel in any format to 32bit ARGB pixel.
  For common formats it's faster than calling GetPixel32 etc.}
procedure ConvertToPixel32(SrcPix: PByte; DestPix: PColor32Rec;
  const SrcInfo: TImageFormatInfo; SrcPalette: PPalette32 = nil); {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Adds padding bytes at the ends of scanlines. Bpp is the number of bytes per
  pixel of source and WidthBytes is the number of bytes per scanlines of dest.}
procedure AddPadBytes(DataIn: Pointer; DataOut: Pointer; Width, Height,
  Bpp, WidthBytes: LongInt);
{ Removes padding from image with scanlines that have aligned sizes. Bpp is
  the number of bytes per pixel of dest and WidthBytes is the number of bytes
  per scanlines of source.}
procedure RemovePadBytes(DataIn: Pointer; DataOut: Pointer; Width, Height,
  Bpp, WidthBytes: LongInt);

{ Converts 1bit image data to 8bit. Used mostly by file loaders for formats
  supporting 1bit images. Scaling of pixel values to 8bits is optional
  (indexed formats don't need this).}
procedure Convert1To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
{ Converts 2bit image data to 8bit. Used mostly by file loaders for formats
  supporting 2bit images. Scaling of pixel values to 8bits is optional
  (indexed formats don't need this).}
procedure Convert2To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
{ Converts 4bit image data to 8bit. Used mostly by file loaders for formats
  supporting 4bit images. Scaling of pixel values to 8bits is optional
  (indexed formats don't need this).}
procedure Convert4To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);

{ Helper function for image file loaders. Some 15 bit images (targas, bitmaps)
  may contain 1 bit alpha but there is no indication of it. This function checks
  all 16 bit(should be X1R5G5B5 or A1R5G5B5 format) pixels and some of them have
  alpha bit set it returns True, otherwise False.}
function Has16BitImageAlpha(NumPixels: LongInt; Data: PWord): Boolean;
{ Helper function for image file loaders. This function checks is similar
  to Has16BitImageAlpha but works with A8R8G8B8/X8R8G8B8 format.}
function Has32BitImageAlpha(NumPixels: LongInt; Data: PLongWord): Boolean;
{ Checks if there is any relevant alpha data (any entry has alpha <> 255)
  in the given palette.}
function PaletteHasAlpha(Palette: PPalette32; PaletteEntries: Integer): Boolean;
{ Checks if given palette has only grayscale entries.}
function PaletteIsGrayScale(Palette: PPalette32; PaletteEntries: Integer): Boolean;

{ Provides indexed access to each line of pixels. Does not work with special
  format images.}
function GetScanLine(ImageBits: Pointer; const FormatInfo: TImageFormatInfo;
  LineWidth, Index: LongInt): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns True if Format is valid image data format identifier.}
function IsImageFormatValid(Format: TImageFormat): Boolean;

{ Converts 16bit half floating point value to 32bit Single.}
function HalfToFloat(Half: THalfFloat): Single;
{ Converts 32bit Single to 16bit half floating point.}
function FloatToHalf(Float: Single): THalfFloat;

{ Converts half float color value to single-precision floating point color.}
function ColorHalfToFloat(ColorHF: TColorHFRec): TColorFPRec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Converts single-precision floating point color to half float color.}
function ColorFloatToHalf(ColorFP: TColorFPRec): TColorHFRec; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Converts ARGB color to grayscale. }
function Color32ToGray(Color32: TColor32): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Makes image PalEntries x 1 big where each pixel has color of one pal entry.}
procedure VisualizePalette(Pal: PPalette32; Entries: Integer; out PalImage: TImageData);

type
  TPointRec = record
    Pos: LongInt;
    Weight: Single;
  end;
  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

{ Helper function for resampling.}
function BuildMappingTable(DstLow, DstHigh, SrcLow, SrcHigh, SrcImageWidth: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean): TMappingTable;
{ Helper function for resampling.}
procedure FindExtremes(const Map: TMappingTable; var MinPos, MaxPos: LongInt);


{ Pixel readers/writers for different image formats }

{ Returns pixel of image in any ARGB format. Channel values are scaled to 16 bits.}
procedure ChannelGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Pix: TColor64Rec);
{ Sets pixel of image in any ARGB format. Channel values must be scaled to 16 bits.}
procedure ChannelSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Pix: TColor64Rec);

{ Returns pixel of image in any grayscale format. Gray value is scaled to 64 bits
  and alpha to 16 bits.}
procedure GrayGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Gray: TColor64Rec; var Alpha: Word);
{ Sets pixel of image in any grayscale format. Gray value must be scaled to 64 bits
  and alpha to 16 bits.}
procedure GraySetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Gray: TColor64Rec; Alpha: Word);

{ Returns pixel of image in any floating point format. Channel values are
  in range <0.0, 1.0>.}
procedure FloatGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Pix: TColorFPRec);
{ Sets pixel of image in any floating point format. Channel values must be
  in range <0.0, 1.0>.}
procedure FloatSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Pix: TColorFPRec);

{ Returns pixel of image in any indexed format. Returned value is index to
  the palette.}
procedure IndexGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Index: LongWord);
{ Sets pixel of image in any indexed format. Index is index to the palette.}
procedure IndexSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  Index: LongWord);


{ Pixel readers/writers for 32bit and FP colors}

{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to 32 bit ARGB.}
function GetPixel32Generic(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32): TColor32Rec;
{ Procedure for setting pixel colors. Input 32 bit ARGB color is translated to
    native format and then written to Image.}
procedure SetPixel32Generic(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32; const Color: TColor32Rec);
{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to FP ARGB.}
function GetPixelFPGeneric(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32): TColorFPRec;
{ Procedure for setting pixel colors. Input FP ARGB color is translated to
    native format and then written to Image.}
procedure SetPixelFPGeneric(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32; const Color: TColorFPRec);


{ Image format conversion functions }

{ Converts any ARGB format to any ARGB format.}
procedure ChannelToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any ARGB format to any grayscale format.}
procedure ChannelToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any ARGB format to any floating point format.}
procedure ChannelToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any ARGB format to any indexed format.}
procedure ChannelToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);

{ Converts any grayscale format to any grayscale format.}
procedure GrayToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any grayscale format to any ARGB format.}
procedure GrayToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any grayscale format to any floating point format.}
procedure GrayToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any grayscale format to any indexed format.}
procedure GrayToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);

{ Converts any floating point format to any floating point format.}
procedure FloatToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any floating point format to any ARGB format.}
procedure FloatToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any floating point format to any grayscale format.}
procedure FloatToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any floating point format to any indexed format.}
procedure FloatToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);

{ Converts any indexed format to any indexed format.}
procedure IndexToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal, DstPal: PPalette32);
{ Converts any indexed format to any ARGB format.}
procedure IndexToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
{ Converts any indexed format to any grayscale format.}
procedure IndexToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
{ Converts any indexed format to any floating point  format.}
procedure IndexToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);


{ Color constructor functions }

{ Constructs TColor24Rec color.}
function Color24(R, G, B: Byte): TColor24Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColor32Rec color.}
function Color32(A, R, G, B: Byte): TColor32Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColor48Rec color.}
function Color48(R, G, B: Word): TColor48Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColor64Rec color.}
function Color64(A, R, G, B: Word): TColor64Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColorFPRec color.}
function ColorFP(A, R, G, B: Single): TColorFPRec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColorHFRec color.}
function ColorHF(A, R, G, B: THalfFloat): TColorHFRec; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Special formats conversion functions }

{ Converts image to/from/between special image formats (dxtc, ...).}
procedure ConvertSpecial(var Image: TImageData; SrcInfo,
  DstInfo: PImageFormatInfo);


{ Inits all image format information. Called internally on startup.}
procedure InitImageFormats(var Infos: TImageFormatInfoArray);

const
  // Grayscale conversion channel weights
  GrayConv: TColorFPRec = (B: 0.114; G: 0.587; R: 0.299; A: 0.0);

  // Contants for converting integer colors to floating point
  OneDiv8Bit: Single = 1.0 / 255.0;
  OneDiv16Bit: Single = 1.0 / 65535.0;