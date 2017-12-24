
{**********************************************************************
 Package pl_AGGPasVS.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com)
***********************************************************************}
unit agx_canvas;

interface

{$I agg_vsmode.inc }

uses
 agg_basics ,
 agg_array ,
 agg_trans_affine ,
 agg_trans_viewport ,
 agg_path_storage ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_conv_curve ,
 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_span_gradient ,
 agg_span_image_filter_rgba ,
 agg_span_image_resample_rgba ,
 agg_span_converter ,
 agg_span_interpolator_linear ,
 agg_span_allocator ,
 agg_rasterizer_scanline_aa ,
 agg_gamma_functions ,
 agg_scanline_u ,
 agg_arc ,
 agg_bezier_arc ,
 agg_rounded_rect ,
 agg_font_engine ,
 agg_font_cache_manager ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_rgba ,
 agg_color ,
 agg_math_stroke ,
 agg_image_filters ,
 agg_vertex_source ,
 agg_render_scanlines ,

 {$IFDEF AGG2D_USE_FREETYPE}
  agg_font_freetype ,
 {$ENDIF }

 {$IFDEF AGG2D_USE_WINFONTS}
  agg_font_win32_tt ,
  Windows ,
 {$ENDIF }

 Math, SysUtils, Variants, Classes, Controls,
 FPimage,Graphics,IntfGraphics;

const
// LineJoin
 AGX_JoinMiter = miter_join;
 AGX_JoinRound = round_join;
 AGX_JoinBevel = bevel_join;

// LineCap
 AGX_CapButt   = butt_cap;
 AGX_CapSquare = square_cap;
 AGX_CapRound  = round_cap;

// TextAlignment
 AGX_AlignLeft   = 0;
 AGX_AlignRight  = 1;
 AGX_AlignCenter = 2;
 AGX_AlignTop    = AGX_AlignRight;
 AGX_AlignBottom = AGX_AlignLeft;

// BlendMode
 AGX_BlendAlpha      = end_of_comp_op_e;
 AGX_BlendClear      = comp_op_clear;
 AGX_BlendSrc        = comp_op_src;
 AGX_BlendDst        = comp_op_dst;
 AGX_BlendSrcOver    = comp_op_src_over;
 AGX_BlendDstOver    = comp_op_dst_over;
 AGX_BlendSrcIn      = comp_op_src_in;
 AGX_BlendDstIn      = comp_op_dst_in;
 AGX_BlendSrcOut     = comp_op_src_out;
 AGX_BlendDstOut     = comp_op_dst_out;
 AGX_BlendSrcAtop    = comp_op_src_atop;
 AGX_BlendDstAtop    = comp_op_dst_atop;
 AGX_BlendXor        = comp_op_xor;
 AGX_BlendAdd        = comp_op_plus;
 AGX_BlendSub        = comp_op_minus;
 AGX_BlendMultiply   = comp_op_multiply;
 AGX_BlendScreen     = comp_op_screen;
 AGX_BlendOverlay    = comp_op_overlay;
 AGX_BlendDarken     = comp_op_darken;
 AGX_BlendLighten    = comp_op_lighten;
 AGX_BlendColorDodge = comp_op_color_dodge;
 AGX_BlendColorBurn  = comp_op_color_burn;
 AGX_BlendHardLight  = comp_op_hard_light;
 AGX_BlendSoftLight  = comp_op_soft_light;
 AGX_BlendDifference = comp_op_difference;
 AGX_BlendExclusion  = comp_op_exclusion;
 AGX_BlendContrast   = comp_op_contrast;

{ TYPES DEFINITION }
type
 PDouble = ^double;

 PAgxColor = ^TAgxColor;
 TAgxColor = rgba8;

 TAgxRectD = AGG_basics.rect_d;
 TAgxAffine = trans_affine;
 PAgxAffine = trans_affine_ptr;
 TAgxFontRasterizer = gray8_adaptor_type;
 PAgxFontRasterizer = gray8_adaptor_type_ptr;
 TAgxFontScanline = gray8_scanline_type;
 PAgxFontScanline = gray8_scanline_type_ptr;

{$IFDEF AGG2D_USE_FREETYPE}
 TAgxFontEngine = font_engine_freetype_int32;
{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}
 TAgxFontEngine = font_engine_win32_tt_int32;
{$ENDIF }

 TAgxGradient  = (AGX_Solid ,AGX_Linear ,AGX_Radial);
 TAgxDirection = (AGX_CW, AGX_CCW);

 TAgxLineJoin  = int;
 TAgxLineCap   = int;
 TAgxBlendMode = comp_op_e;

 TAgxTextAlignment = int;

 TAgxDrawPathFlag = (
  AGX_FillOnly ,
  AGX_StrokeOnly ,
  AGX_FillAndStroke ,
  AGX_FillWithLineColor );

 TAgxViewportOption = (
  AGX_Anisotropic ,
  AGX_XMinYMin ,
  AGX_XMidYMin ,
  AGX_XMaxYMin ,
  AGX_XMinYMid ,
  AGX_XMidYMid ,
  AGX_XMaxYMid ,
  AGX_XMinYMax ,
  AGX_XMidYMax ,
  AGX_XMaxYMax );

 TAgxImageFilter = (
  AGX_NoFilter ,
  AGX_Bilinear ,
  AGX_Hanning ,
  AGX_Hermite ,
  AGX_Quadric ,
  AGX_Bicubic ,
  AGX_Catrom ,
  AGX_Spline16 ,
  AGX_Spline36 ,
  AGX_Blackman144 );

 TAgxImageResample = (
  AGX_NoResample ,
  AGX_ResampleAlways ,
  AGX_ResampleOnZoomOut );


   TPolyPoints=array of TPoint;
   PTPolyPoints = ^TPolyPoints;

 TAgxFontCacheType=(AGX_RasterFontCache,AGX_VectorFontCache);

 PAgxTransformations = ^TAgxTransformations;
 TAgxTransformations = record
   affineMatrix : array[0..5] of double;
  end;

 TAgxRasterizerGamma = object(vertex_source )
   m_alpha : gamma_multiply;
   m_gamma : gamma_power;
   constructor Construct(alpha ,gamma : double );
   function func_operator_gamma(x : double ) : double; virtual;
  end;

 PAgxImage = ^TAgxImage;
 TAgxImage = object
   renBuf:rendering_buffer;

   fLinkIntfImage:TLazIntfImage;
   fLinkBmp:TBitmap;
   constructor Construct;
   destructor  Destruct;
   function  attach(bitmap : TBitmap; flip : boolean ) : boolean;
   function  width : int;
   function  height : int;
  end;

TAgxCanvas = class(TPersistent)
  private
  protected
   m_rbuf : rendering_buffer;
   m_pixf : TPixelFormat;
   m_pixFormat ,m_pixFormatComp ,m_pixFormatPre ,m_pixFormatCompPre : pixel_formats;
   m_renBase   ,m_renBaseComp   ,m_renBasePre   ,m_renBaseCompPre   : renderer_base;
   m_renSolid ,m_renSolidComp : renderer_scanline_aa_solid;
   m_allocator : span_allocator;
   m_clipBox   : TAgxRectD;
   m_blendMode ,m_imageBlendMode : TAgxBlendMode;
   m_imageBlendColor : TAgxColor;
   m_scanline   : scanline_u8;
   m_rasterizer : rasterizer_scanline_aa;
   m_masterAlpha ,m_antiAliasGamma : double;
   m_fillColor ,m_lineColor : TAgxColor;
   m_fillGradient ,m_lineGradient : pod_auto_array;
   m_lineCap  : TAgxLineCap;
   m_lineJoin : TAgxLineJoin;
   m_fillGradientFlag ,m_lineGradientFlag : TAgxGradient;
   m_fillGradientMatrix ,m_lineGradientMatrix : trans_affine;
   m_fillGradientD1 ,
   m_lineGradientD1 ,
   m_fillGradientD2 ,
   m_lineGradientD2 ,
   m_textAngle      : double;
   m_textAlignX     ,
   m_textAlignY     : TAgxTextAlignment;
   m_textHints      : boolean;
   m_fontHeight     ,
   m_fontAscent     ,
   m_fontDescent    : double;
   m_fontWeight     : int;
   m_fontFileName   : AnsiString;
   m_fontItalic     : boolean;
   m_fontCacheType  : TAgxFontCacheType;
   m_imageFilter    : TAgxImageFilter;
   m_imageResample  : TAgxImageResample;
   m_imageFilterLut : image_filter_lut;
   m_fillGradientInterpolator ,
   m_lineGradientInterpolator : span_interpolator_linear;
   m_linearGradientFunction : gradient_x;
   m_radialGradientFunction : gradient_circle;
   m_lineWidth   : double;
   m_evenOddFlag : boolean;
   m_path      : path_storage;
   m_transform : trans_affine;
   m_convCurve  : conv_curve;
   m_convStroke : conv_stroke;
   m_pathTransform ,m_strokeTransform : conv_transform;
   m_imageFlip : boolean;

   {$IFNDEF AGG2D_NO_FONT}
   m_fontEngine       : TAgxFontEngine;
   m_fontCacheManager : font_cache_manager;
   {$ENDIF }

   {$IFDEF AGG2D_USE_WINFONTS}
    m_fontDC : HDC;
   {$ENDIF }
  // Other Pascal-specific members
   m_gammaNone  : gamma_none;
   m_gammaAgg2D : TAgxRasterizerGamma;
   m_ifBilinear    : image_filter_bilinear;
   m_ifHanning     : image_filter_hanning;
   m_ifHermite     : image_filter_hermite;
   m_ifQuadric     : image_filter_quadric;
   m_ifBicubic     : image_filter_bicubic;
   m_ifCatrom      : image_filter_catrom;
   m_ifSpline16    : image_filter_spline16;
   m_ifSpline36    : image_filter_spline36;
   m_ifBlackman144 : image_filter_blackman144;

   //..........................................
   fLinkIntfImage:TLazIntfImage;
   fLinkBmp:TBitmap;
   fWidth , fHeight : integer;

   procedure render(const fillColor_ : boolean ); overload;
   procedure render(ras : PAgxFontRasterizer; sl : PAgxFontScanline ); overload;
   procedure addLine(const x1 ,y1 ,x2 ,y2 : double );
   procedure updateRasterizerGamma;
   procedure renderImage(img:PAgxImage; const x1 ,y1 ,x2 ,y2 : integer; const parl : PDouble );
   procedure FontEngineCreate;
  public
   constructor Create; virtual;
   destructor Destroy; override;
   function  Attach(Bitmap : TBitmap; const flip_y:boolean=false):boolean; virtual;
   procedure Invalidate;
   //............ Color Functions ................................
   Function  ColorAgxToColor(const c:TAgxColor):TColor;
   Function  ColorToColorAgx(const c:TColor):TAgxColor;
   Function  FPColorToColorAgx(const c:TFPColor):TAgxColor;
   Function  ColorAgxToFPColor(const c:TAgxColor):TFPColor;
   //.............................................................
   procedure ClearAll(const c : TFPColor ); overload;
   procedure ClearAll(const c : TAgxColor ); overload;
   procedure ClearAll(const c : TColor );  overload;
   procedure ClearAll(const r ,g ,b : byte; const a : byte = 255 ); overload;
  //..........Master Rendering Properties ........................
   procedure BlendModeSet(m : TAgxBlendMode );
   function  BlendModeGet: TAgxBlendMode;
   procedure MasterAlphaSet(a : double );
   function  MasterAlphaGet:double;
   procedure AntiAliasGammaSet(g : double );
   function  AntiAliasGammaGet : double;
   //...... Fill .................................................
   procedure NoFill;
   procedure FillColorSet(const c : TAgxColor ); overload;
   procedure FillColorSet(const r ,g ,b : byte; const a : byte = 255 ); overload;
   function  FillColorGet: TAgxColor;
   procedure FillEvenOddSet(const evenOddFlag : boolean );
   function  FillEvenOddGet : boolean;
   procedure FillLinearGradient(const x1 ,y1 ,x2 ,y2 : double;const  c1 ,c2 : TAgxColor; const profile : double = 1.0 );
   procedure FillRadialGradient(const x ,y ,r : double; const c1 ,c2 : TAgxColor; const profile : double = 1.0 ); overload;
   procedure FillRadialGradient(const x ,y ,r : double; const c1 ,c2 ,c3 : TAgxColor ); overload;
   procedure FillRadialGradient(const x ,y ,r : double ); overload;
   //..............Line ............................................
   procedure NoLine;
   procedure LineColorSet(const c : TAgxColor ); overload;
   procedure LineColorSet(const r ,g ,b : byte; const a : byte = 255 ); overload;
   function  LineColorGet : TAgxColor;
   procedure LineWidthSet(const w : double );
   function  LineWidthGet : double;
   procedure LineCapSet(const cap : TAgxLineCap );
   function  LineCapGet: TAgxLineCap;
   procedure LineJoinSet(const join : TAgxLineJoin );
   function  LineJoinGet: TAgxLineJoin;
   procedure LineLinearGradient(const x1 ,y1 ,x2 ,y2 : double; const c1 ,c2 : TAgxColor; const profile : double = 1.0 );
   procedure LineRadialGradient(const x ,y ,r : double; const c1 ,c2 : TAgxColor; const profile : double = 1.0 ); overload;
   procedure LineRadialGradient(const x ,y ,r : double; const c1 ,c2 ,c3 : TAgxColor ); overload;
   procedure LineRadialGradient(const x ,y ,r : double ); overload;

  //----------------- Affine Transformations --------------------------
   procedure TransformationsReset;
   procedure TransformationsSet(const tr : PAgxTransformations );
   function  TransformationsGet: TAgxTransformations;

   procedure Affine(tr : PAgxAffine ); overload;
   procedure Affine(tr : PAgxTransformations ); overload;

   procedure Rotate(const angle : double );
   procedure Scale(const sx ,sy : double );
   procedure Skew(const sx ,sy : double );
   procedure Translate(const x ,y : double );
   procedure Parallelogram(const x1 ,y1 ,x2 ,y2 : double; const para : PDouble );

   procedure Viewport(const worldX1  ,worldY1  ,worldX2  ,worldY2 ,
                      screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
                      const opt : TAgxViewportOption = AGX_XMidYMid );

  //--------------------- Coordinates Conversions -------------------------------
   procedure WorldToScreen(const x ,y : PDouble ); overload;
   procedure ScreenToWorld(const x ,y : PDouble ); overload;
   function  WorldToScreen(const scalar : double ) : double; overload;
   function  ScreenToWorld(const scalar : double ) : double; overload;
   procedure AlignPoint(const x ,y : PDouble );
  //-------------------- Clipping ----------------------------------------------
   procedure ClipBoxSet(const x1 ,y1 ,x2 ,y2 : double );
   function  ClipBoxGet : TAgxRectD;
   procedure ClearClipBox(const c : TAgxColor ); overload;
   procedure ClearClipBox(const r ,g ,b : byte; const a : byte = 255 ); overload;
   function  InBox(const worldX ,worldY : double ) : boolean;
  //----------------- Basic Shapes --------------------------------------------
   procedure Line     (const x1 ,y1 ,x2 ,y2 : double );
   procedure Triangle (const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
   procedure Rectangle(const x1 ,y1 ,x2 ,y2 : double );
   procedure RoundedRect(const x1 ,y1 ,x2 ,y2 ,r : double ); overload;
   procedure RoundedRect(const x1 ,y1 ,x2 ,y2 ,rx ,ry : double ); overload;
   procedure RoundedRect(const x1 ,y1 ,x2 ,y2 , rxBottom ,ryBottom , rxTop ,ryTop:double ); overload;
   procedure Ellipse(const cx ,cy ,rx ,ry : double );
   procedure Arc (const cx ,cy ,rx ,ry ,start ,sweep : double );
   procedure Star(const cx ,cy ,r1 ,r2 ,startAngle : double;const  numRays : integer );
   procedure Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double ); overload;
   procedure Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;
   procedure Polygon(xy : PDouble; const numPoints : integer );
   procedure PolygonInt(const Points:TPolyPoints);overload;
   procedure PolygonInt(const Points:array of Tpoint;PointsNum:integer);overload;
   procedure Polyline(xy : PDouble; const numPoints : integer );
   procedure PolylineInt(const Points:TPolyPoints);overload;
   procedure PolylineInt(const Points:array of Tpoint;PointsNum:integer);overload;

  //---------------- Path Commands ----------------------------------------
   procedure ResetPath;
   procedure PathGetBoundRect(var x1,y1,x2,y2:double);

   procedure PathFillWithImage(Img:Tbitmap); overload;

   procedure MoveTo (const x ,y : double );
   procedure MoveRel(const dx ,dy : double );

   procedure LineTo (const x ,y : double );
   procedure LineRel(const dx ,dy : double );

   procedure HorLineTo (const x : double );
   procedure HorLineRel(const dx : double );

   procedure VerLineTo (const y : double );
   procedure VerLineRel(const dy : double );

   procedure ArcTo(const rx,ry,angle:double; const largeArcFlag,sweepFlag:boolean; const x,y:double);
   procedure ArcRel(const rx,ry,angle:double; const largeArcFlag,sweepFlag:boolean; const dx,dy : double);

   procedure QuadricCurveTo (const xCtrl ,yCtrl ,xTo ,yTo : double ); overload;
   procedure QuadricCurveRel(const dxCtrl ,dyCtrl ,dxTo ,dyTo : double ); overload;
   procedure QuadricCurveTo (const xTo ,yTo : double ); overload;
   procedure QuadricCurveRel(const dxTo ,dyTo : double ); overload;

   procedure CubicCurveTo (const xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;
   procedure CubicCurveRel(const dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double ); overload;
   procedure CubicCurveTo (const xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;
   procedure CubicCurveRel(const dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double ); overload;

   procedure AddEllipse(const cx ,cy ,rx ,ry : double; const dir : TAgxDirection );
   procedure ClosePolygon;

   procedure DrawPath(const flag : TAgxDrawPathFlag = AGX_FillAndStroke );

  //-------------- Text Rendering --------------------------------
   function  FontUsesFreeType : boolean;
   function  FontUsesWin32TrueType: boolean;

   procedure FlipText(const flip:boolean);

   procedure Font(const FileName:AnsiString;
                  const height:double;
                  const bold:boolean=false;
                  const italic:boolean=false;
                  const cache:TAgxFontCacheType=AGX_VectorFontCache;
                  const angle:double=0.0);

   procedure FontEx(const FileName:AnsiString;
                    const height:double;
                    const Weight:int=400;
                    const italic:boolean=false;
                    const cache:TAgxFontCacheType=AGX_VectorFontCache;
                    const angle:double=0.0);

   procedure FontBoldSet(const val : Boolean);
   function  FontBoldGet:Boolean;

   procedure FontItalicSet(const val : Boolean);
   function  FontItalicGet:Boolean;

   procedure FontHeightSet(const val : double);
   function  FontHeightGet : double;

   procedure FontFileNameSet(const val : AnsiString);
   function  FontFileNameGet : AnsiString;

   procedure FontWeightSet(const val : int);
   function  FontWeightGet : int;

   procedure TextHintsSet(const hints : boolean );
   function  TextHintsGet : boolean;

   procedure TextAngleSet(const val:double);
   function  TextAngleGet:double;

   procedure TextAlignmentSet(const alignX ,alignY : TAgxTextAlignment );

   function  TextWidthGet(const str : AnsiString ) : double;
   function  TextHeightGet: double;

   procedure Text(const X,Y:double; const str:AnsiString; const roundOff:boolean=false; const ddx:double=0.0; const ddy:double=0.0);

  //-----------  Image Rendering -------------------------------------------------
   procedure ImageFilterSet(const f : TAgxImageFilter );
   function  ImageFilterGet: TAgxImageFilter;
   procedure ImageResampleSet(const f : TAgxImageResample ); overload;
   function  ImageResampleGet: TAgxImageResample; overload;
   procedure ImageFlip(const f : boolean );
    //.................TBitmap Rendering ...................................
   procedure TransformImage(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const dstX1,dstY1,dstX2,dstY2:double); overload;
   procedure TransformImage(bitmap:TBitmap; const dstX1,dstY1,dstX2,dstY2:double); overload;
   procedure TransformImage(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const parallelo:PDouble); overload;
   procedure TransformImage(bitmap:TBitmap; const parallelo:PDouble); overload;

   procedure TransformImagePath(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const dstX1,dstY1,dstX2,dstY2:double); overload;
   procedure TransformImagePath(bitmap:TBitmap; const dstX1,dstY1,dstX2,dstY2:double); overload;
   procedure TransformImagePath(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const parallelo:PDouble); overload;
   procedure TransformImagePath(bitmap:TBitmap; const parallelo : PDouble); overload;

   procedure CopyImage(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const dstX,dstY:double ); overload;
   procedure CopyImage(bitmap:TBitmap; const dstX,dstY : double ); overload;

   //------------- Properties ----------------------------------------------
   Property LinkIntfImage: TLazIntfImage read fLinkIntfImage;
   property LinkBmp: TBitmap read fLinkBmp;
   property Height: Integer read FHeight;
   property Width: Integer read FWidth;
  end;


implementation

var
 g_approxScale : double = 2.0;

//==================================================================================================
type
 PAggSpanConvImageBlend = ^TAggSpanConvImageBlend;
 TAggSpanConvImageBlend = object(span_convertor )
  private
   m_mode  : TAgxBlendMode;
   m_color : TAgxColor;
   m_pixel : pixel_formats_ptr; // m_pixFormatCompPre
  public
   constructor Construct(m : TAgxBlendMode; c : TAgxColor; p : pixel_formats_ptr );
   procedure convert(span : aggclr_ptr; x ,y : int; len : unsigned ); virtual;
  end;

constructor TAggSpanConvImageBlend.Construct(m : TAgxBlendMode; c : TAgxColor; p : pixel_formats_ptr );
begin
 m_mode :=m;
 m_color:=c;
 m_pixel:=p;
end;

procedure TAggSpanConvImageBlend.convert(span : aggclr_ptr; x ,y : int; len : unsigned );
var
 l2 ,a : unsigned;
 s2 : PAgxColor;
begin
 if (m_mode <> AGX_BlendDst ) and
    (m_pixel <> NIL ) then
  begin
   l2:=len;
   s2:=PAgxColor(span );

   repeat
    comp_op_adaptor_clip_to_dst_rgba_pre(
     m_pixel ,
     unsigned(m_mode ) ,
     int8u_ptr(s2 ) ,
     m_color.r ,
     m_color.g ,
     m_color.b ,
     base_mask ,
     cover_full );

    inc(ptrcomp(s2 ) ,sizeof(aggclr ) );
    dec(l2 );

   until l2 = 0;

  end;

 if m_color.a < base_mask then
  begin
   l2:=len;
   s2:=PAgxColor(span );
   a :=m_color.a;

   repeat
    s2^.r:=(s2^.r * a ) shr base_shift;
    s2^.g:=(s2^.g * a ) shr base_shift;
    s2^.b:=(s2^.b * a ) shr base_shift;
    s2^.a:=(s2^.a * a ) shr base_shift;

    inc(ptrcomp(s2 ) ,sizeof(aggclr ) );
    dec(l2 );

   until l2 = 0;

  end;

end;

//========================= TAggImage =====================================================
constructor TAgxImage.Construct;
begin
 renBuf.Construct;
 fLinkIntfImage:=nil;
 fLinkBmp:=nil;
end;

destructor TAgxImage.Destruct;
begin
 if fLinkIntfImage<>nil then fLinkIntfImage.Free;
 renBuf.Destruct;
end;

function TAgxImage.attach(bitmap : TBitmap; flip : boolean ) : boolean;
var buffer : pointer;
    stride : integer;
begin
 result:=false;
  if fLinkIntfImage<>nil then FreeAndNil(fLinkIntfImage);
  fLinkBmp:=bitmap;

  if bitmap=nil then exit;
  fLinkIntfImage:=bitmap.CreateIntfImage;

 if Assigned(bitmap ) and not bitmap.Empty then
  case bitmap.PixelFormat of
   pf32bit :
    begin
    stride:=integer(fLinkIntfImage.GetDataLineStart(1) - integer(fLinkIntfImage.GetDataLineStart(0)));

     if stride < 0 then
      buffer:=fLinkIntfImage.GetDataLineStart(bitmap.Height - 1) else
      buffer:=fLinkIntfImage.GetDataLineStart(0);

     if flip then  stride:=stride * -1;

     renBuf.attach(buffer , bitmap.Width , bitmap.Height,stride);
     result:=true;
    end;
  end;
end;

function TAgxImage.width : int;
begin
 result:=renBuf._width;
end;

function TAgxImage.height : int;
begin
 result:=renBuf._height;
end;

//=========================== TAggRasterizerGamma =============================================
constructor TAgxRasterizerGamma.Construct(alpha ,gamma : double );
begin
 m_alpha.Construct(alpha );
 m_gamma.Construct(gamma );
end;

function TAgxRasterizerGamma.func_operator_gamma(x : double ) : double;
begin
 result:=m_alpha.func_operator_gamma(m_gamma.func_operator_gamma(x ) );
end;


//=========================================================================================
{ AGG2DRENDERER_RENDER }
procedure Agg2DRenderer_render(
           gr : TAgxCanvas;
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           fillColor_ : boolean ); overload;
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
 if (fillColor_ and
     (gr.m_fillGradientFlag = AGX_Linear ) ) or
    (not fillColor_ and
     (gr.m_lineGradientFlag = AGX_Linear ) ) then
  if fillColor_ then
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_fillGradientInterpolator ,
     @gr.m_linearGradientFunction ,
     @gr.m_fillGradient ,
     gr.m_fillGradientD1 ,
     gr.m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

   end
  else
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_lineGradientInterpolator ,
     @gr.m_linearGradientFunction ,
     @gr.m_lineGradient ,
     gr.m_lineGradientD1 ,
     gr.m_lineGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

   end
 else
  if (fillColor_ and
      (gr.m_fillGradientFlag = AGX_Radial ) ) or
     (not fillColor_ and
      (gr.m_lineGradientFlag = AGX_Radial ) ) then
   if fillColor_ then
    begin
     span.Construct(
      @gr.m_allocator ,
      @gr.m_fillGradientInterpolator ,
      @gr.m_radialGradientFunction ,
      @gr.m_fillGradient ,
      gr.m_fillGradientD1 ,
      gr.m_fillGradientD2 );

      ren.Construct   (renBase ,@span );
      render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

    end
   else
    begin
     span.Construct(
      @gr.m_allocator ,
      @gr.m_lineGradientInterpolator ,
      @gr.m_radialGradientFunction ,
      @gr.m_lineGradient ,
      gr.m_lineGradientD1 ,
      gr.m_lineGradientD2 );

     ren.Construct   (renBase ,@span );
     render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

    end
  else
   begin
    if fillColor_ then
     clr.Construct(gr.m_fillColor )
    else
     clr.Construct(gr.m_lineColor );

    renSolid^.color_ (@clr );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,renSolid );

   end;

end;

{ AGG2DRENDERER_RENDER }
procedure Agg2DRenderer_render(
           gr : TAgxCanvas;
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           ras : gray8_adaptor_type_ptr;
           sl : gray8_scanline_type_ptr ); overload;
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
 if gr.m_fillGradientFlag = AGX_Linear then
  begin
   span.Construct(
    @gr.m_allocator ,
    @gr.m_fillGradientInterpolator ,
    @gr.m_linearGradientFunction ,
    @gr.m_fillGradient ,
    gr.m_fillGradientD1 ,
    gr.m_fillGradientD2 );

   ren.Construct   (renBase ,@span );
   render_scanlines(ras ,sl ,@ren );

  end
 else
  if gr.m_fillGradientFlag = AGX_Radial then
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_fillGradientInterpolator ,
     @gr.m_radialGradientFunction ,
     @gr.m_fillGradient ,
     gr.m_fillGradientD1 ,
     gr.m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(ras ,sl ,@ren );

   end
  else
   begin
    clr.Construct   (gr.m_fillColor );
    renSolid^.color_ (@clr );
    render_scanlines(ras ,sl ,renSolid );

   end;

end;

{ AGG2DRENDERER_RENDERIMAGE }
procedure Agg2DRenderer_renderImage(
           gr : TAgxCanvas;
           img : PAGXImage;
           renBase : renderer_base_ptr;
           interpolator : span_interpolator_linear_ptr );
var
 blend : TAggSpanConvImageBlend;

 si : span_image_filter_rgba;
 sg : span_image_filter_rgba_nn;
 sb : span_image_filter_rgba_bilinear;
 s2 : span_image_filter_rgba_2x2;
 sa : span_image_resample_rgba_affine;
 sc : span_converter;
 ri : renderer_scanline_aa;

 clr : aggclr;

 resample : boolean;

 sx ,sy : double;

begin
 case gr.m_pixf of
  pf32bit :
   blend.Construct(gr.m_imageBlendMode ,gr.m_imageBlendColor ,@gr.m_pixFormatCompPre );

  else
   blend.Construct(gr.m_imageBlendMode ,gr.m_imageBlendColor ,NIL );

 end;

 if gr.m_imageFilter = AGX_NoFilter then
  begin
   clr.ConstrInt(0 ,0 ,0 ,0 );
   sg.Construct (@gr.m_allocator ,@img^.renBuf ,@clr ,interpolator ,rgba_order );
   sc.Construct (@sg ,@blend );
   ri.Construct (renBase ,@sc );

   render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

  end
 else
  begin
   resample:=gr.m_imageResample = AGX_ResampleAlways;

   if gr.m_imageResample = AGX_ResampleOnZoomOut then
    begin
     interpolator^._transformer^.scaling_abs(@sx ,@sy );

     if (sx > 1.125 ) or
        (sy > 1.125 ) then
      resample:=true;

    end;

   if resample then
    begin
     clr.ConstrInt(0 ,0 ,0 ,0 );
     sa.Construct(
      @gr.m_allocator ,
      @img^.renBuf ,
      @clr ,
      interpolator ,
      @gr.m_imageFilterLut ,
      rgba_order );

     sc.Construct(@sa ,@blend );
     ri.Construct(renBase ,@sc );

     render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

    end
   else
    if gr.m_imageFilter = AGX_Bilinear then
     begin
      clr.ConstrInt(0 ,0 ,0 ,0 );
      sb.Construct(
       @gr.m_allocator ,
       @img^.renBuf ,
       @clr ,
       interpolator ,
       rgba_order );

      sc.Construct(@sb ,@blend );
      ri.Construct(renBase ,@sc );

      render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

     end
    else
     if gr.m_imageFilterLut.diameter = 2 then
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       s2.Construct(
        @gr.m_allocator ,
        @img^.renBuf ,
        @clr ,
        interpolator,
        @gr.m_imageFilterLut ,
        rgba_order );

       sc.Construct(@s2 ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

      end
     else
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       si.Construct(
        @gr.m_allocator ,
        @img^.renBuf ,
        @clr ,
        interpolator ,
        @gr.m_imageFilterLut ,
        rgba_order );

       sc.Construct(@si ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

      end;

  end;

end;

//============================ TAgxCanvas =========================================

constructor TAgxCanvas.Create;
begin
 inherited;
 m_rbuf.Construct;
 m_pixf:=pf32bit;
 pixfmt_rgba32           (m_pixFormat ,@m_rbuf );
 pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
 pixfmt_rgba32           (m_pixFormatPre ,@m_rbuf );
 pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
 m_renBase.Construct       (@m_pixFormat );
 m_renBaseComp.Construct   (@m_pixFormatComp );
 m_renBasePre.Construct    (@m_pixFormatPre );
 m_renBaseCompPre.Construct(@m_pixFormatCompPre );
 m_renSolid.Construct    (@m_renBase );
 m_renSolidComp.Construct(@m_renBaseComp );
 m_allocator.Construct;
 m_clipBox.Construct(0 ,0 ,0 ,0 );
 m_blendMode     :=AGX_BlendAlpha;
 m_imageBlendMode:=AGX_BlendDst;

 m_imageBlendColor.Construct(0 ,0 ,0 );

 m_scanline.Construct;
 m_rasterizer.Construct;

 m_masterAlpha   :=1.0;
 m_antiAliasGamma:=1.0;

 m_fillColor.Construct(255 ,255 ,255 );
 m_lineColor.Construct(0   ,0   ,0 );

 m_fillGradient.Construct(256 ,sizeof(aggclr ) );
 m_lineGradient.Construct(256 ,sizeof(aggclr ) );

 m_lineCap :=AGX_CapRound;
 m_lineJoin:=AGX_JoinRound;

 m_fillGradientFlag:=AGX_Solid;
 m_lineGradientFlag:=AGX_Solid;

 m_fillGradientMatrix.Construct;
 m_lineGradientMatrix.Construct;

 m_fillGradientD1:=0.0;
 m_lineGradientD1:=0.0;
 m_fillGradientD2:=100.0;
 m_lineGradientD2:=100.0;

 m_textAngle  :=0.0;
 m_textAlignX :=AGX_AlignLeft;
 m_textAlignY :=AGX_AlignBottom;
 m_textHints  :=true;
 m_fontHeight :=24.0;
 m_fontAscent :=0.0;
 m_fontDescent:=0.0;
 m_fontFileName :='Arial';
 m_fontItalic   :=false;
 m_fontWeight   :=400;
 m_fontCacheType:=AGX_RasterFontCache;

 m_imageFilter  :=AGX_Bilinear;
 m_imageResample:=AGX_NoResample;

 m_gammaNone.Construct;

 m_ifBilinear.Construct;
 m_ifHanning.Construct;
 m_ifHermite.Construct;
 m_ifQuadric.Construct;
 m_ifBicubic.Construct;
 m_ifCatrom.Construct;
 m_ifSpline16.Construct;
 m_ifSpline36.Construct;
 m_ifBlackman144.Construct;

 m_imageFilterLut.Construct(@m_ifBilinear ,true );

 m_linearGradientFunction.Construct;
 m_radialGradientFunction.Construct;

 m_fillGradientInterpolator.Construct(@m_fillGradientMatrix );
 m_lineGradientInterpolator.Construct(@m_lineGradientMatrix );

 m_lineWidth  :=1;
 m_evenOddFlag:=false;

 m_imageFlip:=false;

 m_path.Construct;
 m_transform.Construct;

 m_convCurve.Construct (@m_path );
 m_convStroke.Construct(@m_convCurve );

 m_pathTransform.Construct  (@m_convCurve ,@m_transform );
 m_strokeTransform.Construct(@m_convStroke ,@m_transform );

{$IFDEF AGG2D_USE_FREETYPE}
m_fontEngine.Construct;
{$ENDIF}

{$IFDEF AGG2D_USE_WINFONTS}
m_fontDC:=GetDC(0 );
m_fontEngine.Construct(m_fontDC );
{$ENDIF}

{$IFNDEF AGG2D_NO_FONT}
m_fontCacheManager.Construct(@m_fontEngine );
{$ENDIF}

 LineCapSet(m_lineCap );
 LineJoinSet(m_lineJoin );
 //...............................................
 FlipText(false);
 fLinkBmp:=nil;
 fLinkIntfImage:=nil;
 fWidth :=0;
 fHeight:=0;
end;

destructor TAgxCanvas.Destroy;
begin
 if fLinkIntfImage<>nil then FreeAndNil(fLinkIntfImage);
 fLinkBmp:=nil;

 m_rbuf.Destruct;

 m_allocator.Destruct;

 m_scanline.Destruct;
 m_rasterizer.Destruct;

 m_fillGradient.Destruct;
 m_lineGradient.Destruct;

 m_imageFilterLut.Destruct;
 m_path.Destruct;

 m_convCurve.Destruct;
 m_convStroke.Destruct;

{$IFNDEF AGG2D_NO_FONT}
 m_fontEngine.Destruct;
 m_fontCacheManager.Destruct;
{$ENDIF }

{$IFDEF AGG2D_USE_WINFONTS}
 ReleaseDC(0 ,m_fontDC );
{$ENDIF }

 inherited;
end;


function TAgxCanvas.Attach(bitmap : TBitmap; const flip_y : boolean = false ) : boolean;
var
 buffer : pointer;
 stride : integer;

begin
  result:=false;
  if fLinkIntfImage<>nil then FreeAndNil(fLinkIntfImage);
  fLinkBmp:=bitmap;

  if bitmap=nil then exit;
  fLinkIntfImage:=bitmap.CreateIntfImage;

 if Assigned(bitmap ) and not bitmap.Empty then
  case bitmap.PixelFormat of
   pf24bit ,pf32bit :
    begin

     fWidth:=bitmap.Width;
     fHeight:=bitmap.Height;
    { Rendering Buffer }
     stride:=integer(fLinkIntfImage.GetDataLineStart(1) - integer(fLinkIntfImage.GetDataLineStart(0)));

     if stride < 0 then
      buffer:=fLinkIntfImage.GetDataLineStart(bitmap.Height - 1) else
      buffer:=fLinkIntfImage.GetDataLineStart(0);

     if flip_y then  stride:=stride * -1;

     m_rbuf.attach(
      buffer ,
      bitmap.Width ,
      bitmap.Height ,
      stride );

    { Pixel Format }
     m_pixf:=bitmap.PixelFormat;

     case m_pixf of
      pf24bit :
       begin
        pixfmt_rgb24(m_pixFormat ,@m_rbuf );
        pixfmt_rgb24(m_pixFormatPre ,@m_rbuf );

       end;

      pf32bit :
       begin
        pixfmt_rgba32           (m_pixFormat ,@m_rbuf );
        pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
        pixfmt_rgba32           (m_pixFormatPre ,@m_rbuf );
        pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );

       end;

     end;

    { Reset state }
     m_renBase.reset_clipping       (true );
     m_renBaseComp.reset_clipping   (true );
     m_renBasePre.reset_clipping    (true );
     m_renBaseCompPre.reset_clipping(true );

     TransformationsReset;

     LineWidthSet(1.0 );
     LineColorSet(0   ,0   ,0 );
     FillColorSet(255 ,255 ,255 );

     TextAlignmentSet(AGX_AlignLeft ,AGX_AlignBottom );

     ClipBoxSet(0 ,0 ,bitmap.Width ,bitmap.Height );
     LineCapSet(AGX_CapRound );
     LineJoinSet(AGX_JoinRound );
     FlipText(false );

     ImageFilterSet(AGX_Bilinear );
     ImageResampleSet(AGX_NoResample );
     ImageFlip(false );

     m_masterAlpha   :=1.0;
     m_antiAliasGamma:=1.4;

     m_rasterizer.gamma(@m_gammaNone );

     m_blendMode:=AGX_BlendAlpha;

     FillEvenOddSet(false );
     BlendModeSet(AGX_BlendAlpha );

     FlipText(false );
     ResetPath;

     ImageFilterSet(AGX_Bilinear );
     ImageResampleSet(AGX_NoResample );

     viewport(0,0,fwidth,fheight,0,0,fwidth ,fheight,AGX_XMidYMid);
     result:=true;

    end;
  end;
end;

procedure TAgxCanvas.Invalidate;
 begin
  if fLinkIntfImage=nil then Exit;
  if fLinkBmp=nil then Exit;
  fLinkBmp.LoadFromIntfImage(fLinkIntfImage);
 end;

procedure TAgxCanvas.ClipBoxSet(const x1 ,y1 ,x2 ,y2 : double );
var
 rx1 ,ry1 ,rx2 ,ry2 : int;

begin
 m_clipBox.Construct(x1 ,y1 ,x2 ,y2 );

 rx1:=Trunc(x1 );
 ry1:=Trunc(y1 );
 rx2:=Trunc(x2 );
 ry2:=Trunc(y2 );

 m_renBase.clip_box_       (rx1 ,ry1 ,rx2 ,ry2 );
 m_renBaseComp.clip_box_   (rx1 ,ry1 ,rx2 ,ry2 );
 m_renBasePre.clip_box_    (rx1 ,ry1 ,rx2 ,ry2 );
 m_renBaseCompPre.clip_box_(rx1 ,ry1 ,rx2 ,ry2 );

 m_rasterizer.clip_box(x1 ,y1 ,x2 ,y2 );

end;

function TAgxCanvas.ClipBoxGet : TAgxRectD;
begin
 result:=m_clipBox;
end;

Function TAgxCanvas.ColorAgxToColor(const c:TAgxColor):TColor;
 begin
   Result:=RGBToColor(c.r,c.g,c.b);
 end;

Function TAgxCanvas.ColorToColorAgx(const c:TColor):TAgxColor;
  var fc:TFPColor;
 begin
   fc:=TColorToFPColor(c);
   result:=FPColorToColorAgx(fc);
 end;

Function TAgxCanvas.FPColorToColorAgx(const c:TFPColor):TAgxColor;
var  R,G,B,A : byte;
 begin
   R:=c.red;
   G:=c.green;
   B:=c.blue;
   A:=c.alpha;
   result.Construct(R,G,B,A);
 end;

Function TAgxCanvas.ColorAgxToFPColor(const c:TAgxColor):TFPColor;
 begin
  Result:=FPColor(c.r,c.g,c.b,c.a)
 end;

procedure TAgxCanvas.ClearAll(const c : TAgxColor );
var clr : aggclr;
begin
 clr.Construct(c );
 m_renBase.clear(@clr );
end;

procedure TAgxCanvas.ClearAll(const c : TColor);
begin
 ClearAll(ColorToColorAgx(c));
end;

procedure TAgxCanvas.ClearAll(const r ,g ,b : byte; const a : byte = 255 );
var clr : TAgxColor;
begin
 clr.Construct(r ,g ,b ,a );
 ClearAll(clr);
end;

procedure TAgxCanvas.ClearAll(const c : TFPColor );
begin
 ClearAll(FPColorToColorAgx(c));
end;

procedure TAgxCanvas.ClearClipBox(const c : TAgxColor );
var clr : aggclr;
begin
 clr.Construct(c );
 m_renBase.copy_bar(0 ,0 ,m_renBase.width ,m_renBase.height ,@clr );
end;

procedure TAgxCanvas.ClearClipBox(const r ,g ,b : byte; const a : byte = 255 );
var
 clr : TAgxColor;

begin
 clr.Construct(r ,g ,b ,a );
 ClearClipBox (clr );
end;

procedure TAgxCanvas.WorldToScreen(const x ,y : PDouble );
begin
 m_transform.transform(@m_transform ,double_ptr(x ) ,double_ptr(y ) );
end;

procedure TAgxCanvas.ScreenToWorld(const x ,y : PDouble );
begin
 m_transform.inverse_transform(@m_transform ,double_ptr(x ) ,double_ptr(y ) );
end;

function TAgxCanvas.WorldToScreen(const scalar : double ) : double;
var
 x1 ,y1 ,x2 ,y2 : double;
begin
 x1:=0;
 y1:=0;
 x2:=scalar;
 y2:=scalar;

 WorldToScreen(@x1 ,@y1 );
 WorldToScreen(@x2 ,@y2 );
 result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;
end;

function TAgxCanvas.ScreenToWorld(const scalar : double ) : double;
var
 x1 ,y1 ,x2 ,y2 : double;
begin
 x1:=0;
 y1:=0;
 x2:=scalar;
 y2:=scalar;

 ScreenToWorld(@x1 ,@y1 );
 ScreenToWorld(@x2 ,@y2 );

 result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;
end;

procedure TAgxCanvas.AlignPoint(const x ,y : PDouble );
begin
 WorldToScreen(x ,y );

 x^:=Floor(x^ ) + 0.5;
 y^:=Floor(y^ ) + 0.5;

 ScreenToWorld(x ,y );
end;

function TAgxCanvas.InBox(const worldX ,worldY : double ) : boolean;
begin
 WorldToScreen(@worldX ,@worldY );
 result:=m_renBase.inbox(Trunc(worldX ) ,Trunc(worldY ) );
end;

procedure TAgxCanvas.BlendModeSet(m : TAgxBlendMode );
begin
 m_blendMode:=m;
 m_pixFormatComp.comp_op_   (unsigned(m ) );
 m_pixFormatCompPre.comp_op_(unsigned(m ) );
end;

function TAgxCanvas.BlendModeGet : TAgxBlendMode;
begin
 result:=m_blendMode;
end;

procedure TAgxCanvas.MasterAlphaSet(a : double );
begin
 m_masterAlpha:=a;
 UpdateRasterizerGamma;
end;

function TAgxCanvas.MasterAlphaGet: double;
begin
 result:=m_masterAlpha;
end;

procedure TAgxCanvas.AntiAliasGammaSet(g : double );
begin
 m_antiAliasGamma:=g;
 UpdateRasterizerGamma;
end;

function TAgxCanvas.AntiAliasGammaGet: double;
begin
 result:=m_antiAliasGamma;
end;

procedure TAgxCanvas.FillColorSet(const c : TAgxColor );
begin
 m_fillColor       :=c;
 m_fillGradientFlag:=AGX_Solid;
end;

procedure TAgxCanvas.FillColorSet(const r ,g ,b : byte; const a : byte = 255 );
var clr : TAgxColor;
begin
 clr.Construct(r ,g ,b ,a );
 FillColorSet(clr );
end;

procedure TAgxCanvas.NoFill;
var clr : TAgxColor;
begin
 clr.Construct(0 ,0 ,0 ,0 );
 FillColorSet(clr );
end;

procedure TAgxCanvas.LineColorSet(const c : TAgxColor );
begin
 m_lineColor:=c;
 m_lineGradientFlag:=AGX_Solid;
end;

procedure TAgxCanvas.LineColorSet(const r ,g ,b : byte; const a : byte = 255 );
var clr : TAgxColor;
begin
 clr.Construct(r ,g ,b ,a );
 LineColorSet(clr );
end;

procedure TAgxCanvas.NoLine;
var clr : TAgxColor;
begin
 clr.Construct(0 ,0 ,0 ,0 );
 LineColorSet(clr);
end;

function TAgxCanvas.FillColorGet : TAgxColor;
begin
 result:=m_fillColor;
end;

function TAgxCanvas.LineColorGet : TAgxColor;
begin
 result:=m_lineColor;
end;

procedure TAgxCanvas.FillLinearGradient(const x1 ,y1 ,x2 ,y2 : double; const c1 ,c2 : TAgxColor; const  profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;
 k ,angle : double;
 c : TAgxColor;
 clr : aggclr;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;
begin
 startGradient:=128 - Trunc(profile * 127.0 );
 endGradient  :=128 + Trunc(profile * 127.0 );

 if endGradient <= startGradient then endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient ) * k );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 angle:=ArcTan2(y2 - y1 ,x2 - x1 );

 m_fillGradientMatrix.reset;

 tar.Construct(angle );

 m_fillGradientMatrix.multiply(@tar );

 tat.Construct(x1 ,y1 );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.multiply(@m_transform );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0.0;
 m_fillGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
 m_fillGradientFlag:=AGX_Linear;

 m_fillColor.Construct(0 ,0 ,0 );  // Set some real color

end;

procedure TAgxCanvas.LineLinearGradient(const x1 ,y1 ,x2 ,y2 : double; const c1 ,c2 : TAgxColor; const profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;
 k ,angle : double;
 c : TAgxColor;
 clr : aggclr;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;
begin
 startGradient:=128 - Trunc(profile * 128.0 );
 endGradient  :=128 + Trunc(profile * 128.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient) * k );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 angle:=ArcTan2(y2 - y1 ,x2 - x1 );

 m_lineGradientMatrix.reset;

 tar.Construct(angle );

 m_lineGradientMatrix.multiply(@tar );

 tat.Construct(x1 ,y1 );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.multiply(@m_transform );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0.0;
 m_lineGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
 m_lineGradientFlag:=AGX_Linear;

 m_lineColor.Construct(0 ,0 ,0 );  // Set some real color

end;

procedure TAgxCanvas.FillRadialGradient(const x ,y ,r : double; const c1 ,c2 : TAgxColor; const profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;
 k : double;
 c : TAgxColor;
 clr : aggclr;
 tat : trans_affine_translation;
begin
 startGradient:=128 - Trunc(profile * 127.0 );
 endGradient  :=128 + Trunc(profile * 127.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient ) * k );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_fillGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0;
 m_fillGradientFlag:=AGX_Radial;

 m_fillColor.Construct(0 ,0 ,0 );  // Set some real color

end;

procedure TAgxCanvas.LineRadialGradient(const x ,y ,r : double; const c1 ,c2 : TAgxColor; const profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k : double;
 c : TAgxColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 startGradient:=128 - Trunc(profile * 128.0 );
 endGradient  :=128 + Trunc(profile * 128.0 );

 if endGradient <= startGradient then
  endGradient:=startGradient + 1;

 k:=1.0 / (endGradient - startGradient );
 i:=0;

 while i < startGradient do
  begin
   clr.Construct(c1 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < endGradient do
  begin
   c:=c1.gradient(c2 ,(i - startGradient ) * k );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   clr.Construct(c2 );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_lineGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0;
 m_lineGradientFlag:=AGX_Radial;

 m_lineColor.Construct(0 ,0 ,0 );  // Set some real color

end;

procedure TAgxCanvas.FillRadialGradient(const x ,y ,r : double; const c1 ,c2 ,c3 : TAgxColor );
var
 i : int;
 c : TAgxColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 i:=0;

 while i < 128 do
  begin
   c:=c1.gradient(c2 ,i / 127.0 );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   c:=c2.gradient(c3 ,(i - 128 ) / 127.0 );

   clr.Construct(c );

   move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_fillGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0;
 m_fillGradientFlag:=AGX_Radial;

 m_fillColor.Construct(0 ,0 ,0 ); // Set some real color

end;

procedure TAgxCanvas.LineRadialGradient(const x ,y ,r : double; const c1 ,c2 ,c3 : TAgxColor );
var
 i : int;
 c : TAgxColor;

 clr : aggclr;
 tat : trans_affine_translation;

begin
 i:=0;

 while i < 128 do
  begin
   c:=c1.gradient(c2 ,i / 127.0 );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 while i < 256 do
  begin
   c:=c2.gradient(c3 ,(i - 128 ) / 127.0 );

   clr.Construct(c );

   move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
   inc (i );

  end;

 m_lineGradientD2:=worldToScreen(r );

 WorldToScreen(@x ,@y );
 m_lineGradientMatrix.reset;
 tat.Construct(x ,y );
 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;
 m_lineGradientD1  :=0;
 m_lineGradientFlag:=AGX_Radial;
 m_lineColor.Construct(0 ,0 ,0 ); // Set some real color

end;

procedure TAgxCanvas.FillRadialGradient(const x ,y ,r : double );
 var tat : trans_affine_translation;
begin
 m_fillGradientD2:=worldToScreen(r );
 WorldToScreen(@x ,@y );
 m_fillGradientMatrix.reset;
 tat.Construct(x ,y );
 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;
 m_fillGradientD1:=0;
end;

procedure TAgxCanvas.LineRadialGradient(const x ,y ,r : double );
var tat : trans_affine_translation;
begin
 m_lineGradientD2:=worldToScreen(r );
 WorldToScreen(@x ,@y );
 m_lineGradientMatrix.reset;
 tat.Construct(x ,y );
 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;
 m_lineGradientD1:=0;
end;

procedure TAgxCanvas.LineWidthSet(const w : double );
begin
 m_lineWidth:=w;
 m_convStroke.width_(w );
end;

function TAgxCanvas.LineWidthGet : double;
begin
 result:=m_lineWidth;
end;

procedure TAgxCanvas.LineCapSet(const cap : TAgxLineCap );
begin
 m_lineCap:=cap;
 m_convStroke.line_cap_(cap );
end;

function TAgxCanvas.LineCapGet: TAgxLineCap;
begin
 result:=m_lineCap;
end;

procedure TAgxCanvas.LineJoinSet(const join : TAgxLineJoin );
begin
 m_lineJoin:=join;
 m_convStroke.line_join_(join );
end;

function TAgxCanvas.LineJoinGet : TAgxLineJoin;
begin
 result:=m_lineJoin;
end;

procedure TAgxCanvas.FillEvenOddSet(const evenOddFlag : boolean );
begin
 m_evenOddFlag:=evenOddFlag;
 if evenOddFlag then
  m_rasterizer.filling_rule(fill_even_odd ) else
  m_rasterizer.filling_rule(fill_non_zero );
end;

function TAgxCanvas.FillEvenOddGet : boolean;
begin
 result:=m_evenOddFlag;
end;

procedure TAgxCanvas.TransformationsSet(const tr : PAgxTransformations );
begin
 m_transform.load_from(@tr^.affineMatrix[0 ] );
 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );
end;

function TAgxCanvas.TransformationsGet : TAgxTransformations;
begin
 m_transform.store_to(@result.affineMatrix[0 ] );
end;

procedure TAgxCanvas.TransformationsReset;
begin
 m_transform.reset;
end;

procedure TAgxCanvas.Affine(tr : PAgxAffine );
begin
 m_transform.multiply(tr );
 m_convCurve.approximation_scale_ (WorldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(WorldToScreen(1.0 ) * g_approxScale );
end;

procedure TAgxCanvas.Affine(tr : PAgxTransformations );
var ta : trans_affine;
begin
 ta.Construct( tr^.affineMatrix[0 ] ,tr^.affineMatrix[1 ] ,tr^.affineMatrix[2 ] ,
               tr^.affineMatrix[3 ] ,tr^.affineMatrix[4 ] ,tr^.affineMatrix[5 ] );
 affine(PAgxAffine(@ta ) );
end;

procedure TAgxCanvas.Rotate(const angle : double );
var tar : trans_affine_rotation;
begin
 tar.Construct(DegToRad(angle));
 m_transform.multiply(@tar );
end;

procedure TAgxCanvas.Scale(const sx ,sy : double );
var tas : trans_affine_scaling;
begin
 tas.Construct(sx ,sy );
 m_transform.multiply(@tas );
 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );
end;

procedure TAgxCanvas.Skew(const sx ,sy : double );
var tas : trans_affine_skewing;
begin
 tas.Construct(sx ,sy );
 m_transform.multiply(@tas );
end;

procedure TAgxCanvas.Translate(const x ,y : double );
var tat : trans_affine_translation;
begin
 tat.Construct(x ,y );
 m_transform.multiply(@tat );
end;

procedure TAgxCanvas.Parallelogram(const x1 ,y1 ,x2 ,y2 : double; const  para : PDouble );
var ta : trans_affine;
begin
 ta.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(para ) );
 m_transform.multiply(@ta );
 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );
end;

procedure TAgxCanvas.Viewport(
           const worldX1  ,worldY1  ,worldX2  ,worldY2 ,
           screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
           const opt : TAgxViewportOption = AGX_XMidYMid );
var
 vp : trans_viewport;
 mx : trans_affine;

begin
 vp.Construct;

 case opt of
  AGX_Anisotropic :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_stretch );

  AGX_XMinYMin :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_meet );

  AGX_XMidYMin :
   vp.preserve_aspect_ratio(0.5 ,0.0 ,aspect_ratio_meet );

  AGX_XMaxYMin :
   vp.preserve_aspect_ratio(1.0 ,0.0 ,aspect_ratio_meet );

  AGX_XMinYMid :
   vp.preserve_aspect_ratio(0.0 ,0.5 ,aspect_ratio_meet );

  AGX_XMidYMid :
   vp.preserve_aspect_ratio(0.5 ,0.5 ,aspect_ratio_meet );

  AGX_XMaxYMid :
   vp.preserve_aspect_ratio(1.0 ,0.5 ,aspect_ratio_meet );

  AGX_XMinYMax :
   vp.preserve_aspect_ratio(0.0 ,1.0 ,aspect_ratio_meet );

  AGX_XMidYMax :
   vp.preserve_aspect_ratio(0.5 ,1.0 ,aspect_ratio_meet );

  AGX_XMaxYMax :
   vp.preserve_aspect_ratio(1.0 ,1.0 ,aspect_ratio_meet );

 end;

 vp.world_viewport (worldX1  ,worldY1  ,worldX2  ,worldY2 );
 vp.device_viewport(screenX1 ,screenY1 ,screenX2 ,screenY2 );

 mx.Construct;

 vp.to_affine        (@mx );
 m_transform.multiply(@mx );

 m_convCurve.approximation_scale_ (WorldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(WorldToScreen(1.0 ) * g_approxScale );

end;

procedure TAgxCanvas.Line(const x1 ,y1 ,x2 ,y2 : double );
begin
 m_path.remove_all;
 addLine (x1 ,y1 ,x2 ,y2 );
 DrawPath(AGX_StrokeOnly );
end;

procedure TAgxCanvas.Triangle(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y2 );
 m_path.line_to(x3 ,y3 );
 m_path.close_polygon;
 DrawPath(AGX_FillAndStroke );
end;

procedure TAgxCanvas.Rectangle(const x1 ,y1 ,x2 ,y2 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y1 );
 m_path.line_to(x2 ,y2 );
 m_path.line_to(x1 ,y2 );
 m_path.close_polygon;
 DrawPath(AGX_FillAndStroke );
end;

procedure TAgxCanvas.RoundedRect(const x1 ,y1 ,x2 ,y2 ,r : double );
var  rc : rounded_rect;
begin
 m_path.remove_all;
 rc.Construct(x1 ,y1 ,x2 ,y2 ,r );
 rc.normalize_radius;
 rc.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );
 m_path.add_path(@rc ,0 ,false );
 DrawPath(AGX_FillAndStroke );
end;


procedure TAgxCanvas.RoundedRect(const x1 ,y1 ,x2 ,y2 ,rx ,ry : double );
var rc : rounded_rect;
begin
 m_path.remove_all;
 rc.Construct;
 rc.rect  (x1 ,y1 ,x2 ,y2 );
 rc.radius(rx ,ry );
 rc.normalize_radius;
 m_path.add_path(@rc ,0 ,false );
 DrawPath(AGX_FillAndStroke );
end;

procedure TAgxCanvas.RoundedRect( const x1 ,y1 ,x2 ,y2 ,rxBottom ,ryBottom ,rxTop ,ryTop : double );
var rc : rounded_rect;
begin
 m_path.remove_all;
 rc.Construct;
 rc.rect  (x1 ,y1 ,x2 ,y2 );
 rc.radius(rxBottom ,ryBottom ,rxTop ,ryTop );
 rc.normalize_radius;
 rc.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );
 m_path.add_path(@rc ,0 ,false );
 DrawPath(AGX_FillAndStroke );
end;

procedure TAgxCanvas.Ellipse(const cx ,cy ,rx ,ry : double );
var el : bezier_arc;
begin
 m_path.remove_all;
 el.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi );
 m_path.add_path(@el ,0 ,false );
 m_path.close_polygon;
 DrawPath(AGX_FillAndStroke );
end;

procedure TAgxCanvas.Arc(const cx ,cy ,rx ,ry ,start ,sweep : double );
var ar : {bezier_}AGG_arc.arc;
begin
 m_path.remove_all;
 ar.Construct(cx ,cy ,rx ,ry ,sweep ,start ,false );
 m_path.add_path(@ar ,0 ,false );
 DrawPath(AGX_StrokeOnly );
end;


procedure TAgxCanvas.Star(const cx ,cy ,r1 ,r2 ,startAngle : double; const numRays : integer );
var da ,a ,x ,y : double;
    i : int;
begin
 m_path.remove_all;

 da:=pi / numRays;
 a :=startAngle;
 i:=0;
 while i < numRays do
  begin
   x:=Cos(a ) * r2 + cx;
   y:=Sin(a ) * r2 + cy;

   if i <> 0 then
    m_path.line_to(x ,y ) else
    m_path.move_to(x ,y );

   a:=a + da;
   m_path.line_to(Cos(a ) * r1 + cx ,Sin(a ) * r1 + cy );
   a:=a + da;
   inc(i );
  end;

 ClosePolygon;
 DrawPath(AGX_FillAndStroke );
end;

procedure TAgxCanvas.Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.curve3 (x2 ,y2 ,x3 ,y3 );
 DrawPath(AGX_StrokeOnly );
end;

procedure TAgxCanvas.Curve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.curve4 (x2 ,y2 ,x3 ,y3 ,x4 ,y4 );
 DrawPath(AGX_StrokeOnly );
end;

procedure TAgxCanvas.Polygon(xy : PDouble; const numPoints : integer );
begin
 m_path.remove_all;
 m_path.add_poly(double_2_ptr(xy ) ,numPoints );
 ClosePolygon;
 DrawPath(AGX_FillAndStroke);
end;

procedure TAgxCanvas.PolygonInt(const Points:TPolyPoints);
 var PointsNum:integer;
begin
 PointsNum:=High(Points)-Low(Points);
 PolygonInt(Points,PointsNum);
end;

procedure TAgxCanvas.PolygonInt(const Points:array of Tpoint;PointsNum:integer);
 var i:integer;
     p:Tpoint;
begin

 if PointsNum<2 then exit;

 m_path.remove_all;

 p:=Points[0];
 m_path.move_to(p.X,p.Y);
 for i := 1 to PointsNum-1 do
  begin
   p:=Points[i];
   m_path.line_to(p.x,p.Y);
  end;

 ClosePolygon;
 DrawPath(AGX_FillAndStroke);
end;
procedure TAgxCanvas.Polyline(xy : PDouble; const numPoints : integer );
begin
 m_path.remove_all;
 m_path.add_poly(double_2_ptr(xy) ,numPoints );
 DrawPath(AGX_StrokeOnly);
end;

procedure TAgxCanvas.PolylineInt(const Points:TPolyPoints);
 var i,PointsNum:integer;
     p:Tpoint;
begin
 PointsNum:=High(Points)-Low(Points)+1;
 PolylineInt(Points,PointsNum);
end;

procedure TAgxCanvas.PolylineInt(const Points:array of Tpoint;PointsNum:integer);
 var i:integer;
     p:Tpoint;
begin
 if PointsNum<2 then exit;
 m_path.remove_all;

 p:=Points[0];
 m_path.move_to(p.X,p.Y);
 for i := 1 to PointsNum-1 do
  begin
   p:=Points[i];
   m_path.line_to(p.x,p.Y);
  end;

 DrawPath(AGX_StrokeOnly);
end;

procedure TAgxCanvas.FlipText(const flip : boolean );
begin
 {$IFNDEF AGG2D_NO_FONT}
  m_fontEngine.flip_y_(not flip );
 {$ENDIF}
end;

procedure TAgxCanvas.FontEngineCreate;
 begin
  {$IFDEF AGG2D_USE_FREETYPE }
 if m_fontCacheType = AGX_VectorFontCache then
  m_fontEngine.load_font(PChar(@m_fontFileName[1] ) ,0 ,glyph_ren_outline ) else
  m_fontEngine.load_font(PChar(@m_fontFileName[1] ) ,0 ,glyph_ren_agg_gray8 );

 m_fontEngine.hinting_(m_textHints );

 if m_fontCacheType = AGX_VectorFontCache then
  m_fontEngine.height_(m_fontHeight ) else
  m_fontEngine.height_(worldToScreen(m_fontHeight ) );

 {$ENDIF }
 {$IFDEF AGG2D_USE_WINFONTS}

 m_fontEngine.hinting_(m_textHints );

 if m_fontCacheType = AGX_VectorFontCache then
  m_fontEngine.create_font_(PChar(@m_fontFileName[1] ) ,glyph_ren_outline ,m_fontHeight ,0.0 ,m_fontWeight ,m_fontItalic )
 else
  m_fontEngine.create_font_(PChar(@m_fontFileName[1] ) ,glyph_ren_agg_gray8 ,worldToScreen(m_fontHeight) ,0.0 ,m_fontWeight ,m_fontItalic );

{$ENDIF }
 end;

procedure TAgxCanvas.Font(const FileName : AnsiString;
                          const height : double;
                          const bold : boolean = false;
                          const italic : boolean = false;
                          const cache : TAgxFontCacheType = AGX_VectorFontCache;
                          const angle : double = 0.0 );
begin
 m_textAngle    :=DegToRad(angle);
 m_fontHeight   :=height;
 m_fontCacheType:=cache;
 m_fontFileName :=FileName;
 m_fontItalic   :=italic;

 if bold then
   m_fontWeight:=700 else
   m_fontWeight:=400;

  FontEngineCreate;
end;




procedure TAgxCanvas.FontEx(const FileName:AnsiString;
                            const height:double;
                            const Weight:int=400;
                            const italic:boolean=false;
                            const cache:TAgxFontCacheType=AGX_VectorFontCache;
                            const angle:double=0.0);

begin
 m_textAngle    :=DegToRad(angle);
 m_fontHeight   :=height;
 m_fontCacheType:=cache;
 m_fontFileName :=FileName;
 m_fontItalic   :=italic;
 m_fontWeight:=Weight;

  FontEngineCreate;
end;

procedure TAgxCanvas.FontBoldSet(const val : Boolean);
 begin
  if val then
   m_fontWeight:=700 else
   m_fontWeight:=400;
 end;
function TAgxCanvas.FontBoldGet:Boolean;
begin
 result:=(m_fontWeight=700);
end;

procedure TAgxCanvas.FontItalicSet(const val : Boolean);
 begin
  m_fontItalic :=val;
 end;
function TAgxCanvas.FontItalicGet:Boolean;
begin
 result:=m_fontItalic;
end;

procedure TAgxCanvas.FontHeightSet(const val : double);   //=============================================== 7777
begin
 if val=m_fontHeight then exit;

 m_fontHeight:=val;

 Font(m_FontFileName, m_fontHeight, FontBoldGet, m_fontItalic,m_fontCacheType, radtodeg(m_textAngle) );
end;
function TAgxCanvas.FontHeightGet:double;
begin
 result:=m_fontHeight;
end;

procedure TAgxCanvas.FontFileNameSet(const val : AnsiString);
 begin
 m_FontFileName:=val;
end;
function  TAgxCanvas.FontFileNameGet : AnsiString;
 begin
 result:=m_FontFileName;
end;

procedure TAgxCanvas.FontWeightSet(const val : int);
 begin
 m_FontWeight:=val;
end;
function  TAgxCanvas.FontWeightGet : int;
 begin
 result:=m_FontWeight;
end;

procedure TAgxCanvas.TextAlignmentSet(const alignX ,alignY : TAgxTextAlignment );
begin
 m_textAlignX:=alignX;
 m_textAlignY:=alignY;
end;

procedure TAgxCanvas.TextHintsSet(const hints : boolean );
begin
 m_textHints:=hints;
end;
function TAgxCanvas.TextHintsGet : boolean;
begin
 result:=m_textHints;
end;

function TAgxCanvas.TextWidthGet(const str : AnsiString ) : double;
{$IFDEF AGG2D_NO_FONT}
begin
  Result:=0;
end;
{$ELSE}
var
 x ,y  : double;
 first : boolean;
 glyph : glyph_cache_ptr;
 str_  : PChar;

begin
 x:=0;
 y:=0;

 first:=true;
 str_ :=@str[1 ];

 while str_^ <> #0 do
  begin
   glyph:=m_fontCacheManager.glyph(int32u(str_^ ) );

   if glyph <> NIL then
    begin
      if not first then m_fontCacheManager.add_kerning(@x ,@y );
      x:=x + glyph^.advance_x;
      y:=y + glyph^.advance_y;
      first:=false;
    end;
   inc(ptrcomp(str_ ) );

  end;

 if m_fontCacheType = AGX_VectorFontCache then
  result:=x else
  result:=ScreenToWorld(x);

end;
{$ENDIF}

function  TAgxCanvas.TextHeightGet: double;
 begin
  result:=m_fontHeight;
 end;

procedure TAgxCanvas.TextAngleSet(const val:double);
 begin
  m_TextAngle:=val;
 end;

function  TAgxCanvas.TextAngleGet:double;
 begin
  result:=m_TextAngle;
 end;

procedure TAgxCanvas.Text(const X,Y:double; const str:AnsiString; const roundOff:boolean=false; const ddx:double=0.0; const ddy:double=0.0);
{$IFDEF AGG2D_NO_FONT}
begin

end;
{$ELSE}
var
 dx ,dy ,asc ,start_x ,start_y : double;
 glyph : glyph_cache_ptr;
 mtx  : trans_affine;
 str_ : PChar;
 i : int;
 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tr : conv_transform;
begin
 dx:=0.0;
 dy:=0.0;

 case m_textAlignX of
  AGX_AlignCenter : dx:=-textWidthGet(str ) * 0.5;
  AGX_AlignRight  : dx:=-textWidthGet(str );
 end;

 asc  :=fontHeightGet;
 glyph:=m_fontCacheManager.glyph(int32u('H' ) );

 if glyph <> NIL then asc:=glyph^.bounds.y2 - glyph^.bounds.y1;

 if m_fontCacheType = AGX_RasterFontCache then  asc:=screenToWorld(asc );

 case m_textAlignY of
  AGX_AlignCenter : dy:=-asc * 0.5;
  AGX_AlignTop    :dy:=-asc;
 end;

 if m_fontEngine._flip_y then dy:=-dy;
 mtx.Construct;
 start_x:=x + dx;
 start_y:=y + dy;

 if roundOff then
  begin
   start_x:=Trunc(start_x );
   start_y:=Trunc(start_y );
  end;

 start_x:=start_x + ddx;
 start_y:=start_y + ddy;

 tat.Construct(-x ,-y );
 mtx.multiply (@tat );

 tar.Construct(m_textAngle );
 mtx.multiply (@tar );

 tat.Construct(x ,y );
 mtx.multiply (@tat );

 tr.Construct(m_fontCacheManager.path_adaptor ,@mtx );

 if m_fontCacheType = AGX_RasterFontCache then WorldToScreen(@start_x ,@start_y );

 i:=0;

 str_:=@str[1];

 if m_fontCacheType = AGX_VectorFontCache then   m_path.remove_all;

 while char_ptr(ptrcomp(str_ ) + i * sizeof(char ) )^ <> #0 do
  begin
   glyph:=m_fontCacheManager.glyph(int32u(char_ptr(ptrcomp(str_ ) + i * sizeof(char ) )^ ) );

   if glyph <> NIL then
    begin
     if i <> 0 then m_fontCacheManager.add_kerning(@x ,@y );
     m_fontCacheManager.init_embedded_adaptors(glyph ,start_x ,start_y );

     if glyph^.data_type = glyph_data_outline then
      begin
       // m_path.remove_all;
       m_path.add_path(@tr ,0 ,false);
       //drawPath;
      end;

     if glyph^.data_type = glyph_data_gray8 then
      begin
       render( m_fontCacheManager.gray8_adaptor , m_fontCacheManager.gray8_scanline );
      end;

     start_x:=start_x + glyph^.advance_x;
     start_y:=start_y + glyph^.advance_y;
    end;
   inc(i );
  end;

 if m_fontCacheType = AGX_VectorFontCache then drawPath;
end;
{$ENDIF}

procedure TAgxCanvas.ResetPath;
begin
 m_path.remove_all;
 m_path.move_to(0 ,0 );
end;

procedure TAgxCanvas.PathGetBoundRect(var x1,y1,x2,y2:double);
 var x ,y : double;
     i:integer;
 begin
   x1:=0;
   y1:=0;
   x2:=0;
   y2:=0;
   if m_path.m_total_vertices=0 then exit;

   for i := 0 to m_path.m_total_vertices-1 do
    begin
     m_path.vertex_(i,@x ,@y);
     if(x < x1) then x1 := x;
     if(y < y1) then y1 := y;
     if(x > x2) then x2 := x;
     if(y > y2) then y2 := y;
    end;

    if x1<0 then x1:=0;
    if y1<0 then y1:=0;
 end;

procedure TAgxCanvas.PathFillWithImage(Img:Tbitmap);
 var x1,y1,x2,y2:Double;
 begin
   if Img=nil then exit;
   PathGetBoundRect(x1,y1,x2,y2);
   TransformImagePath(Img,0,0,Img.Width,Img.Height,x1,y1,x2,y2);
 end;


procedure TAgxCanvas.MoveTo(const x ,y : double );
begin
 m_path.move_to(x ,y );
end;

procedure TAgxCanvas.MoveRel(const dx ,dy : double );
begin
 m_path.move_rel(dx ,dy );
end;

procedure TAgxCanvas.LineTo(const x ,y : double );
begin
 m_path.line_to(x ,y );
end;

procedure TAgxCanvas.LineRel(const dx ,dy : double );
begin
 m_path.line_rel(dx ,dy );

end;

procedure TAgxCanvas.HorLineTo(const x : double );
begin
 m_path.hline_to(x );
end;

procedure TAgxCanvas.HorLineRel(const dx : double );
begin
 m_path.hline_rel(dx );
end;

procedure TAgxCanvas.VerLineTo(const y : double );
begin
 m_path.vline_to(y );
end;

procedure TAgxCanvas.VerLineRel(const dy : double );
begin
 m_path.vline_rel(dy );
end;

procedure TAgxCanvas.ArcTo(const rx,ry,angle:double; const largeArcFlag,sweepFlag:boolean; const x,y:double );
begin
 m_path.arc_to(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,x ,y );
end;

procedure TAgxCanvas.ArcRel(const rx,ry,angle:double; const largeArcFlag,sweepFlag:boolean; const dx,dy : double );
begin
 m_path.arc_rel(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,dx ,dy );
end;

procedure TAgxCanvas.QuadricCurveTo (const xCtrl ,yCtrl ,xTo ,yTo : double );
begin
 m_path.curve3(xCtrl ,yCtrl ,xTo ,yTo );
end;

procedure TAgxCanvas.QuadricCurveRel(const dxCtrl ,dyCtrl ,dxTo ,dyTo : double );
begin
 m_path.curve3_rel(dxCtrl ,dyCtrl ,dxTo ,dyTo );
end;

procedure TAgxCanvas.QuadricCurveTo (const xTo ,yTo : double );
begin
 m_path.curve3(xTo ,yTo );
end;

procedure TAgxCanvas.QuadricCurveRel(const dxTo ,dyTo : double );
begin
 m_path.curve3_rel(dxTo ,dyTo );
end;

procedure TAgxCanvas.CubicCurveTo (const xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4(xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo );
end;

procedure TAgxCanvas.CubicCurveRel(const dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );
begin
 m_path.curve4_rel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );
end;

procedure TAgxCanvas.CubicCurveTo (const xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4(xCtrl2 ,yCtrl2 ,xTo ,yTo );
end;

procedure TAgxCanvas.CubicCurveRel(const dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );
begin
 m_path.curve4_rel(dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );
end;

procedure TAgxCanvas.AddEllipse(const cx ,cy ,rx ,ry : double; const dir : TAgxDirection );
var ar : bezier_arc;
begin
 if dir = AGX_CCW then
  ar.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi ) else
  ar.Construct(cx ,cy ,rx ,ry ,0 ,-2 * pi );

 m_path.add_path(@ar ,0 ,false );
 m_path.close_polygon;
end;

procedure TAgxCanvas.ClosePolygon;
begin
 m_path.close_polygon;
end;

procedure TAgxCanvas.DrawPath(const flag : TAgxDrawPathFlag = AGX_FillAndStroke );
begin
 m_rasterizer.reset;

 case flag of
  AGX_FillOnly :
   if m_fillColor.a <> 0 then
    begin
     m_rasterizer.add_path(@m_pathTransform );

     render(true );

    end;

  AGX_StrokeOnly :
   if (m_lineColor.a <> 0 ) and
      (m_lineWidth > 0.0 ) then
    begin
     m_rasterizer.add_path(@m_strokeTransform );

     render(false );

    end;

  AGX_FillAndStroke :
   begin
    if m_fillColor.a <> 0 then
     begin
      m_rasterizer.add_path(@m_pathTransform );

      render(true );

     end;

    if (m_lineColor.a <> 0 ) and
       (m_lineWidth > 0.0 ) then
     begin
      m_rasterizer.add_path(@m_strokeTransform );

      render(false );

     end;

   end;

  AGX_FillWithLineColor :
   if m_lineColor.a <> 0 then
    begin
     m_rasterizer.add_path(@m_pathTransform );

     render(false );

    end;

 end;

end;

{ IMAGEFILTER }
procedure TAgxCanvas.ImageFilterSet(const f : TAgxImageFilter );
begin
 m_imageFilter:=f;

 case f of
  AGX_Bilinear :
   m_imageFilterLut.calculate(@m_ifBilinear ,true );

  AGX_Hanning :
   m_imageFilterLut.calculate(@m_ifHanning ,true );

  AGX_Hermite :
   m_imageFilterLut.calculate(@m_ifHermite ,true );

  AGX_Quadric :
   m_imageFilterLut.calculate(@m_ifQuadric ,true );

  AGX_Bicubic :
   m_imageFilterLut.calculate(@m_ifBicubic ,true );

  AGX_Catrom :
   m_imageFilterLut.calculate(@m_ifCatrom ,true );

  AGX_Spline16 :
   m_imageFilterLut.calculate(@m_ifSpline16 ,true );

  AGX_Spline36 :
   m_imageFilterLut.calculate(@m_ifSpline36 ,true );

  AGX_Blackman144 :
   m_imageFilterLut.calculate(@m_ifBlackman144 ,true );

 end;
end;

function TAgxCanvas.ImageFilterGet : TAgxImageFilter;
begin
 result:=m_imageFilter;
end;

procedure TAgxCanvas.ImageResampleSet(const f : TAgxImageResample );
begin
 m_imageResample:=f;
end;

function TAgxCanvas.ImageResampleGet : TAgxImageResample;
begin
 result:=m_imageResample;
end;

procedure TAgxCanvas.ImageFlip(const f : boolean );
begin
 m_imageFlip:=f;
end;

procedure TAgxCanvas.TransformImage(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const dstX1,dstY1,dstX2,dstY2:double);
var
 parall : array[0..5 ] of double;
 image  : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   resetPath;
   moveTo(dstX1 ,dstY1 );
   lineTo(dstX2 ,dstY1 );
   lineTo(dstX2 ,dstY2 );
   lineTo(dstX1 ,dstY2 );
   closePolygon;
   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;
   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.TransformImage(bitmap : TBitmap; const dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;
 image  : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   ResetPath;
   MoveTo(dstX1 ,dstY1 );
   LineTo(dstX2 ,dstY1 );
   LineTo(dstX2 ,dstY2 );
   LineTo(dstX1 ,dstY2 );
   ClosePolygon;

   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;
   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,@parall[0 ] );
   image.Destruct;
  end;
end;



procedure TAgxCanvas.TransformImage(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const parallelo:PDouble);
 var image : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   ResetPath;
   MoveTo( PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ ,
           PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ );
   LineTo( PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
          PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

   LineTo( PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ ,
           PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ );
   LineTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

   ClosePolygon;
   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelo );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.TransformImage(bitmap : TBitmap; const parallelo : PDouble );
  var image : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   ResetPath;
   MoveTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ );
   LineTo(
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );
   LineTo(
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ );
   LineTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );
   ClosePolygon;
   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,parallelo );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.TransformImagePath(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const dstX1,dstY1,dstX2,dstY2:double);
var parall : array[0..5 ] of double;
    image  : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;
   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.TransformImagePath(bitmap:TBitmap; const dstX1,dstY1,dstX2,dstY2:double);
var parall : array[0..5 ] of double;
    image  : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   parall[0 ]:=dstX1;
   parall[1 ]:=dstY1;
   parall[2 ]:=dstX2;
   parall[3 ]:=dstY1;
   parall[4 ]:=dstX2;
   parall[5 ]:=dstY2;
   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,@parall[0 ] );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.TransformImagePath(bitmap:TBitmap; const imgX1,imgY1,imgX2,imgY2:integer; const parallelo:PDouble);
var image : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   renderImage(@image ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelo );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.TransformImagePath(bitmap:TBitmap; const parallelo:PDouble );
 var image : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip ) then
  begin
   renderImage(@image ,0 ,0 ,image.renBuf._width ,image.renBuf._height ,parallelo );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.CopyImage(
           bitmap : TBitmap;
           const imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
           const dstX ,dstY : double );
var
 r     : AGG_basics.rect;
 image : TAgxImage;
begin
 image.Construct;

 if image.attach(bitmap ,m_imageFlip ) then
  begin
   WorldToScreen(@dstX ,@dstY );
   r.Construct(imgX1 ,imgY1 ,imgX2 ,imgY2 );
   m_renBase.copy_from(@image.renBuf ,@r ,Trunc(dstX ) - imgX1 ,Trunc(dstY ) - imgY1 );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.CopyImage(bitmap : TBitmap; const dstX ,dstY : double );
var image : TAgxImage;
begin
 image.Construct;
 if image.attach(bitmap ,m_imageFlip) then
  begin
   WorldToScreen(@dstX ,@dstY );
   m_renBase.copy_from(@image.renBuf ,NIL ,Trunc(dstX ) ,Trunc(dstY ) );
   image.Destruct;
  end;
end;


procedure TAgxCanvas.render(const fillColor_ : boolean );
begin
 if (m_blendMode = AGX_BlendAlpha ) or (m_pixf = pf24bit ) then
    Agg2DRenderer_render(self ,@m_renBase ,@m_renSolid ,fillColor_ ) else
    Agg2DRenderer_render(self ,@m_renBaseComp ,@m_renSolidComp ,fillColor_ );
end;

procedure TAgxCanvas.render(ras : PAgxFontRasterizer; sl : PAgxFontScanline );
begin
 if (m_blendMode = AGX_BlendAlpha ) or (m_pixf = pf24bit ) then
  Agg2DRenderer_render(self ,@m_renBase ,@m_renSolid ,ras ,sl ) else
  Agg2DRenderer_render(self ,@m_renBaseComp ,@m_renSolidComp ,ras ,sl );
end;

procedure TAgxCanvas.addLine(const x1 ,y1 ,x2 ,y2 : double );
begin
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y2 );
end;

procedure TAgxCanvas.updateRasterizerGamma;
begin
 m_gammaAgg2D.Construct(m_masterAlpha ,m_antiAliasGamma );
 m_rasterizer.gamma    (@m_gammaAgg2D );
end;


procedure TAgxCanvas.renderImage(
           img : PAGXImage;
           const x1 ,y1 ,x2 ,y2 : integer;
           const parl : PDouble );
var
 mtx : trans_affine;

 interpolator : span_interpolator_linear;

begin
 mtx.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(parl ) );
 mtx.multiply (@m_transform );
 mtx.invert;

 m_rasterizer.reset;
 m_rasterizer.add_path(@m_pathTransform );

 interpolator.Construct(@mtx );

 if (m_blendMode = AGX_BlendAlpha ) or  (m_pixf = pf24bit ) then
  Agg2DRenderer_renderImage(self ,img ,@m_renBasePre ,@interpolator ) else
  Agg2DRenderer_renderImage(self ,img ,@m_renBaseCompPre ,@interpolator );

end;


function TAgxCanvas.FontUsesFreeType : boolean;
begin
{$IFDEF AGG2D_USE_FREETYPE }
 result:=true;
{$ELSE }
 result:=false;
{$ENDIF }

end;

 function TAgxCanvas.FontUsesWin32TrueType: boolean;
begin
{$IFDEF AGG2D_USE_WINFONTS }
 result:=true;
{$ELSE }
 result:=false;
{$ENDIF }
end;

END.
