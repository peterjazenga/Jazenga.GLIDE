
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Graphics;

{$MODE Delphi}


interface

uses
  LCLIntf, LCLType, LMessages, Types,
  SysUtils, Classes, Graphics
  , Dialogs, FileUtil, lazfileutils //=== ct9999 =====
  , GR32
  , GR32_Blend
  , GR32_Filters
  , GR32_Resamplers
  , XGR32_FilterEx
  , XGR32_GraphUtils
  , XGR32_Effects
  ;

const
  DefaultWallpaperAlphaValue = 250;
  DefaultImageBorderAlphaValue = 255;

type

  TFontQuality = (fqNone, fqLow, fqNormal, fqHigh);

  PRGBColor32Array = ^TRGBColor32Array;
  TRGBColor32Array = array [0..1024] of TColor32Rec;

  TGradientColors = array[0..255] of TColor32Rec;

  TGradientShift = -100..100;
  TGradientRotation = -100..100;

  //@param gsPattern Load Pattern from picture file!
  TGradientStyle = (gsNone, gsCustom, gsRadialC, gsRadialT, gsRadialB, gsRadialL,
    gsRadialR, gsRadialTL, gsRadialTR, gsRadialBL, gsRadialBR, gsLinearH,
    gsLinearV, gsReflectedH, gsReflectedV, gsDiagonalLF, gsDiagonalLB,
    gsDiagonalRF, gsDiagonalRB, gsArrowL, gsArrowR, gsArrowU, gsArrowD,
    gsDiamond, gsButterfly, gsPattern);


type
  TWallpaperStyle = (wlpsCenter, wlpsTile, wlpsStretch);
  TGRBackgroundClass = class of TGRBackground;
  
  TGRGradient = class;
  TGRWallpaper = class;
  TGRBackground = class;
  TGRStyle = class;
  TCustomGradientEvent = procedure (Sender: TObject; const Colors:
    TGradientColors; Pattern: TBitmap32) of object;

  {
  Call the paintTo method for use.
  }
  TGRGradient = class(TCustomGraphicProperty)
  private
    Dirty: Boolean;
    FAlphaBegin: Byte;
    FAlphaChannel: Boolean;
    FAlphaEnd: Byte;
    FColorBegin: TColor;
    FColorEnd: TColor;
    FOnCustom: TCustomGradientEvent;
    FPattern: TBitmap32;
    FPatternFile: string;
    FReverse: Boolean;
    FRotation: TGradientRotation;
    FShift: TGradientShift;
    FStyle: TGradientStyle;
    FTiled: Boolean;
    FUseSysColors: Boolean;
    procedure SetAlphaBegin(const Value: Byte);
    procedure SetAlphaChannel(const Value: Boolean);
    procedure SetAlphaEnd(const Value: Byte);
    procedure SetColorBegin(Value: TColor);
    procedure SetColorEnd(Value: TColor);
    procedure SetPatternFile(const Value: string);
    procedure SetReverse(Value: Boolean);
    procedure SetRotation(Value: TGradientRotation);
    procedure SetShift(Value: TGradientShift);
    procedure SetStyle(Value: TGradientStyle);
    procedure SetTiled(const Value: Boolean);
    procedure SetUseSysColors(Value: Boolean);
  protected
    procedure iAssign(Source: TPersistent); override;
    procedure InternalUpdate; override;
    function IsColorBeginSaved: Boolean;
    function IsColorEndSaved: Boolean;
    procedure Loaded;
    procedure UpdateSysColors; virtual;
    procedure WMSettingChange;
    property Pattern: TBitmap32 read FPattern;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CopyPatternTo(Bitmap: TBitmap32): Boolean;
    procedure InvalidatePattern;
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;
  published
    property AlphaBegin: Byte read FAlphaBegin write SetAlphaBegin default 255;
    property AlphaChannel: Boolean read FAlphaChannel write SetAlphaChannel;
    property AlphaEnd: Byte read FAlphaEnd write SetAlphaEnd default 255;
    property ColorBegin: TColor read FColorBegin write SetColorBegin stored
      IsColorBeginSaved;
    property ColorEnd: TColor read FColorEnd write SetColorEnd stored
      IsColorEndSaved;
    property OnCustom: TCustomGradientEvent read FOnCustom write FOnCustom;
    property PatternFile: string read FPatternFile write SetPatternFile;
    property Reverse: Boolean read FReverse write SetReverse default False;
    property Rotation: TGradientRotation read FRotation write SetRotation
      default 0;
    property Shift: TGradientShift read FShift write SetShift default 0;
    property Style: TGradientStyle read FStyle write SetStyle default gsRadialC;
    property Tiled: Boolean read FTiled write SetTiled;
    property UseSysColors: Boolean read FUseSysColors write SetUseSysColors
      default False;
  end;
  

  TGRWallpaper = class(TCustomGraphicProperty)
  private
    FAlpha: Byte;
    FFileName: string;
    FTransparent: Boolean;
    procedure SetAlpha(const Value: Byte);
    procedure SetFileName(const Value: string);
    procedure SetStyle(const Value: TWallpaperStyle);
    procedure SetTransparent(const Value: Boolean);
  protected
    FPicture: TPicture;
    FStyle: TWallpaperStyle;
    procedure iAssign(Source: TPersistent); override;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function Empty: Boolean;
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;
  published
    { Summary 0 means fully transparent. }
    { Description
    0-$FF, $FF means no AlphaBlending!
    }
    property Alpha: Byte read FAlpha write SetAlpha default
      DefaultWallpaperAlphaValue;
    property FileName: string read FFileName write SetFileName;
    property OnChanged;
    property Picture: TPicture read FPicture;
    property Style: TWallpaperStyle read FStyle write SetStyle;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;
  
  { Description
    LeftTop, Top, RightTop, Left, Right, LeftBottom, Bottom, RightBottom:
    TPicture;
    MiddleMask: TPicture; ΏΙΤΟρ²£Α§ΥΦ°ΡΔΪΘέΥΦΖπ£΅±ίΏςΚΗΧξΊσ»­£¨ΟΘ»­ΔΪΘέ£©£΅
    Width: TRect; ±ίΏςΛΔ±ίµΔΏν¶Θ΅£LeftWidth, TopWidth, BottomWidth, RightWidth:
    of the border; ΘηΉϋ²»ΙθΦΓΎΝΤL, R, T, BΝΌΟρΐ΄ΌΖΛγ΅£
    CalcClientRect(const aSrc: TRect): TRect; ΌΖΛγ³ύΑΛ±ίΏςΊσµΔΏΝ»§ΗψΏν¶Θ΅£
    »ζΦΖΛ³ΠςΞΟΘ»­ΛΔ±ί(L,R,T,B)ΤΩ»­ΛΔ½Η(LT, RT, LB, RB).
    ΧξΠ΅£¬Φ»¶¥±ίΌ΄ΏΙ»­£΅Ζδ΄ΞΏΙΤΌΣΙΟLeftTop, Θ»ΊσΚΗRightTop.ΤΩ½ΣΧΕΚΗ Left.
    ¶ΤΣΪ½ΗΣ¦ΈΓFlip£¬µ«ΚΗ¶ΤΣΪ±ίΎΝΣ¦ΈΓΚΗΠύΧ΅£
  }
  TGRImageBorder = class(TCustomGraphicProperty)
  private
    FAlpha: Byte;
    FFileNameBottom: string;
    FFileNameLeft: string;
    FFileNameLeftBottom: string;
    FFileNameLeftTop: string;
    FFileNameMask: string;
    FFileNameRight: string;
    FFileNameRightBottom: string;
    FFileNameRightTop: string;
    FFileNameTop: string;
    FFrameSides: TFrameSides;
    FPictureLeftTop: TBItmap32;
    FPictureMask: TBItmap32;
    FWidth: TRectProperty;
    procedure SetAlpha(const Value: Byte);
    procedure SetFileNameBottom(const Value: string);
    procedure SetFileNameLeft(const Value: string);
    procedure SetFileNameLeftBottom(const Value: string);
    procedure SetFileNameLeftTop(const Value: string);
    procedure SetFileNameMask(const Value: string);
    procedure SetFileNameRight(const Value: string);
    procedure SetFileNameRightBottom(const Value: string);
    procedure SetFileNameRightTop(const Value: string);
    procedure SetFileNameTop(const Value: string);
    procedure SetFrameSides(const Value: TFrameSides);
  protected
    FPictureBottom: TBItmap32;
    FPictureLeft: TBItmap32;
    FPictureLeftBottom: TBItmap32;
    FPictureRight: TBItmap32;
    FPictureRightBottom: TBItmap32;
    FPictureRightTop: TBItmap32;
    FPictureTop: TBItmap32;
    { Summary whether the user modified width property, }
    FWidthModified: Boolean;
    procedure iAssign(Source: TPersistent); override;
    procedure PictureChanged(Sender: TObject);
    { Summary calc the border width via Picture. }
    procedure ReCalcWidth;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    { Summary ΌΖΛγ³ύΑΛ±ίΏςΊσµΔΏΝ»§ΗψΏν¶Θ }
    function CalcClientRect(const aSrc: TRect): TRect;
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;
    { Summary 0 means fully transparent. }
    { Description
    0-$FF, $FF means no AlphaBlending!
    }
    property Alpha: Byte read FAlpha write SetAlpha default
      DefaultImageBorderAlphaValue;
    property OnChanged;
    { Summary the top Border pic }
    property PictureBottom: TBItmap32 read FPictureBottom;
    { Summary the top Border pic }
    property PictureLeft: TBItmap32 read FPictureLeft;
    { Summary the top Border pic }
    property PictureLeftBottom: TBItmap32 read FPictureLeftBottom;
    { Summary the top Border pic }
    property PictureLeftTop: TBItmap32 read FPictureLeftTop;
    { Summary the top Border pic }
    property PictureMask: TBItmap32 read FPictureMask;
    { Summary the top Border pic }
    property PictureRight: TBItmap32 read FPictureRight;
    { Summary the top Border pic }
    property PictureRightBottom: TBItmap32 read FPictureRightBottom;
    { Summary the top Border pic }
    property PictureRightTop: TBItmap32 read FPictureRightTop;
    { Summary the top Border pic }
    { Description
    this is MUST EXISTS PIC.
    }
    property PictureTop: TBItmap32 read FPictureTop;
  published
    property Enabled;
    property FileNameBottom: string read FFileNameBottom write
      SetFileNameBottom;
    { Summary the picture file name for left border. }
    property FileNameLeft: string read FFileNameLeft write SetFileNameLeft;
    property FileNameLeftBottom: string read FFileNameLeftBottom write
      SetFileNameLeftBottom;
    property FileNameLeftTop: string read FFileNameLeftTop write
      SetFileNameLeftTop;
    property FileNameMask: string read FFileNameMask write SetFileNameMask;
    property FileNameRight: string read FFileNameRight write SetFileNameRight;
    property FileNameRightBottom: string read FFileNameRightBottom write
      SetFileNameRightBottom;
    property FileNameRightTop: string read FFileNameRightTop write
      SetFileNameRightTop;
    property FileNameTop: string read FFileNameTop write SetFileNameTop;
    property FrameSides: TFrameSides read FFrameSides write SetFrameSides
      default ALLFRAME_SIDES;
    property Width: TRectProperty read FWidth;
  end;
  
  { Description
    LeftTop, Top, RightTop, Left, Right, LeftBottom, Bottom, RightBottom:
    TPicture;
    MiddleMask: TPicture; ΏΙΤΟρ²£Α§ΥΦ°ΡΔΪΘέΥΦΖπ£΅±ίΏςΚΗΧξΊσ»­£¨ΟΘ»­ΔΪΘέ£©£΅
    Width: TRect; ±ίΏςΛΔ±ίµΔΏν¶Θ΅£LeftWidth, TopWidth, BottomWidth, RightWidth:
    of the border; ΘηΉϋ²»ΙθΦΓΎΝΤL, R, T, BΝΌΟρΐ΄ΌΖΛγ΅£
    CalcClientRect(const aSrc: TRect): TRect; ΌΖΛγ³ύΑΛ±ίΏςΊσµΔΏΝ»§ΗψΏν¶Θ΅£
    »ζΦΖΛ³ΠςΞΟΘ»­ΛΔ±ί(L,R,T,B)ΤΩ»­ΛΔ½Η(LT, RT, LB, RB).
    ΧξΠ΅£¬Φ»¶¥±ίΌ΄ΏΙ»­£΅Ζδ΄ΞΏΙΤΌΣΙΟLeftTop, Θ»ΊσΚΗRightTop.ΤΩ½ΣΧΕΚΗ Left.
    ¶ΤΣΪ½ΗΣ¦ΈΓFlip£¬µ«ΚΗ¶ΤΣΪ±ίΎΝΣ¦ΈΓΚΗΠύΧ΅£
  }
  TGRFrame = class(TCustomGraphicProperty)
  private
    FAlpha: Byte;
    FColor: TColor;
    FFrameSides: TFrameSides;
    FFrameStyle: TFrameStyle;
    FImageFrame: TGRImageBorder;
    FWidth: Integer;
    procedure SetAlpha(const Value: Byte);
    procedure SetColor(const Value: TColor);
    procedure SetFrameSides(const Value: TFrameSides);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetWidth(const Value: Integer);
  protected
    { Summary whether the user modified width property, }
    FWidthModified: Boolean;
    procedure BorderChanged(Sender: TObject);
    procedure iAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    { Summary ΌΖΛγ³ύΑΛ±ίΏςΊσµΔΏΝ»§ΗψΏν¶Θ }
    function CalcClientRect(const aSrc: TRect): TRect;
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;
    { Summary 0 means fully transparent. }
    { Description
    0-$FF, $FF means no AlphaBlending!
    }
    property Alpha: Byte read FAlpha write SetAlpha default
      DefaultImageBorderAlphaValue;
  published
    property Color: TColor read FColor write SetColor;
    property Enabled;
    property FrameSides: TFrameSides read FFrameSides write SetFrameSides
      default ALLFRAME_SIDES;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle;
    property ImageFrame: TGRImageBorder read FImageFrame;
    property OnChanged;
    property Width: Integer read FWidth write SetWidth;
  end;
  
  { Description
  1. First draw the Wallpaper(if any), 
  2. then draw the Gradient(if any) alpha blending, 
   if Gradient is AlphaChannel then alpha blending with Texture. 
  3.draw the Texture(if any).
  4.At last apply the mask if any.
  
  NOTE: I used the Wallpaper.Alpha as the Background Alpha.
  }
  TGRBackground = class(TCustomGraphicProperty)
  private
    FBuffered: Boolean;
    FColor: TColor;
    FMask: TBitmap32;
    FTexture: TGRWallpaper;
    procedure SetBuffered(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetGradient(const Value: TGRGradient);
    procedure SetMask(Value: TBitmap32);
    procedure SetTexture(const Value: TGRWallpaper);
    procedure SetWallpaper(const Value: TGRWallpaper);
  protected
    { Summary When Buffered }
    FBuffer: TBitmap32;
    FBufferDirty: Boolean;
    FGradient: TGRGradient;
    FWallpaper: TGRWallpaper;
    procedure HookChanged(Sender: TObject);
    procedure iAssign(Source: TPersistent); override;
    procedure InternalPaintTo(aBitmap32: TBitmap32; R:TRect); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function Empty: Boolean;
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;
    property Mask: TBitmap32 read FMask write SetMask;
  published
    property Buffered: Boolean read FBuffered write SetBuffered;
    property Color: TColor read FColor write SetColor default clNone;
    property Enabled;
    property Gradient: TGRGradient read FGradient write SetGradient;
    property Texture: TGRWallpaper read FTexture write SetTexture;
    property Wallpaper: TGRWallpaper read FWallpaper write SetWallpaper;
  end;
  
  { Description
  Note: 
    before use this, you must assign your bitmap32 to the Font
    LOldFont := TFont32.Create;
  try
    LOldFont.Assign(aBitmap32.Font);
    aBitmap32.Font := FFont32;
  finally
    aBitmap32.Font := LOldFont;
    LOldFont.Free;
  end;  
  }
  TFont32 = class(TFont)
  private
    FBackground: TGRBackground;
    FCharSpacing: Integer;
    FLineSpacing: Integer;
    FOpacity: Byte;
    FOutline: Boolean;
    FQuality: TFontQuality;
    FShadow: TShadowEffect;
    procedure SetBackground(const Value: TGRBackground);
    procedure SetCharSpacing(const Value: Integer);
    procedure SetLineSpacing(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    procedure SetOutline(const Value: Boolean);
    procedure SetQuality(const Value: TFontQuality);
    procedure SetShadow(Value: TShadowEffect);
  protected
    procedure DoPropertyChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Description
    @param aRect the return aRect is the text width and height
    
    the following aFormat Options are supported:
    DT_CALCRECT Determines the width and height of the rectangle. 
                If there are multiple lines of text, DrawText 
                uses the width of the rectangle pointed to by the 
                aRect parameter and extends the bottom of the rectangle 
                to bound the last line of text. If there is only one line 
                of text, DrawText modifies the right side of the rectangle 
                so that it bounds the last character in the line. In either 
                case, DrawText returns the height of the formatted text, but 
                does not draw the text.
    DT_TOP      Top justifies text (single line only)(default).
    DT_VCENTER  Centers text vertically (single line only).
    DT_BOTTOM   Justifies the text to the bottom of the rectangle. This 
                value must be combined with DT_SINGLELINE.
    DT_LEFT     Aligns text to the left(default).
    DT_CENTER   Centers text horizontally in the rectangle.
    DT_RIGHT    Aligns text to the right.
    DT_WORDBREAK Breaks words. Lines are automatically broken between words 
                 if a word extends past the edge of the rectangle specified 
                 by the aRect parameter. A carriage return-linefeed sequence 
                 also breaks the line.
    DT_NOPREFIX Turns off processing of prefix characters. Normally, 
                DrawText interprets the ampersand (&) mnemonic-prefix 
                character as a directive to underscore the character 
                that follows, and the double ampersand (&&) mnemonic-prefix 
                characters as a directive to print a single ampersand. 
                By specifying DT_NOPREFIX, this processing is turned off.
    DT_EXPANDTABS Expands tab characters. The default number of characters 
                  per tab is two.
    DT_END_ELLIPSIS For displayed text, if the end of a string does not fit 
                    in the rectangle, it is truncated and ellipses are added. 
                    If a word that is not at the end of the string goes beyond 
                    the limits of the rectangle, it is truncated without
                    ellipses.
    
    DT_NOCLIP
    }
    procedure DrawText(Dst: TBitmap32; Text: string; var aRect: TRect; aFormat: LongWord);
    procedure RenderText(Dst: TBitmap32; X, Y: Integer; const Text: String);
    procedure RenderTextW(Dst: TBitmap32; X, Y: Integer; const Text: WideString);
    function TextExtent(Dst: TBitmap32; const Text: string): TSize;
    function TextExtentW(Dst: TBitmap32; const Text: Widestring): TSize;
  published
    { Summary The Font background Texture. }
    property Background: TGRBackground read FBackground write SetBackground;
    { Summary Specifies the spacing between characters. }
    { Description
    TODO: not used yet!
    }
    property CharSpacing: Integer read FCharSpacing write SetCharSpacing;
    { Summary Specifies the spacing between Lines. }
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing;
    { Summary the alpha blending value. }
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    { Summary whether the text is outline only. }
    property Outline: Boolean read FOutline write SetOutline;
    { Summary the font antialiasing quality. }
    property Quality: TFontQuality read FQuality write SetQuality;
    { Summary the font shadow }
    property Shadow: TShadowEffect read FShadow write SetShadow;
  end;
  
  TGRStyle = class(TCustomGraphicProperty)
  private
    FBackground: TGRBackground;
    FColor: TColor;
    FFont: TFont32;
    FFrame: TGRFrame;
    procedure SetBackground(const Value: TGRBackground);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont32);
    procedure SetFrame(const Value: TGRFrame);
  protected
    procedure DoStyleChanged(Sender: TObject);
    procedure iAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property Font: TFont32 read FFont write SetFont;
  published
    property Background: TGRBackground read FBackground write SetBackground;
    property Color: TColor read FColor write SetColor;
    property Frame: TGRFrame read FFrame write SetFrame;
  end;
  

{ Summary Draw title to canvas Rect}
procedure DrawTileToCanvas(aCanvas: TCanvas; aR: TRect; aGraphic: TGraphic);

{ Description
  @param aR the paint area in the aDstBmp32. 
}
procedure DrawTileToBMP32(aDstBmp32: TBitmap32; aR: TRect; aSrcBmp32: TBitmap32);

{ Summary apply the color as transplarent to the Bitmap32 by riceball. }
function GetCurrentColorDepth : Integer;
function IsTrueColor: boolean;

type
  TStretchToDCTransparentFunc = procedure(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
    SrcBmp: TBitmap32; SrcX, SrcY, SrcW, SrcH: Integer);

var
  StretchToDCTransparentFunc: TStretchToDCTransparentFunc;
  
//Wrap Chars Supports
function WrapTextEx(const Line, BreakStr: string; const BreakChars: TSysCharSet;
  MaxCol: Integer): string; overload;

function WrapTextEx(const Line: string; MaxCol: Integer = 45): string; overload;

function IsEmptyPicture(const aPic: TPicture): Boolean;overload;
function IsEmptyPicture(const aPic: TBitmap32): Boolean;overload;

implementation

uses
  XGR32_BitmapEx;

function WrapTextEx(const Line: string; MaxCol: Integer): string;
begin
  Result := WrapTextEx(Line, sLineBreak, [' ', '-', #9], MaxCol); { do not localize }
end;

function WrapTextEx(const Line, BreakStr: string; const BreakChars: TSysCharSet; MaxCol: Integer): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
  L: Integer;
begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := #0;
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if CurChar in LeadBytes then
    begin
    //  L := CharLength(Line, Pos) - 1;  // SOS 9999
      Inc(Pos, L);
      Inc(Col, L);
      if (Col >= MaxCol) then BreakPos := Pos;
    end
    else
    begin
      if CurChar in QuoteChars then
        if QuoteChar = #0 then
          QuoteChar := CurChar
        else if CurChar = QuoteChar then
          QuoteChar := #0;
      if QuoteChar = #0 then   
      begin
        if CurChar = BreakStr[1] then
        begin
          ExistingBreak := StrLComp(Pointer(BreakStr), Pointer(@Line[Pos]), BreakLen) = 0;
          if ExistingBreak then
          begin
            Inc(Pos, BreakLen-1);
            BreakPos := Pos;
          end;
        end;
       
        if not ExistingBreak then
          if (Col >= MaxCol) or (CurChar in BreakChars) then
            BreakPos := Pos;
      end;
    end;

    Inc(Pos);
    Inc(Col);

    if not (QuoteChar in QuoteChars) and (ExistingBreak or
      ((Col >= MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := 1;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not (CurChar in QuoteChars) then
      begin
        while Pos <= LineLen do
        begin
          if Line[Pos] in BreakChars then
          begin
            Inc(Pos);
            ExistingBreak := False;
          end
          else
          begin
            ExistingBreak := StrLComp(Pointer(@Line[Pos]), sLineBreak, Length(sLineBreak)) = 0;
            if ExistingBreak then
              Inc(Pos, Length(sLineBreak))
            else
              Break;
          end;
        end;
      end;
      if (Pos <= LineLen) and not ExistingBreak then
        Result := Result + BreakStr;

      Inc(BreakPos);
      LinePos := BreakPos;
      Pos := LinePos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

function GetCurrentColorDepth : Integer;
var
  topDC : HDC;
begin
  topDC:=GetDC(0);
  try
    Result:=GetDeviceCaps(topDC, BITSPIXEL)*GetDeviceCaps(topDC, PLANES);
  finally
    ReleaseDC(0, topDC);
  end;
end;

function IsTrueColor: boolean;
begin
  Result := GetCurrentColorDepth >= 24;
end;

procedure DrawTileToBMP32(aDstBmp32: TBitmap32; aR: TRect; aSrcBmp32: TBitmap32);
var
  I, J, Tx, Ty: Integer;
  R: TRect;
  CachedBitmapRect: TRect;
begin
  CachedBitmapRect := Rect(0,0,aSrcBmp32.Width, aSrcBmp32.Height);
    with CachedBitmapRect do
    begin
      Tx := (aR.Right - aR.Left) div Right;
      Ty := (aR.Bottom - aR.Top) div Bottom;
      for J := 0 to Ty do
        for I := 0 to Tx do
        begin
          R := CachedBitmapRect;
          OffsetRect(R, aR.Left + Right * I, aR.Top + Bottom * J);
          if (R.Left < aR.Right) and (R.Top < aR.Bottom) then
            aSrcBmp32.DrawTo(aDstBmp32, R.Left, R.Top);
        end;
    end;
end;

procedure DrawTileToCanvas(aCanvas: TCanvas; aR: TRect; aGraphic: TGraphic);
var
  I, J, Tx, Ty: Integer;
  R: TRect;
  CachedBitmapRect: TRect;
begin
  CachedBitmapRect := Rect(0,0,aGraphic.Width, aGraphic.Height);
    with CachedBitmapRect do
    begin
      Tx := (aR.Right - aR.Left) div Right;
      Ty := (aR.Bottom - aR.Top) div Bottom;
      for J := 0 to Ty do
        for I := 0 to Tx do
        begin
          R := CachedBitmapRect;
          OffsetRect(R, aR.Left + Right * I, aR.Top + Bottom * J);
          aCanvas.Draw(R.Left, R.Top, aGraphic);
        end;
    end;
end;

procedure _StretchToDCTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcBmp: TBitmap32; SrcX, SrcY, SrcW, SrcH: Integer);
var
  SrcRect, DstRect: TRect;
  ResBmp: TBitmap32;
begin
  { Stretch }
  SrcRect := Rect(SrcX, SrcY, SrcX + SrcW, SrcY + SrcH);
  DstRect := Rect(DstX, DstY, DstX + DstW, DstY + DstH);
  { Stretch }
  ResBmp := TBitmap32.Create;
  try
    ResBmp.SetSize(DstW, DstH);
    { Copy DstDC  }
    BitBlt(ResBmp.Handle, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
    { Draw bitmap transparent to ResBmp }
    SrcBmp.DrawMode := dmBlend;
    SrcBmp.DrawTo(ResBmp, Rect(0, 0, DstW, DstH), SrcRect);
    { Draw ResBmp }
    BitBlt(DstDC, DstRect.Left, DstRect.Top, DstW, DstH, ResBmp.Handle, 0, 0, SRCCOPY);
  finally
    { Free resource }
    ResBmp.Free;
  end;
end;

{$ifdef MSWINDOWS}
(*
procedure Win2k_StretchToDCTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcBmp: TBitmap32; SrcX, SrcY, SrcW, SrcH: Integer);
begin
  { Use Win2k, WinXP TransparentBlt }
  TransparentBltFunc(DstDC, DstX, DstY, DstW, DstH, SrcBmp.Handle, SrcX, SrcY,
    SrcW, SrcH, LongInt(clBlack32));
end;//*)
{$endif}

{------ Gradient Pattern Paint Helper Proc ------}
procedure RadialCentral(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcX, PreCalcY: Integer;
  PreCalcXs: array[0..361] of Integer;
begin
  Pattern.SetSize(362, 362);
  //Pattern.Width := 362;
  //Pattern.Height := 362;
  for X := 0 to 180 do
  begin
    PreCalcX := 180 - X;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for X := 181 to 361 do
  begin
    PreCalcX := X - 181;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for Y := 0 to 361 do
  begin
    PreCalcY := 180 - Y;
    PreCalcY := PreCalcY * PreCalcY;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 361 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 361 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(180 - Y)))];
    for X := 181 to 361 do
      Row[X] := Colors[Round(Sqrt(Sqr(X - 181) + Sqr(180 - Y)))];
  end;
  }
end;

procedure RadialTop(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcX, PreCalcY: Integer;
  PreCalcXs: array[0..361] of Integer;
begin
  Pattern.SetSize(362, 181);
  //Pattern.Width := 362;
  //Pattern.Height := 181;
  for X := 0 to 180 do
  begin
    PreCalcX := 180 - X;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for X := 181 to 361 do
  begin
    PreCalcX := X - 181;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for Y := 0 to 180 do
  begin
    PreCalcY := Y * Y;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 361 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(Y)))];
    for X := 181 to 361 do
      Row[X] := Colors[Round(Sqrt(Sqr(X - 181) + Sqr(Y)))];
  end;
  }
end;

procedure RadialBottom(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcX, PreCalcY: Integer;
  PreCalcXs: array[0..361] of Integer;
begin
  Pattern.SetSize(362, 181);
  //Pattern.Width := 362;
  //Pattern.Height := 181;
  for X := 0 to 180 do
  begin
    PreCalcX := 180 - X;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for X := 181 to 361 do
  begin
    PreCalcX := X - 181;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for Y := 0 to 180 do
  begin
    PreCalcY := 180 - Y;
    PreCalcY := PreCalcY * PreCalcY;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 361 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(180 - Y)))];
    for X := 181 to 361 do
      Row[X] := Colors[Round(Sqrt(Sqr(X - 181) + Sqr(180 - Y)))];
  end;
  }
end;

procedure RadialLeft(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcY: Integer;
  PreCalcYs: array[0..361] of Integer;
begin
  Pattern.SetSize(181, 362);
  //Pattern.Width := 181;
  //Pattern.Height := 362;
  for Y := 0 to 180 do
  begin
    PreCalcY := 180 - Y;
    PreCalcYs[Y] := PreCalcY * PreCalcY;
  end;
  for Y := 181 to 361 do
  begin
    PreCalcY := Y - 181;
    PreCalcYs[Y] := PreCalcY * PreCalcY;
  end;
  for Y := 0 to 361 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row^[X] := Colors[Round(Sqrt(X * X + PreCalcYs[Y]))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(X) + Sqr(180 - Y)))];
  end;
  for Y := 181 to 361 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(X) + Sqr(Y - 181)))];
  end;
  }
end;

procedure RadialRight(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcX, PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
  PreCalcYs: array[0..361] of Integer;
begin
  Pattern.SetSize(181, 362);
  //Pattern.Width := 181;
  //Pattern.Height := 362;
  for X := 0 to 180 do
  begin
    PreCalcX := 180 - X;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for Y := 0 to 180 do
  begin
    PreCalcY := 180 - Y;
    PreCalcYs[Y] := PreCalcY * PreCalcY;
  end;
  for Y := 181 to 361 do
  begin
    PreCalcY := Y - 181;
    PreCalcYs[Y] := PreCalcY * PreCalcY;
  end;
  for Y := 0 to 361 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcYs[Y]))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(180 - Y)))];
  end;
  for Y := 181 to 361 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(Y - 181)))];
  end;
  }
end;

procedure RadialTopLeft(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.SetSize(181, 181);
  //Pattern.Width := 181;
  //Pattern.Height := 181;
  for X := 0 to 180 do
    PreCalcXs[X] := X * X;
  for Y := 0 to 180 do
  begin
    PreCalcY := Y * Y;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(X) + Sqr(Y)))];
  end;
  }
end;

procedure RadialTopRight(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcX, PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.SetSize(181, 181);
  //Pattern.Width := 181;
  //Pattern.Height := 181;
  for X := 0 to 180 do
  begin
    PreCalcX := 180 - X;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for Y := 0 to 180 do
  begin
    PreCalcY := Y * Y;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(Y)))];
  end;
  }
end;

procedure RadialBottomLeft(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.SetSize(181, 181);
  //Pattern.Width := 181;
  //Pattern.Height := 181;
  for X := 0 to 180 do
    PreCalcXs[X] := X * X;
  for Y := 0 to 180 do
  begin
    PreCalcY := 180 - Y;
    PreCalcY := PreCalcY * PreCalcY;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(X) + Sqr(180 - Y)))];
  end;
  }
end;

procedure RadialBottomRight(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
  PreCalcX, PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.SetSize(181, 181);
  //Pattern.Width := 181;
  //Pattern.Height := 181;
  for X := 0 to 180 do
  begin
    PreCalcX := 180 - X;
    PreCalcXs[X] := PreCalcX * PreCalcX;
  end;
  for Y := 0 to 180 do
  begin
    PreCalcY := 180 - Y;
    PreCalcY := PreCalcY * PreCalcY;
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row^[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
  { Not optimized code
  for Y := 0 to 180 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(Sqr(180 - X) + Sqr(180 - Y)))];
  end;
  }
end;

procedure LinearHorizontal(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 1);
  //Pattern.Width := 256;
  //Pattern.Height := 1;
  Row := PRGBColor32Array(Pattern.ScanLine[0]);
  for X := 0 to 255 do
    Row^[X] := Colors[X];
end;

procedure LinearVertical(const Colors: TGradientColors; Pattern: TBitmap32);
var
  Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(1, 256);
  //Pattern.Width := 1;
  //Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    Row^[0] := Colors[Y];
  end;
end;

procedure ReflectedHorizontal(const Colors: TGradientColors; Pattern: TBitmap32);
var
  Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(1, 512);
  //Pattern.Width := 1;
  //Pattern.Height := 512;
  for Y := 0 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    Row^[0] := Colors[255 - Y];
    Row := PRGBColor32Array(Pattern.ScanLine[511 - Y]);
    Row^[0] := Colors[255 - Y];
  end;
end;

procedure ReflectedVertical(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(512, 1);
  //Pattern.Width := 512;
  //Pattern.Height := 1;
  Row := PRGBColor32Array(Pattern.ScanLine[0]);
  for X := 0 to 255 do
  begin
    Row^[X] := Colors[255 - X];
    Row^[511 - X] := Colors[255 - X];
  end;
end;

procedure DiagonalLinearForward(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(128, 129);
  //Pattern.Width := 128;
  //Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[X + Y];
  end;
end;

procedure DiagonalLinearBackward(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(128, 129);
  //Pattern.Width := 128;
  //Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[127 + (Y - X)];
  end;
end;

procedure DiagonalReflectedForward(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 256);
  //Pattern.Width := 256;
  //Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 255 do
      if X + Y < 255 then
        Row^[X] := Colors[255 - (X + Y)]
      else
        Row^[X] := Colors[(Y + X) - 255];
  end;
end;

procedure DiagonalReflectedBackward(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 256);
  //Pattern.Width := 256;
  //Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 255 do
      if X > Y then
        Row^[X] := Colors[X - Y]
      else
        Row^[X] := Colors[Y - X];
  end;
end;

procedure ArrowLeft(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(129, 256);
  //Pattern.Width := 129;
  //Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row^[X] := Colors[255 - (X + Y)];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row^[X] := Colors[Y - X];
  end;
end;

procedure ArrowRight(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(129, 256);
  //Pattern.Width := 129;
  //Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row^[X] := Colors[(X - Y) + 127];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row^[X] := Colors[(X + Y) - 128];
  end;
end;

procedure ArrowUp(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 129);
  //Pattern.Width := 256;
  //Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[255 - (X + Y)];
    for X := 128 to 255 do
      Row^[X] := Colors[X - Y];
  end;
end;

procedure ArrowDown(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 129);
  //Pattern.Width := 256;
  //Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[127 + (Y - X)];
    for X := 128 to 255 do
      Row^[X] := Colors[(X + Y) - 128];
  end;
end;

procedure Diamond(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 256);
  //Pattern.Width := 256;
  //Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[255 - (X + Y)];
    for X := 128 to 255 do
      Row^[X] := Colors[X - Y];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[Y - X];
    for X := 128 to 255 do
      Row^[X] := Colors[(X + Y) - 255];
  end;
end;

procedure Butterfly(const Colors: TGradientColors; Pattern: TBitmap32);
var
  X, Y: Integer;
  Row: PRGBColor32Array;
begin
  Pattern.SetSize(256, 256);
  //Pattern.Width := 256;
  //Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[(X - Y) + 128];
    for X := 128 to 255 do
      Row^[X] := Colors[383 - (X + Y)];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBColor32Array(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row^[X] := Colors[(X + Y) - 128];
    for X := 128 to 255 do
      Row^[X] := Colors[128 + (Y - X)];
  end;
end;

function IsEmptyPicture(const aPic: TPicture): Boolean;overload;
begin
  Result := (aPic.Graphic = nil) or (aPic.Graphic.Empty);
end;

function IsEmptyPicture(const aPic: TBitmap32): Boolean;overload;
begin
  Result := (aPic = nil) or (aPic.Empty);
end;

{ For TGradient }
type
  TPatternBuilder = procedure(const Colors: TGradientColors; Pattern: TBitmap32);

const
  PatternBuilder: array[TGradientStyle] of TPatternBuilder = (nil, nil,
    @RadialCentral, @RadialTop, @RadialBottom, @RadialLeft, @RadialRight,
    @RadialTopLeft, @RadialTopRight, @RadialBottomLeft, @RadialBottomRight,
    @LinearHorizontal, @LinearVertical, @ReflectedHorizontal, @ReflectedVertical,
    @DiagonalLinearForward, @DiagonalLinearBackward, @DiagonalReflectedForward,
    @DiagonalReflectedBackward, @ArrowLeft, @ArrowRight, @ArrowUp, @ArrowDown,
    @Diamond, @Butterfly, nil);

constructor TGRGradient.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FColorBegin := clWhite;
  FColorEnd := clBtnFace;
  FStyle := gsRadialC;
  FShift := 0;
  FRotation := 0;
  FAlphaBegin := 255;
  FAlphaEnd := 255;
  FReverse := False;
  FUseSysColors := False;
  FPattern := TBitmap32.Create;
  FPattern.DrawMode := dmBlend;
  FPattern.CombineMode := cmMerge;

  TLinearResampler.Create(FPattern);

  //FPattern.PixelFormat := pf24bit;
  //FPattern.PixelFormat := pf32bit;
  Update;
end;

destructor TGRGradient.Destroy;
begin
  FPattern.Free;
  inherited Destroy;
end;

function TGRGradient.CopyPatternTo(Bitmap: TBitmap32): Boolean;
begin
  Result := False;
  if not Dirty and (FUpdateCount = 0) and Assigned(Bitmap) then
  begin
    Bitmap.Assign(Pattern);
    Result := True;
  end;
end;

procedure TGRGradient.iAssign(Source: TPersistent);
begin
  inherited iAssign(Source);
  if Source is TGRGradient then
    with Source as TGRGradient do
    begin
        Self.FAlphaChannel := AlphaChannel;
        Self.FColorBegin := ColorBegin;
        Self.FColorEnd := ColorEnd;
        Self.FAlphaBegin := AlphaBegin;
        Self.FAlphaEnd := AlphaEnd;
        Self.FPatternFile := PatternFile;
        Self.FReverse := Reverse;
        Self.FRotation := Rotation;
        Self.FShift := Shift;
        Self.FStyle := Style;
        Self.FTiled := Tiled;
        Self.FUseSysColors := UseSysColors;
    end;
end;

procedure TGRGradient.InternalUpdate;
var
  Colors: TGradientColors;
  dRed, dGreen, dBlue, dAlpha: Integer;
  RGBColor1, RGBColor2: TColor32;
  RGB1, RGB2: TColor32Rec;
  Index: Integer;
  M: Integer;
begin
  if Reverse then
  begin
    RGBColor1 := Color32(ColorEnd);
    TColor32Rec(RGBColor1).rgbAlpha := AlphaEnd;
    RGBColor2 := Color32(ColorBegin);
    TColor32Rec(RGBColor2).rgbAlpha := AlphaBegin;
  end
  else
  begin
    RGBColor1 := Color32(ColorBegin);
    TColor32Rec(RGBColor1).rgbAlpha := AlphaBegin;
    RGBColor2 := Color32(ColorEnd);
    TColor32Rec(RGBColor2).rgbAlpha := AlphaEnd;
  end;
  
  If FAlphaChannel Then
  Begin
    TColor32Rec(RGBColor1).rgbAlpha := TColor32Rec(RGBColor1).rgbBlue;
    TColor32Rec(RGBColor2).rgbAlpha := TColor32Rec(RGBColor2).rgbBlue;
  End;
  
  RGB1.Color := RGBColor1;
  RGB2.Color := RGBColor2;
  
  if Shift > 0 then
  begin
    Inc(RGB1.rgbRed, MulDiv(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    Inc(RGB1.rgbGreen, MulDiv(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    Inc(RGB1.rgbBlue, MulDiv(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
    Inc(RGB1.rgbAlpha, MulDiv(RGB2.rgbAlpha - RGB1.rgbAlpha, Shift, 100));
  end
  else if Shift < 0 then
  begin
    Inc(RGB2.rgbRed, MulDiv(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    Inc(RGB2.rgbGreen, MulDiv(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    Inc(RGB2.rgbBlue, MulDiv(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
    Inc(RGB2.rgbAlpha, MulDiv(RGB2.rgbAlpha - RGB1.rgbAlpha, Shift, 100));
  end;
  
  dRed   := RGB2.rgbRed - RGB1.rgbRed;
  dGreen := RGB2.rgbGreen - RGB1.rgbGreen;
  dBlue  := RGB2.rgbBlue - RGB1.rgbBlue;
  dAlpha := RGB2.rgbAlpha - RGB1.rgbAlpha;
  
  M := MulDiv(255, Rotation, 100);
  if M = 0 then
    for Index := 0 to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div 255;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div 255;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div 255;
        if FAlphaChannel then
          rgbAlpha := rgbBlue
        else
          rgbAlpha := RGB1.rgbAlpha + (Index * dAlpha) div 255;
      end
  else if M > 0 then
  begin
    M := 255 - M;
    for Index := 0 to M - 1 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div M;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div M;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div M;
        if FAlphaChannel then
          rgbAlpha := rgbBlue
        else
          rgbAlpha := RGB1.rgbAlpha + (Index * dAlpha) div M;
      end;
    for Index := M to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + ((255 - Index) * dRed) div (255 - M);
        rgbGreen := RGB1.rgbGreen + ((255 - Index) * dGreen) div (255 - M);
        rgbBlue := RGB1.rgbBlue + ((255 - Index) * dBlue) div (255 - M);
        if FAlphaChannel then
          rgbAlpha := rgbBlue
        else
          rgbAlpha := RGB1.rgbAlpha + ((255 - Index) * dAlpha) div (255 - M);;
      end;
  end
  else if M < 0 then
  begin
    M := -M;
    for Index := 0 to M do
      with Colors[Index] do
      begin
        rgbRed := RGB2.rgbRed - (Index * dRed) div M;
        rgbGreen := RGB2.rgbGreen - (Index * dGreen) div M;
        rgbBlue := RGB2.rgbBlue - (Index * dBlue) div M;
        if FAlphaChannel then
          rgbAlpha := rgbBlue
        else
          rgbAlpha := RGB2.rgbAlpha - (Index * dAlpha) div M;;
      end;
    for Index := M + 1 to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB2.rgbRed - ((255 - Index) * dRed) div (255 - M);
        rgbGreen := RGB2.rgbGreen - ((255 - Index) * dGreen) div (255 - M);
        rgbBlue := RGB2.rgbBlue - ((255 - Index) * dBlue) div (255 - M);
        if FAlphaChannel then
          rgbAlpha := rgbBlue
        else
          rgbAlpha := RGB2.rgbAlpha - ((255 - Index) * dAlpha) div (255 - M);
      end;
  end;
  
  Dirty := True;
  try
    if @PatternBuilder[Style] <> nil then
      PatternBuilder[Style](Colors, Pattern)
    else if (Style = gsPattern) and (FileExistsUTF8(PatternFile)) then
    begin
      Pattern.LoadFromFile(PatternFile);
      if AlphaChannel then
        ApplyBlueChannelToAlpha(Pattern);
    end
    else if Assigned(FOnCustom) then
      FOnCustom(Self, Colors, Pattern)
    else
    begin
      Pattern.SetSize(2, 2);
      //Pattern.Width := 2;
      //Pattern.Height := 2;
      Pattern.Canvas.Pixels[0, 0] := RGBColor1;
      Pattern.Canvas.Pixels[0, 1] := RGBColor2;
      Pattern.Canvas.Pixels[1, 0] := RGBColor2;
      Pattern.Canvas.Pixels[1, 1] := RGBColor1;
    end;
  finally
    Dirty := False;
  end;
  
  {if (Parent <> nil) and Parent.HandleAllocated then
  begin
    UpdatedRect := BoundsRect;
    InvalidateRect(Parent.Handle, @UpdatedRect, False);
    if csDesigning in ComponentState then Parent.Update;
  end
  else
    Invalidate; //}
end;

procedure TGRGradient.InvalidatePattern;
begin
  Update;
end;

function TGRGradient.IsColorBeginSaved: Boolean;
begin
  Result := not UseSysColors and (ColorBegin <> clWhite);
end;

function TGRGradient.IsColorEndSaved: Boolean;
begin
  Result := not UseSysColors and (ColorBegin <> clBtnFace);
end;

procedure TGRGradient.Loaded;
begin
  //inherited Loaded;
  Update;
end;

procedure TGRGradient.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY:
  integer);
begin
  if FEnabled and not Dirty and (FStyle <> gsNone) then
  begin
    if Tiled then
      DrawTileToBMP32(aDst, aR, Pattern)
    else
      Pattern.DrawTo(aDst, aR, Pattern.BoundsRect);
  end
  //else
    //ShowMessage('Dirty or S');
end;

procedure TGRGradient.SetAlphaBegin(const Value: Byte);
begin
  if FAlphaBegin <> Value then
  begin
    FAlphaBegin := Value;
    Update;
  end;
end;

procedure TGRGradient.SetAlphaChannel(const Value: Boolean);
begin
  if FAlphaChannel <> Value then
  begin
    FAlphaChannel := Value;
    if FAlphaChannel then
    begin
      Pattern.DrawMode := dmBlend;
    end
    else
      Pattern.DrawMode := dmOpaque;
    Update;
  end;
end;

procedure TGRGradient.SetAlphaEnd(const Value: Byte);
begin
  if FAlphaEnd <> Value then
  begin
    FAlphaEnd := Value;
    Update;
  end;
end;

procedure TGRGradient.SetColorBegin(Value: TColor);
begin
  if FColorBegin <> Value then
  begin
    FColorBegin := Value;
    FUseSysColors := False;
    Update;
  end;
end;

procedure TGRGradient.SetColorEnd(Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    FColorEnd := Value;
    FUseSysColors := False;
    Update;
  end;
end;

procedure TGRGradient.SetPatternFile(const Value: string);
begin
  if FPatternFile <> Value then
  begin
    FPatternFile := Value;
    if FStyle = gsPattern then Update;
  end;
end;

procedure TGRGradient.SetReverse(Value: Boolean);
begin
  if FReverse <> Value then
  begin
    FReverse := Value;
    Update;
  end;
end;

procedure TGRGradient.SetRotation(Value: TGradientRotation);
begin
  if Value < Low(TGradientRotation) then
    Value := Low(TGradientRotation)
  else if Value > High(TGradientRotation) then
    Value := High(TGradientRotation);
  
  if FRotation <> Value then
  begin
    FRotation := Value;
    Update;
  end;
end;

procedure TGRGradient.SetShift(Value: TGradientShift);
begin
  if Value < Low(TGradientShift) then
    Value := Low(TGradientShift)
  else if Value > High(TGradientShift) then
    Value := High(TGradientShift);
  
  if FShift <> Value then
  begin
    FShift := Value;
    Update;
  end;
end;

procedure TGRGradient.SetStyle(Value: TGradientStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Update;
  end;
end;

procedure TGRGradient.SetTiled(const Value: Boolean);
begin
  if FTiled <> Value then
  begin
    FTiled := Value;
    //Update;
    DoChanged;
  end;
end;

procedure TGRGradient.SetUseSysColors(Value: Boolean);
begin
  if FUseSysColors <> Value then
  begin
    FUseSysColors := Value;
    if FUseSysColors then
      UpdateSysColors;
  end;
end;

procedure TGRGradient.UpdateSysColors;
  
  {$IFDEF DELPHI3}
  const
    COLOR_GRADIENTACTIVECAPTION = 27;
  {$ENDIF}
  
begin
  BeginUpdate;
  try
    ColorBegin := GetSysColor(COLOR_ACTIVECAPTION);
    try
      ColorEnd := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
      FUseSysColors := True;
    except
      // This windows version doesn't support gradient colors...
      ColorEnd := ColorBegin;
      FUseSysColors := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TGRGradient.WMSettingChange;
  
  //(var Message: TMessage);
  
begin
  //inherited;
  if UseSysColors then
    UpdateSysColors;
end;

constructor TGRWallpaper.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FAlpha := DefaultWallpaperAlphaValue;
end;

destructor TGRWallpaper.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TGRWallpaper.Empty: Boolean;
begin
  Result := not Assigned(FPicture.Graphic) or FPicture.Graphic.Empty;
end;

procedure TGRWallpaper.iAssign(Source: TPersistent);
begin
  inherited iAssign(Source);
  if Source is TGRWallpaper then
    with Source as TGRWallpaper do
    begin
        Self.FAlpha := Alpha;
        Self.FFileName := FileName;
        Self.FStyle := Style;
        Self.FTransparent := Transparent;
        Self.Picture.Assign(Picture);
        Self.Update;
    end;
end;

procedure TGRWallpaper.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY:
  integer);
var
  bmp: TBitmap32;
  w, h: Integer;
begin
  if FEnabled and Assigned(FPicture.Graphic) and not FPicture.Graphic.Empty then
  begin
    bmp := TBitmap32.Create;
    try
      bmp.DrawMode := dmBlend;
      bmp.MasterAlpha := Alpha;
      //bmp.CombineMode := cmMerge;
      FPicture.Graphic.Transparent := FTransparent;
      bmp.Assign(FPicture);

      TLinearResampler.Create(bmp);
  
      w := aR.Right - aR.Left;
      h := aR.Bottom - aR.Top;
      case FStyle of
        wlpsCenter:
        begin
          bmp.DrawTo(aDst, aR.Left + (w - FPicture.Width) div 2,
            aR.Top + (h - FPicture.Height) div 2);
        end;
        wlpsTile: DrawTileToBMP32(aDst, aR, bmp);
        wlpsStretch: begin
          BMP.DrawTo(aDst, aR);
        end;
      end; //case
    finally
      bmp.Free;
    end;
  end;
end;

procedure TGRWallpaper.PictureChanged(Sender: TObject);
begin
  if Assigned(FPicture.Graphic) and not FPicture.Graphic.Empty then
    FEnabled := True
  else
    FEnabled := False
  ;
  Update;
end;

procedure TGRWallpaper.SetAlpha(const Value: Byte);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Update;
  end;
end;

procedure TGRWallpaper.SetFileName(const Value: string);
begin
  if (FFileName <> Value) and (Value <> '') then
  begin
    try
      Picture.LoadFromFile(Value);
    except
      Exit;
    end;
    FFileName := Value;
    Update;
  end;
end;

procedure TGRWallpaper.SetStyle(const Value: TWallpaperStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Update;
  end;
end;

procedure TGRWallpaper.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> value then
  begin
    FTransparent := Value;
    Update;
  end;
end;

constructor TGRImageBorder.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FPictureTop := TBItmap32.Create;
  FPictureTop.OnChange := PictureChanged;
  FPictureLeftTop := TBItmap32.Create;
  FPictureLeftTop.OnChange := PictureChanged;
  FPictureRightTop :=TBItmap32.Create;
  FPictureRightTop.OnChange := PictureChanged;
  FPictureLeft := TBItmap32.Create;
  FPictureLeft.OnChange := PictureChanged;
  FPictureRight := TBItmap32.Create;
  FPictureRight.OnChange := PictureChanged;
  FPictureLeftBottom := TBItmap32.Create;
  FPictureLeftBottom.OnChange := PictureChanged;
  FPictureBottom := TBItmap32.Create;
  FPictureBottom.OnChange := PictureChanged;
  FPictureRightBottom := TBItmap32.Create;
  FPictureRightBottom.OnChange := PictureChanged;
  
  FPictureMask := TBItmap32.Create;
  FPictureMask.OnChange := PictureChanged;
  
  FWidth := TRectProperty.Create(Self);
  FAlpha := DefaultImageBorderAlphaValue;
  FFrameSides := ALLFRAME_SIDES;
end;

destructor TGRImageBorder.Destroy;
begin
  FreeAndNil(FPictureTop);
  FreeAndNil(FPictureLeftTop);
  FreeAndNil(FPictureRightTop);
  FreeAndNil(FPictureLeft);
  FreeAndNil(FPictureRight);
  FreeAndNil(FPictureLeftBottom);
  FreeAndNil(FPictureRightBottom);
  FreeAndNil(FPictureBottom);
  FreeAndNil(FWidth);
  inherited Destroy;
end;

function TGRImageBorder.CalcClientRect(const aSrc: TRect): TRect;
begin
  ReCalcWidth;
  if sdLeft in FFrameSides then
    Result.Left := aSrc.Left + FWidth.Left
  else
    Result.Left := aSrc.Left;
  if sdRight in FFrameSides then
    Result.Right := aSrc.Right - FWidth.Right
  else
    Result.Right := aSrc.Right;
  if sdTop in FFrameSides then
    Result.Top := aSrc.Top + FWidth.Top
  else
    Result.Top := aSrc.Top;
  if sdBottom in FFrameSides then
    Result.Bottom := aSrc.Bottom - FWidth.Bottom
  else
    Result.Bottom := aSrc.Bottom;
end;

procedure TGRImageBorder.iAssign(Source: TPersistent);
begin
  inherited iAssign(Source);
  if Source is TGRImageBorder then
    with Source as TGRImageBorder do
    begin
        Self.FAlpha := Alpha;
        Self.FileNameTop := FileNameTop;
        Self.FileNameLeftTop := FileNameLeftTop;
        Self.FileNameRight := FileNameRight;
        Self.FileNameRightBottom := FileNameRightBottom;
        Self.FileNameBottom := FileNameBottom;
        Self.FileNameLeftBottom := FileNameLeftBottom;
        Self.FFrameSides := FrameSides;
        Self.FWidth.Assign(Width);
        //Self.FTransparent := Transparent;
        //Self.FPictureTop.Assign(PictureTop);
    end
end;

procedure TGRImageBorder.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX,
  aDstY: integer);
var
  bmp: TBitmap32;
  w, h: Integer;
  LR: TRect;
begin
  if not FEnabled or IsEmptyPicture(FPictureTop) then exit;
  bmp := TBitmap32.Create;
  try
    bmp.DrawMode := dmBlend;
    bmp.MasterAlpha := FAlpha;
    //Draw Top Border
    if sdTop in FFrameSides then
    begin
      //FPictureTop.Graphic.Transparent := True;
      bmp.Assign(FPictureTop);
      LR.Left := aR.Left;
      LR.Right := aR.Right;
      LR.Top := aR.Top;
      LR.Bottom := LR.Top + FWidth.Top;
      DrawTileToBMP32(aDst, LR, bmp);
    end;
  
  
    if sdLeft in FFrameSides then
    begin
      if not IsEmptyPicture(FPictureLeft) then
      begin
        //FPictureLeft.Graphic.Transparent := True;
        bmp.Assign(FPictureLeft);
      end
      else begin
        //bmp.Assign(FPictureTop);
        bmp.Rotate270;
      end;
      LR.Left := aR.Left;
      LR.Right := LR.Left + FWidth.Left;
      LR.Top := aR.Top;
      LR.Bottom := aR.Bottom;
      DrawTileToBMP32(aDst, LR, bmp);
    end;
  
    if sdBottom in FFrameSides then
    begin
      if not IsEmptyPicture(FPictureBottom) then
      begin
        //FPictureBottom.Graphic.Transparent := True;
        bmp.Assign(FPictureBottom);
      end
      else begin
        //bmp.Assign(FPictureTop);
        //bmp.Rotate180;
        bmp.Rotate270;
      end;
      LR.Left := aR.Left;
      LR.Right := aR.Right;
      LR.Top := aR.Bottom - FWidth.Bottom;
      LR.Bottom := aR.Bottom;
      DrawTileToBMP32(aDst, LR, bmp);
    end;
  
    if sdRight in FFrameSides then
    begin
      if not IsEmptyPicture(FPictureRight) then
      begin
        //FPictureRight.Graphic.Transparent := True;
        bmp.Assign(FPictureRight);
      end
      else if not IsEmptyPicture(FPictureLeft) then
      begin
        //bmp.Assign(FPictureLeft);
        //bmp.Rotate180;
        bmp.Rotate270;
      end
      else begin
        bmp.Assign(FPictureTop);
        bmp.Rotate90;
      end;
      LR.Left := aR.Right - FWidth.Right;
      LR.Right := aR.Right;
      LR.Top := aR.Top;
      LR.Bottom := aR.Bottom;
      DrawTileToBMP32(aDst, LR, bmp);
    end;
  
    if (FFrameSides = ALLFRAME_SIDES) and not IsEmptyPicture(FPictureLeftTop) then
    begin
      //draw left-top
      //FPictureLeftTop.Graphic.Transparent := True;
      bmp.Assign(FPictureLeftTop);
      bmp.DrawTo(aDst, aR.Left, aR.Top);
  
      //draw right-top
      if not IsEmptyPicture(FPictureRightTop) then
      begin
        //FPictureRightTop.Graphic.Transparent := True;
        bmp.Assign(FPictureRightTop);
      end
      else begin
        //bmp.Assign(FPictureLeftTop);
        bmp.Rotate90;
      end;
      bmp.DrawTo(aDst, aR.Right-bmp.width, aR.Top);
  
      if not IsEmptyPicture(FPictureRightBottom) then
      begin
        //FPictureRightBottom.Graphic.Transparent := True;
        bmp.Assign(FPictureRightBottom);
      end
      else begin
        //bmp.Assign(FPictureLeftTop);
        //bmp.Rotate180;
        bmp.Rotate90;
      end;
      bmp.DrawTo(aDst, aR.Right-bmp.width, aR.Bottom-bmp.Height);
  
      if not IsEmptyPicture(FPictureLeftBottom) then
      begin
        //FPictureLeftBottom.Graphic.Transparent := True;
        bmp.Assign(FPictureLeftBottom);
      end
      else begin
        bmp.Rotate90;
      end;
      bmp.DrawTo(aDst, aR.Left, aR.Bottom - bmp.Height);
  
    end;
  finally
    bmp.Free;
  end;
end;

procedure TGRImageBorder.PictureChanged(Sender: TObject);
begin
  ReCalcWidth;
  Update;
end;

procedure TGRImageBorder.ReCalcWidth;
begin
  if FWidthModified then exit; //the user modify the width
  
  if not IsEmptyPicture(PictureTop) then
  begin
    FWidth.Top := PictureTop.Height;
  end
  else
    FWidth.Top := 0;
  
  if not IsEmptyPicture(PictureLeft) then
  begin
    FWidth.Left := PictureLeft.Width;
  end
  else
    FWidth.Left := FWidth.Top;
  
  if not IsEmptyPicture(PictureRight) then
  begin
    FWidth.Right := PictureRight.Width;
  end
  else
    FWidth.Right := FWidth.Left;
  
  if not IsEmptyPicture(PictureBottom) then
  begin
    FWidth.Bottom := PictureBottom.Height;
  end
  else
    FWidth.Bottom := FWidth.Top;
  
  {if Sender is PictureTop then
  begin
    if not IsEmptyPicture(PictureTop) then
    begin
      FWidth.Top := PictureTop.Height;
    end;
  end
  else if Sender is PictureLeft then
    if not IsEmptyPicture(PictureLeft) then
    begin
      FWidth.Left := PictureLeft.Width;
    end;
  end
  else if Sender is PictureBottom then
    if not IsEmptyPicture(PictureBottom) then
    begin
      FWidth.Bottom := PictureBottom.Height;
    end;
  end
  else if Sender is PictureRight then
    if not IsEmptyPicture(PictureRight) then
    begin
      FWidth.Right := PictureLeft.Width;
    end;
  end
  ;
  //}
end;

procedure TGRImageBorder.SetAlpha(const Value: Byte);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameBottom(const Value: string);
begin
  if (FFileNameBottom <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureBottom, Value);
      except
        Exit;
      end;
    end
    else
      PictureBottom.Delete;
    FFileNameBottom := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameLeft(const Value: string);
begin
  if (FFileNameLeft <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureLeft, Value);
      except
        Exit;
      end;
    end
    else
      PictureLeft.Delete;
    FFileNameLeft := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameLeftBottom(const Value: string);
begin
  if (FFileNameLeftBottom <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureLeftBottom, Value);
      except
        Exit;
      end;
    end
    else
      PictureLeftBottom.Delete;
    FFileNameLeftBottom := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameLeftTop(const Value: string);
begin
  if (FFileNameLeftTop <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureLeftTop, Value);
      except
        Exit;
      end;
    end
    else
      PictureLeftTop.Delete;
    FFileNameLeftTop := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameMask(const Value: string);
begin
  if (FFileNameMask <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureMask, Value);
      except
        Exit;
      end;
    end
    else
      PictureMask.Delete;
    FFileNameMask := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameRight(const Value: string);
begin
  if (FFileNameRight <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureRight, Value);
      except
        Exit;
      end;
    end
    else
      PictureRight.Delete;
    FFileNameRight := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameRightBottom(const Value: string);
begin
  if (FFileNameRightBottom <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureRightBottom, Value);
      except
        Exit;
      end;
    end
    else
      PictureRightBottom.Delete;
    FFileNameRightBottom := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameRightTop(const Value: string);
begin
  if (FFileNameRightTop <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureRightTop, Value);
      except
        Exit;
      end;
    end
    else
      PictureRightTop.Delete;
    FFileNameRightTop := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFileNameTop(const Value: string);
begin
  if (FFileNameTop <> Value) then
  begin
    if Value <> '' then
    begin
      try
        LoadPicture(PictureTop, Value);
      except
        Exit;
      end;
    end
    else
      PictureTop.Delete;
    FFileNameTop := Value;
    Update;
  end;
end;

procedure TGRImageBorder.SetFrameSides(const Value: TFrameSides);
begin
  if FFrameSides <> Value then
  begin
  FFrameSides := Value;
  Update;
  end;
end;

constructor TGRFrame.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  
  //FWidth := TRectProperty.Create(Self);
  //FWidth.OnChanged := BorderChanged;
  FAlpha := DefaultImageBorderAlphaValue;
  FFrameSides := ALLFRAME_SIDES;
  
  FImageFrame := TGRImageBorder.Create(Self);
  FImageFrame.OnChanged := BorderChanged;
  
  FWidth := 1;
  
  FColor := clWindowFrame;
  FFrameStyle := fsFlat;
end;

destructor TGRFrame.Destroy;
begin
  //FreeAndNil(FWidth);
  FreeAndNil(FImageFrame);
  inherited Destroy;
end;

procedure TGRFrame.BorderChanged(Sender: TObject);
begin
  Update;
end;

function TGRFrame.CalcClientRect(const aSrc: TRect): TRect;
begin
  if FFrameStyle = fsImage then
    Result := FImageFrame.CalcClientRect(aSrc)
  else begin
    if sdLeft in FFrameSides then
      Result.Left := aSrc.Left + FWidth shl 1
    else
      Result.Left := aSrc.Left;
    if sdRight in FFrameSides then
      Result.Right := aSrc.Right - FWidth shl 1
    else
      Result.Right := aSrc.Right;
    if sdTop in FFrameSides then
      Result.Top := aSrc.Top + FWidth shl 1
    else
      Result.Top := aSrc.Top;
    if sdBottom in FFrameSides then
      Result.Bottom := aSrc.Bottom - FWidth shl 1
    else
      Result.Bottom := aSrc.Bottom;
  
    {if sdLeft in FFrameSides then
      Result.Left := aSrc.Left - FWidth.Left
    else
      Result.Left := aSrc.Left;
    if sdRight in FFrameSides then
      Result.Right := aSrc.Right - FWidth.Right
    else
      Result.Right := aSrc.Right;
    if sdTop in FFrameSides then
      Result.Top := aSrc.Top - FWidth.Top
    else
      Result.Top := aSrc.Top;
    if sdBottom in FFrameSides then
      Result.Bottom := aSrc.Bottom - FWidth.Bottom
    else
      Result.Bottom := aSrc.Bottom; //}
  end;
end;

procedure TGRFrame.iAssign(Source: TPersistent);
begin
  inherited iAssign(Source);
  if Source is TGRFrame then
    with Source as TGRFrame do
    begin
        Self.FAlpha := Alpha;
        Self.FEnabled := Enabled;
        Self.FWidth := Width;
        Self.FrameSides := FrameSides;
        Self.FColor := Color;
        Self.FFrameStyle := FrameStyle;
        Self.FImageFrame.Assign(ImageFrame);
        //Self.FTransparent := Transparent;
        //Self.FPictureTop.Assign(PictureTop);
    end;
end;

procedure TGRFrame.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY:
  integer);
var
  bmp: TBitmap32;
  w, h: Integer;
  LR: TRect;
begin
  if not FEnabled then exit;
  case FFrameStyle of
    fsImage: begin
      ImageFrame.FAlpha := Alpha;
      ImageFrame.FFrameSides := FFrameSides;
      ImageFrame.PaintTo(aSrc, aDst, aR, aDstX, aDstY);
    end
    else begin
      DrawColorBorderSidesWithWidth(aDst, aR, Color32(FColor),
        FFrameStyle, FFrameSides, FWidth, FAlpha);
    (*
      bmp := TBitmap32.Create;
      try
        w := aR.Right - aR.Left;
        h := aR.Bottom - aR.Top;
        bmp.SetSize(w, h);
        bmp.Clear(0);
        //DrawColorBorderSides(bmp: TBitmap32; Bounds: TRect; FaceColor: TColor; Style: TFrameStyle; Sides: TFrameSides): TRect;
  
        DrawColorBorderSidesWithWidth(bmp, bmp.BoundsRect, Color32(FColor),
          FFrameStyle, FFrameSides, FWidth);
        {$IFDEF GR32_1_8_Above}
        bmp.DrawMode := dmBlend;
        //bmp.DrawMode := dmTransparent;
        //bmp.OuterColor := 0;
        //bmp.CombineMode := cmMerge;
        {$ELSE}
        bmp.DrawMode := dmBlend;
        //bmp.CombineMode := cmMerge;
        {$ENDIF}
        bmp.MasterAlpha := Alpha;
        bmp.DrawTo(aDst, aR.Left, aR.Top);
      finally
        bmp.Free;
      end;
    *)
    end;
  end;
end;

procedure TGRFrame.SetAlpha(const Value: Byte);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Update;
  end;
end;

procedure TGRFrame.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Update;
  end;
end;

procedure TGRFrame.SetFrameSides(const Value: TFrameSides);
begin
  if FFrameSides <> Value then
  begin
  FFrameSides := Value;
  Update;
  end;
end;

procedure TGRFrame.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Update;
  end;
end;

procedure TGRFrame.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Update;
  end;
end;

constructor TGRBackground.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGradient := TGRGradient.Create(Self);
  FWallpaper := TGRWallpaper.Create(Self);
  FTexture := TGRWallpaper.Create(Self);
  FMask := TBitmap32.Create;
  FGradient.OnChanged := HookChanged;
  FWallpaper.OnChanged := HookChanged;
  FTexture.OnChanged := HookChanged;
  FMask.OnChange := HookChanged;
  FEnabled := True;
  FColor := clNone;
  

  TLinearResampler.Create(FMask);

end;

destructor TGRBackground.Destroy;
begin
  FGradient.Free;
  FWallpaper.Free;
  FTexture.Free;
  FreeAndNil(FMask);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TGRBackground.Empty: Boolean;
begin
  Result := (Gradient.Style = gsNone) and Wallpaper.Empty and Texture.Empty;
end;

procedure TGRBackground.HookChanged(Sender: TObject);
begin
  FBufferDirty := True;
  DoChanged;
end;

procedure TGRBackground.iAssign(Source: TPersistent);
begin
  inherited iAssign(Source);
  if Source is TGRBackground then
    with Source as TGRBackground do
    begin
        Self.FEnabled := Enabled;
        Self.Wallpaper.Assign(Wallpaper);
        Self.Texture.Assign(Texture);
        Self.Gradient.Assign(Gradient);
        Self.Buffered := Buffered;
        Self.FColor := Color;
    end;
end;

procedure TGRBackground.InternalPaintTo(aBitmap32: TBitmap32; R:TRect);
var
  bmpGradient, bmpTexture: TBitmap32;
  w, h: Integer;
  LColor32: TColor32Rec;
begin
  if FEnabled then
  begin
    w := R.Right - R.Left;
    h := R.Bottom - R.Top;
  
    //FGradient.PaintTo(aBitmap32, R);
  
    if not FWallpaper.Empty then
    begin
      FWallpaper.PaintTo(aBitmap32, R);
    end
    else begin
      if FColor <> clNone then
      begin
        LColor32.Color := Color32(FColor);
        LColor32.rgbAlpha := FWallpaper.Alpha;
        aBitmap32.FillRectTS(R, LColor32.Color);
      end;
    end;
  
    if FGradient.Style <> gsNone then
    begin
      //ShowMessage('Paint Gradinet');
      bmpGradient := TBitmap32.Create;
      try
        bmpGradient.SetSize(w, h);
        bmpGradient.DrawMode := dmBlend;
        //bmpGradient.CombineMode := cmMerge;
        //bmpGradient.Clear(clRed32);
        FGradient.PaintTo(bmpGradient, bmpGradient.BoundsRect);
        {if not FWallpaper.Empty and (FWallpaper.Alpha > 0) and not FGradient.AlphaChannel then
        begin
          bmpGradient.MasterAlpha := $FF - FWallpaper.Alpha;
        end; //}
        if not FTexture.Empty then
        begin
          bmpTexture := TBitmap32.Create;
          try
            bmpTexture.DrawMode := dmBlend;
            bmpTexture.CombineMode := cmMerge;
            bmpTexture.SetSize(w, h);
            FTexture.PaintTo(bmpTexture, bmpTexture.BoundsRect);
            if FGradient.AlphaChannel then
              BlueChannelToAlpha(bmpTexture, bmpGradient)
            else begin
              BlockTransfer(bmpTexture, 0, 0, bmpTexture.BoundsRect, bmpGradient,
                        bmpGradient.BoundsRect, dmBlend, nil);
            end;
            bmpTexture.MasterAlpha := FWallpaper.Alpha;
            BlockTransfer(aBitmap32, R.Left, R.Top, bmpTexture.BoundsRect, bmpTexture,
                        bmpTexture.BoundsRect, dmBlend, nil);
            // ------}
            {// Combine Texture A with B
            BlendTransfer(aBitmap32, R.Left, R.Top, bmpTexture.BoundsRect,
              bmpGradient, bmpGradient.BoundsRect,
              bmpTexture,  bmpTexture.BoundsRect,
              BlendRegEx, FWallpaper.Alpha);
            //This is needed because we may use MMX in the custom pixelcombiners
            EMMS; // ----}
          finally
            bmpTexture.Free;
          end;
        end
        else begin //mask it
          //FGradient.PaintTo(aBitmap32, R);
          //BlueChannelToAlpha(aBitmap32, bmpGradient);
          bmpGradient.MasterAlpha := FWallpaper.Alpha;
          bmpGradient.DrawTo(aBitmap32,R.Left,R.Top);
          //ColorToAlpha(bmpGradient, aBitmap32, bmpGradient.Bits[0]);
        end;
      finally
        bmpGradient.Free;
      end;
    end;
  
    if not FMask.Empty then
    begin
      bmpGradient := TBitmap32.Create;
      try
        bmpGradient.SetSize(w, h);
        FMask.DrawTo(bmpGradient, Rect(0,0,w,h));
        ColorToAlpha(bmpGradient, aBitmap32, clBlack32);
      finally
        bmpGradient.Free;
      end;
    end;
  end;
end;

procedure TGRBackground.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY:
  integer);
var
  w, h: Integer;
begin
  if not Enabled then Exit;
  
  w := aR.Right - aR.Left;
  h := aR.Bottom - aR.Top;
  
  if FBuffered then
  begin
    if FBufferDirty or (FBuffer.Width <> w) or (FBuffer.Height <> h) then
    begin
      FBuffer.SetSize(w, h);
      FBuffer.Clear;
      InternalPaintTo(FBuffer, Rect(0,0,w,h));
      FBufferDirty := False;
    end;
    FBuffer.DrawTo(aDst, aR);
  end
  else
    InternalPaintTo(aDst, aR);
end;

procedure TGRBackground.SetBuffered(const Value: Boolean);
begin
  if FBuffered <> Value then
  begin
    if Value then
    begin
      FBufferDirty := True;
      FBuffer := TBitmap32Ex.Create;
      FBuffer.DrawMode := dmBlend;
      FBuffer.CombineMode := cmMerge;
    end
    else begin
      FreeAndNil(FBuffer);
    end;
    FBuffered := Value;
  end;
end;

procedure TGRBackground.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChanged;
  end;
end;

procedure TGRBackground.SetGradient(const Value: TGRGradient);
begin
  if FGradient <> Value then
  begin
    FGradient.Assign(Value);
  end;
end;

procedure TGRBackground.SetMask(Value: TBitmap32);
begin
  if FMask <> Value then
    FMask.Assign(Value);
end;

procedure TGRBackground.SetTexture(const Value: TGRWallpaper);
begin
  if FTexture <> Value then
  begin
    FTexture.Assign(Value);
  end;
end;

procedure TGRBackground.SetWallpaper(const Value: TGRWallpaper);
begin
  if FWallpaper <> Value then
  begin
    FWallpaper.Assign(Value);
  end;
end;

constructor TFont32.Create;
begin
  inherited Create;
  FOpacity := 255;
  FBackground := TGRBackground.Create(Self);
  FShadow := TShadowEffect.Create(Self);
  FBackground.Enabled := False;
  FBackground.OnChanged := DoPropertyChanged;
  FShadow.OnChanged := DoPropertyChanged;
end;

destructor TFont32.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FShadow);
  inherited Destroy;
end;

procedure TFont32.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if (Source is TFont32) and (Source <> Self) then
  begin
    //Self.BeginUpdate;
    with Source as TFont32 do
    begin
      Self.FOpacity := Opacity;
      Self.FQuality := Quality;
      Self.FOutline := Outline;
      Self.FBackground.Assign(Background);
      Self.FShadow.Assign(Shadow);
    end;
  end;
end;

procedure TFont32.DoPropertyChanged(Sender: TObject);
begin
  Changed;
end;

procedure TFont32.DrawText(Dst: TBitmap32; Text: string; var aRect: TRect;
  aFormat: LongWord);
var
  vCharSize: TSize;
  I: Integer;
  CurrentX, CurrentY: Integer;
  aStrs: TStringList;
  MaxRight: Integer;
  
  function GetFirstColPos(const X: integer; const aTextSize: TSize):integer;
  begin
    if (DT_CENTER and aFormat) = DT_CENTER then
    begin
      Result := X + (aRect.Right- aRect.Left) div 2 - aTextSize.cx div 2;
    end
    else if (DT_RIGHT and aFormat) = DT_RIGHT then
    begin
      Result :=  X + (aRect.Right-aRect.Left) - aTextSize.cx;
      {$ifdef debug}
      //SendDebug(Format('Right=%d', [Result]));
      {$endif}
    end
    else
      Result := X;
  end;
  
  //if the string is in the rect then return -1.
  function StringInRect(var s: string; var vTextSize: TSize; const X: Integer): Integer;
  
  begin
      Result := -1;
  
      while aRect.Right < X + vTextSize.cx do
      begin
        //remove the beyond text.
        Result := Length(s);
        if ByteType(s, Result) = mbTrailByte then
          while (Result >0) and (ByteType(s, Result) <> mbLeadByte) do
            Dec(Result);
        s := Copy(s, 1, Result-1);
        vTextSize := TextExtent(Dst, s);
      end;
      //Show the ELLIPSIS ...
      if (Result > 0) and ((DT_END_ELLIPSIS and aFormat) = DT_END_ELLIPSIS) then
      begin
        Result := Length(s);
        if ByteType(s, Result) = mbTrailByte then
          while (Result >0) and (ByteType(s, Result) <> mbLeadByte) do
            Dec(Result);
        s := Copy(s, 1, Result-1);
  
        s := s + '΅­';
  
        vTextSize := TextExtent(Dst, s);
      end;
  end;
  
  procedure DrawLine(var X: Integer; Y: Integer; Text: string);
  var
    vTextSize: TSize;
    ColPos: integer;
    s: string;
    i: integer;
  begin
    if (fsUnderline in Style) or (DT_NOPREFIX and aFormat = DT_NOPREFIX) then
    begin
      vTextSize := TextExtent(Dst, Text);
      X := GetFirstColPos(X, vTextSize);
    end
    else begin
      if (DT_EXPANDTABS and aFormat) = DT_EXPANDTABS then
      begin
        Text := StringReplace(Text, #9, '  ', [rfReplaceAll]);
      end;
      s := StringReplace(Text, '&&', #$FF, [rfReplaceAll]);
      s := StringReplace(s, '&', '', [rfReplaceAll]);
      s := StringReplace(s, #$FF, '&', [rfReplaceAll]);
      vTextSize := TextExtent(Dst, s);
      X := GetFirstColPos(X, vTextSize);
      ColPos := AnsiPos('&', Text);
      i := -1;
      while (ColPos > 0) and (ColPos + 1 <= Length(Text)) do
      begin
        if ColPos > 1 then
          s := Copy(Text, 1, ColPos-1)
        else
          s := '';
        if s <> '' then
        begin
          vTextSize := TextExtent(Dst, s);
          if (DT_CALCRECT and aFormat) = 0 then
          begin
            if (DT_NOCLIP and aFormat) = 0 then
              i := StringInRect(s, vTextSize, X);
  
            //if X + vTextSize.cx < aRect.Right then
              RenderText(Dst, X, Y, s);
              if I > 0 then Exit; //this is > aRect.Right
            //else
              //Exit;
          end;
          X := X + vTextSize.cx;
        end;
        Delete(Text, 1, ColPos);
          if Text[1] in LeadBytes then
          begin
            //s := Copy(Text, 1, CharLength(Text, 1));    // SOS 9999
          end
          else
            s := Text[1];
          if s <> '&' then
            Dst.Font.Style := Dst.Font.Style + [fsUnderline];
          vTextSize := TextExtent(Dst, s);
          if DT_CALCRECT and aFormat = 0 then
            RenderText(Dst, X, Y, s);
          if s <> '&' then
            Dst.Font.Style := Dst.Font.Style - [fsUnderline];
          X := X + vTextSize.cx;
          Delete(Text, 1, length(s));
        ColPos := AnsiPos('&', Text);
      end; //while
    end; //
  
    if Text <> '' then
    begin
      vTextSize := TextExtent(Dst, Text);
      if (DT_CALCRECT and aFormat) = 0 then
      begin
        //Only draw the text in the Rect.
        s := Text;
        if (DT_NOCLIP and aFormat) = 0 then
          i := StringInRect(s, vTextSize, X);
  

        RenderText(Dst, X, Y, s);
      end;
      X := X + vTextSize.cx;

    end;
  end;
  
begin
  vCharSize := TextExtent(Dst, 'A');
  MaxRight := 0;
  if DT_WORDBREAK and aFormat = DT_WORDBREAK then
  begin
    Text := WrapTextEx(Text, (aRect.Right - aRect.Left) div vCharSize.cx);

  end;
  
  if DT_SINGLELINE and aFormat = DT_SINGLELINE then
  begin
    vCharSize := TextExtent(Dst, Text);
    if DT_VCENTER and aFormat = DT_VCENTER then
    begin
      CurrentY := aRect.Top + (aRect.Bottom-aRect.Top) div 2 - vCharSize.cy div 2;
    end
    else if DT_BOTTOM and aFormat = DT_BOTTOM then
    begin
      CurrentY := aRect.Top + (aRect.Bottom-aRect.Top) div 2 - vCharSize.cy div 2;
    end
    else
      CurrentY := aRect.Top;
    //LTop := CurrentY;
    //LLeft := aRect.Left;
    CurrentX := aRect.Left;
    DrawLine(CurrentX, CurrentY, Text);
    CurrentY := CurrentY + vCharSize.cy;
    MaxRight := CurrentX;
  end
  else begin
    aStrs := TStringList.Create;
    try
      aStrs.Text := Text;
      CurrentY := aRect.Top;
      for i := 0 to aStrs.Count -1 do
      begin
        CurrentX := aRect.Left;
        {$ifdef debug}
        //SendDebug(Format(aStrs[i]+'.X=%d;Y=%d', [CurrentX, CurrentY]));
        {$endif}
        DrawLine(CurrentX, CurrentY, aStrs[i]);
        if CurrentX> MaxRight then
          MaxRight := CurrentX;
        CurrentY := CurrentY + vCharSize.cy + LineSpacing;
        if (DT_CALCRECT and aFormat = 0) and (CurrentY > aRect.Bottom) then break;
      end;
    finally
      aStrs.Free;
    end;
  end;

  aRect.Right := MaxRight;
  aRect.Bottom := CurrentY;
end;

procedure TFont32.RenderText(Dst: TBitmap32; X, Y: Integer; const Text: String);
var
  vColor: TColor32Rec;
  vTexture, vTextBMP: TBitmap32;
  Sz: Tsize;
  PaddedText: string;
begin
  //with Background do
    //if Background.Enabled and not Background.Empty then
    //begin
      //TODO: Draw Texture Text
  
      vTexture := nil;
      vTextBMP := nil;
      try
  
        vTexture := TBitmap32.Create;
        vTextBMP := TBitmap32.Create;
        PaddedText := Text + ' ';
        //if Assigned(TBitmap32Ex(vTextBMP).Font.Background) then
        vTextBMP.Font := Dst.Font;
        //vTextBMP.Font := Self;
        //if AALevel > 0 then
          //vTextBMP.Font.Size := vTextBMP.Font.Size shl AALevel;
        Sz := vTextBMP.TextExtent(PaddedText);
        //if AALevel > 0 then //restore
          //vTextBMP.Font.Size := vTextBMP.Font.Size shr AALevel;
        {if Shadow.Enabled then
        begin
          Inc(Sz.cx,Abs(Shadow.OffsetX));
          Inc(Sz.cy,Abs(Shadow.OffsetY));
        end; //}
        vTextBMP.SetSize(Sz.cx, Sz.cy);
        vTexture.SetSize(Sz.cx, Sz.cy);
        vTexture.DrawMode := dmBlend;
        vTexture.CombineMode := cmMerge;
        if Background.Enabled and not Background.Empty then
          FBackground.PaintTo(vTexture, vTexture.BoundsRect)
        else begin
          vColor.Color := Color32(Self.Color);
          vColor.rgbAlpha := FOpacity;
          vTexture.Clear(vColor.Color);
        end;
        //vTextBMP.DrawMode := dmBlend;
        //vTextBMP.CombineMode := cmMerge;
        vTextBMP.Clear(clBlack32);
        vColor.Color := clWhite32;
        //vColor.rgbAlpha := $FF;
        if Quality = fqNone then
        begin
          vTextBMP.Font.Color := clWhite;
          vTextBMP.TextOut(0, 0, Text);
        end
        else
          vTextBMP.RenderText(0, 0, Text, Integer(Quality), vColor.Color);
        if Outline then
        begin
          ConvolveI(LaplaceFilter3x3, vTextBMP);
          //if Quality > fqNormal then
            //ConvolveI(GaussianBlureFilter3x3, vTextBMP);
        end;
        BlueChannelToAlpha(vTexture, vTextBMP);
        //ApplyBlueChannelToAlpha(vTextBMP);
        //ApplyTransparentColor(vTextBMP, clWhite32);
        //vTextBMP.DrawTo(vTexture);
        if Shadow.Enabled then
        begin
          Shadow.PaintTo(vTexture, Dst, vTexture.BoundsRect, X, Y);
          //Shadow.GenerateShadow(vTexture, vShadowBMP, vTexture.BoundsRect);
          //vShadowBMP.DrawTo(Dst, X+Shadow.OffsetX, Y+Shadow.OffsetY);
        end;
        //ApplyTransparentColor(vTexture, clBlack32);
        vTexture.MasterAlpha := FOpacity;
        vTexture.DrawTo(Dst, X, Y);
      finally
        FreeAndNil(vTexture);
        FreeAndNil(vTextBMP);
      end;
    //end
    {else begin
      Dst.Font := Self;
      vColor.Color := Color32(Self.Color);
      vColor.rgbAlpha := FOpacity;
      Dst.RenderText(X, Y, Text, Integer(Quality), vColor.Color);
    end; //}
end;

procedure TFont32.RenderTextW(Dst: TBitmap32; X, Y: Integer; const Text:
  WideString);
var
  vColor: TColor32Rec;
  vTexture, vTextBMP: TBitmap32;
  Sz: TSize;
  PaddedText: Widestring;
begin
  //with Background do
    //if Background.Enabled and not Background.Empty then
    begin
      //TODO: Draw Texture Text
      vTexture := nil;
      vTextBMP := nil;
      try
        vTexture := TBitmap32.Create;
        vTextBMP := TBitmap32.Create;
        PaddedText := Text + ' ';
        //if Assigned(TBitmap32Ex(vTextBMP).Font.Background) then
        vTextBMP.Font := Dst.Font;
        //if AALevel > 0 then
          //vTextBMP.Font.Size := vTextBMP.Font.Size shl AALevel;
        Sz := vTextBMP.TextExtentW(PaddedText);
        //if AALevel > 0 then //restore
          //vTextBMP.Font.Size := vTextBMP.Font.Size shr AALevel;
        vTextBMP.SetSize(Sz.cx, Sz.cy);
        vTexture.SetSize(Sz.cx, Sz.cy);
        vTexture.DrawMode := dmBlend;
        vTexture.CombineMode := cmMerge;
        if Background.Enabled and not Background.Empty then
          FBackground.PaintTo(vTexture, vTexture.BoundsRect)
        else begin
          vColor.Color := Color32(Self.Color);
          vColor.rgbAlpha := FOpacity;
          vTexture.Clear(vColor.Color);
        end;
        //vTextBMP.DrawMode := dmBlend;
        //vTextBMP.CombineMode := cmMerge;
        vTextBMP.Clear(clBlack32);
        vColor.Color := clWhite32;
        //vColor.rgbAlpha := $FF;
        if Quality = fqNone then
        begin
          vTextBMP.Font.Color := clWhite;
          vTextBMP.TextOutW(0, 0, Text);
        end
        else
          vTextBMP.RenderTextW(0, 0, Text, Integer(Quality), vColor.Color);
        if Outline then
        begin
          ConvolveI(LaplaceFilter3x3, vTextBMP);
          //if Quality > fqNormal then
            //ConvolveI(GaussianBlureFilter3x3, vTextBMP);
        end;
        BlueChannelToAlpha(vTexture, vTextBMP);
        if Shadow.Enabled then
        begin
          Shadow.PaintTo(vTexture, Dst, vTexture.BoundsRect, X, Y);
        end;
        //ApplyBlueChannelToAlpha(vTextBMP);
        //ApplyTransparentColor(vTextBMP, clWhite32);
        //vTextBMP.DrawTo(vTexture);
        vTexture.DrawTo(Dst, X, Y);
        //ApplyTransparentColor(vTexture, clBlack32);
      finally
        FreeAndNil(vTexture);
        FreeAndNil(vTextBMP);
      end;
    end
    {else begin
      Dst.Font := Self;
      vColor.Color := Color32(Self.Color);
      vColor.rgbAlpha := FOpacity;
      Dst.RenderTextW(X, Y, Text, Integer(Quality), vColor.Color);
    end; //}
end;

procedure TFont32.SetBackground(const Value: TGRBackground);
begin
  if FBackground <> Value then
    FBackground.Assign(Value);
end;

procedure TFont32.SetCharSpacing(const Value: Integer);
begin
  if FCharSpacing <> Value then
  begin
    FCharSpacing := Value;
    Changed;
  end;
end;

procedure TFont32.SetLineSpacing(const Value: Integer);
begin
  if FLineSpacing <> Value then
  begin
    FLineSpacing := Value;
    Changed;
  end;
end;

procedure TFont32.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TFont32.SetOutline(const Value: Boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    Changed;
  end;
end;

procedure TFont32.SetQuality(const Value: TFontQuality);
begin
  if FQuality <> Value then
  begin
    FQuality := Value;
    Changed;
  end;
end;

procedure TFont32.SetShadow(Value: TShadowEffect);
begin
  if FShadow <> Value then
    FShadow.Assign(Value);
end;

function TFont32.TextExtent(Dst: TBitmap32; const Text: string): TSize;
begin
  //Dst.Font := Self;
  Result := Dst.TextExtent(Text);
end;

function TFont32.TextExtentW(Dst: TBitmap32; const Text: Widestring): TSize;
begin
  //Dst.Font := Self;
  Result := Dst.TextExtentW(Text);
end;

constructor TGRStyle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFrame := TGRFrame.Create(Self);
  FFrame.OnChanged := DoStyleChanged;
  FFont := TFont32.Create;
  FFont.OnChange := DoStyleChanged;
  
  FBackground := TGRBackground.Create(Self);
  FBackground.OnChanged := DoStyleChanged;
end;

destructor TGRStyle.Destroy;
begin
  FreeAndNil(FFrame);
  FreeAndNil(FFont);
  FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TGRStyle.DoStyleChanged(Sender: TObject);
begin
  Update;
end;

procedure TGRStyle.iAssign(Source: TPersistent);
begin
  inherited iAssign(Source);
  if (Source is TGRStyle) and (Source <> Self) then
  begin
    with Source as TGRStyle do
    begin
      Self.Frame.Assign(Frame);
      Self.Font.Assign(Font);
      Self.Color := Color;
      Self.Background.Assign(Background);
    end;
  end;
end;

procedure TGRStyle.SetBackground(const Value: TGRBackground);
begin
  if FBackground <> Value then
    FBackground.Assign(Value);
end;

procedure TGRStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Update;
  end;
end;

procedure TGRStyle.SetFont(const Value: TFont32);
begin
  if FFont <> Value then
    FFont.Assign(Value);
end;

procedure TGRStyle.SetFrame(const Value: TGRFrame);
begin
  if FFrame <> Value then
    FFrame.Assign(Value);
end;


initialization

    StretchToDCTransparentFunc := @_StretchToDCTransparent;
end.
