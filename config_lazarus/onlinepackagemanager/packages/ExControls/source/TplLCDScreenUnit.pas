
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLCDScreenUnit;

interface

uses
  Classes, Graphics, Controls, Dialogs, ExtCtrls, Math, SysUtils,
  LCLtype, LCLIntf, IntfGraphics,
  Forms;

const
  MaxSizeCodeInstructionArray = 255; //Removing dynamic Arrays...

type
  TplLCDAnimator = class;

  TCodeInstruction = record
    word: string[25];
    Param: integer;
  end;

  CodeInstructionArrayBuilder = array[1..MaxSizeCodeInstructionArray] of TCodeInstruction; //Removing dynamic Arrays...

  TEndCodeEvent = procedure(Sender: TplLCDAnimator) of object;

  TResetMode = (rmDisplay, rmCode, rmDisplayAndCode);

  TLineExecutedEvent = procedure(Sender: TplLCDAnimator; LineNumber: word) of object;

  TPixelSize = (pixCustom, pix1x1, pix2x2, pix3x3, pix4x4, pix5x5, pix6x6,
    pix7x7, pix8x8, pix9x9, pix10x10, pix11x11, pix12x12, pix13x13,
    pix14x14, pix15x15, pix16x16);

  TPixelShape = (psSquare, psRound);

  TLCDBorder = (bsLowered, bsNone, bsRaised);

  TSpecialEffect = (spBlink, spBold, spInverse, spItalic, spUnderline, spStrike);

  TSpecialEffects = set of TSpecialEffect;

  TOneChar = record
    TheChar: char;
    SpEff: byte;
  end;

  TAnimMode = (amDynamic, amStatic);

  TDisplayMode = (dmText, dmBitmap, dmBoth);

  TBitmapCopyMode = (cmNotTransparent, cmTransparent);


  TplLCDAnimator = class(TComponent)
  private
    FCode: TStrings;
    FCodeErrorFound: boolean;
    FCurrentLine: smallint;
    FOnEndCode: TEndCodeEvent;
    FOnLineExecuted: TLineExecutedEvent;
    procedure SetCurrentLine(Value: smallint);
  protected
    procedure SetCode(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Code: TStrings read FCode write SetCode;
    property CodeErrorFound: boolean read FCodeErrorFound write FCodeErrorFound;
    property CurrentLine: smallint read FCurrentLine write SetCurrentLine;
    property OnEndCode: TEndCodeEvent read FOnEndCode write FOnEndCode;
    property OnLineExecuted: TLineExecutedEvent read FOnLineExecuted write FOnLineExecuted;
  end;


  TplLCDScreen = class(TGraphicControl)
  private
    P: ^CodeInstructionArrayBuilder;      //Removing dynamic Arrays...
    Display: array [0..255, 0..255] of TOneChar; { Internal image of the screen        }
    PixHRef, PixVRef: integer;            { Internal offset in Pixel unit used for animation routines }
    FNbXPixels, FNbYPixels: word;         { Internal size in dots of the component    }
    FLinesImage: TBitmap;                 { Internal black&white image of Flines      }
    FWidth, FHeight: integer;             { Internal height and width of the control  }
    FTimer: TTimer;                       { Internal Timer used                       }
    FTrueOnColor: TColor;                { Internal color for OnPixels               }
    FTrueOffColor: TColor;                { Internal color for OffPixels              }
    FBlinkingStatus: boolean;             { Internal Blinking On/Off flag             }
    FAnimationDelay: cardinal;            { In milliseconds; the smaller, the faster  }
    FAnimationEnabled: boolean;           { Enable or disable the animation           }
    FAnimationRepeating: boolean;         { What to do when all code lines had been processed }
    FBitmap: TBitmap;                     { The component's bitmap                    }
    FBitmapCopyMode: TBitmapCopyMode;     { Set bitmap transparent or not             }
    FBitmapAnimMode: TAnimMode;           { Bitmap is static or dynamic               }
    FBitmapXOffset: shortint;             { Value in Pixel for bitmap horz correction }
    FBitmapYOffset: shortint;             { Value in Pixel for bitmap vertical correction }
    FBorderSpace: shortint;               { Distance from the LCD border              }
    FBorderStyle: TLCDBorder;             { Border around the the component           }
    FColor: TColor;                       { LCD color                                 }
    FDisplayMode: TDisplayMode;           { Text only, Bitmap only or both displayed  }
    FFont: TFont;                         { The Font used in TplLCDScreen               }
    FIntensity: shortint;                 { [-128..127] percentage of ligth           }
    FLCDAnimator: TplLCDAnimator;           { The TplLCDAnimator associated with          }
    FLines: TStringList;                  { The component's multilines text           }
    FLinesAnimMode: TAnimMode;            { Lines are static or dynamic               }
    FLinesXOffset: shortint;              { Value in Pixel for lines horz  correction }
    FLinesYOffset: shortint;              { Value in Pixel for lines vertical correction }
    FPixelHeight: shortint;               { Pixel height                              }
    FPixelOffColor: TColor;               { LCD pixel OFF color                       }
    FPixelSize: TPixelSize;               { Size of a LCD pixel (in screen pixels)    }
    FPixelShape: TPixelShape;             { Shape of a LCD pixel                      }
    FPixelSpacing: shortint;              { Space between each pixel in the matrix    }
    FPixelWidth: shortint;                { Pixel widht                               }
    FSpecialEffects: TSpecialEffects;     { Enable or disable special effects         }
    procedure SetAnimationEnabled(Value: boolean);
    procedure SetAnimationRepeating(Value: boolean);
    procedure SetAnimationDelay(Value: cardinal);
    procedure SetBitmap(Value: TBitmap);
    procedure SetBitmapAnimMode(Value: TAnimMode);
    procedure SetBorderSpace(Value: shortint);
    procedure SetBorderStyle(Value: TLCDBorder);
    procedure SetBitmapCopyMode(Value: TBitmapCopyMode);
    procedure SetBitmapXOffset(Value: shortint);
    procedure SetBitmapYOffset(Value: shortint);
    procedure SetColor(Value: TColor); override;
    procedure SetDisplayMode(Value: TDisplayMode);
    procedure SetFont(Value: TFont);
    procedure SetIntensity(Value: shortint);
    procedure SeTplLCDAnimator(Value: TplLCDAnimator);
    procedure SetLinesAnimMode(Value: TAnimMode);
    procedure SetLinesXOffset(Value: shortint);
    procedure SetLinesYOffset(Value: shortint);
    procedure SetPixelSize(Value: TPixelSize);
    procedure SetPixelShape(Value: TPixelShape);
    procedure SetPixelSpacing(Value: shortint);
    procedure SetPixelOffColor(Value: TColor);
    procedure SetPixelWidth(Value: shortint);
    procedure SetPixelHeight(Value: shortint);
    procedure SetSpecialEffects(Value: TSpecialEffects);

    procedure DrawDisplayCharacters(BitMap: TBitmap);
    procedure FontOnChange(Sender: TObject);
    procedure SetCorrectSize;
    procedure UpdateFLinesImage;
    procedure UpdateTrueColors;

  protected
    procedure HorzScroll(Value: shortint);
    procedure LinesOnChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetLines(Value: TStringList);
    procedure TimerOnTimer(Sender: TObject);
    procedure VertScroll(Value: shortint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset(Value: TResetMode);
    property Canvas;

  published
    property AnimationDelay: cardinal read FAnimationDelay write SetAnimationDelay;
    property AnimationEnabled: boolean read FAnimationEnabled write SetAnimationEnabled;
    property AnimationRepeating: boolean read FAnimationRepeating write SetAnimationRepeating;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapCopyMode: TBitmapCopyMode read FBitmapCopyMode write SetBitmapCopyMode;
    property BitmapAnimMode: TAnimMode read FBitmapAnimMode write SetBitmapAnimMode;
    property BitmapXOffset: shortint read FBitmapXOffset write SetBitmapXOffset;
    property BitmapYOffset: shortint read FBitmapYOffset write SetBitmapYOffset;
    property BorderSpace: shortint read FBorderSpace write SetBorderSpace;
    property BorderStyle: TLCDBorder read FBorderStyle write SetBorderStyle;
    property Color: TColor read FColor write SetColor;
    property DisplayMode: TDisplayMode read FDisplayMode write SetDisplayMode;
    property Enabled;
    property Font: TFont read FFont write SetFont;
    property Intensity: shortint read FIntensity write SetIntensity;
    property LCDAnimator: TplLCDAnimator read FLCDAnimator write SeTplLCDAnimator;
    property Lines: TStringList read FLines write SetLines;
    property LinesAnimMode: TAnimMode read FLinesAnimMode write SetLinesAnimMode;
    property LinesXOffset: shortint read FLinesXOffset write SetLinesXOffset;
    property LinesYOffset: shortint read FLinesYOffset write SetLinesYOffset;
    property PixelHeight: shortint read FPixelHeight write SetPixelHeight;
    property PixelOff: TColor read FPixelOffColor write SetPixelOffColor;
    property PixelShape: TPixelShape read FPixelShape write SetPixelShape;
    property PixelSize: TPixelSize read FPixelSize write SetPixelSize;
    property PixelSpacing: shortint read FPixelSpacing write SetPixelSpacing;
    property PixelWidth: shortint read FPixelWidth write SetPixelWidth;
    property ShowHint;
    property SpecialEffects: TSpecialEffects read FSpecialEffects write SetSpecialEffects;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

const
  startinverse_tag = '<inv>';
  stopinverse_tag = '</inv>';
  startblinking_tag = '<bl>';
  stopblinking_tag = '</bl>';
  startunderline_tag = '<u>';
  stopunderline_tag = '</u>';
  startstrike_tag = '<s>';
  stopstrike_tag = '</s>';
  startbold_tag = '<b>';
  stopbold_tag = '</b>';
  startitalic_tag = '<i>';
  stopitalic_tag = '</i>';

  LCDTag: array[0..11] of string = (startinverse_tag, stopinverse_tag,
    startblinking_tag, stopblinking_tag,
    startunderline_tag, stopunderline_tag,
    startstrike_tag, stopstrike_tag,
    startbold_tag, stopbold_tag,
    startitalic_tag, stopitalic_tag);

function CountLCDTag(str: string): byte;
function NbOfThings(tempcode: string; Thing: char): integer;
function RemoveLCDTag(str: string): string;


implementation


function NbOfThings(tempcode: string; Thing: char): integer;
var
  i: integer;
begin
  i := 0;
  while Pos(Thing, tempcode) <> 0 do
  begin
    try
      Delete(tempcode, 1, Pos(Thing, tempcode));
    except;
    end;
    Inc(i);
  end;
  NbOfThings := i;
end;


function CountLCDTag(str: string): byte;
var
  i, col, nbtag: byte;
begin
  nbtag := 0;
  if Length(str) <> 0 then
    for i := 0 to 11 do
      for col := 0 to Length(str) - 3 do
        if Copy(str, col, Length(LCDTag[i])) = LCDTag[i] then
          Inc(nbtag);

  CountLCDTag := nbtag;
end;


function RemoveLCDTag(str: string): string;
var
  i: byte;
begin
  if Length(str) <> 0 then
    for i := 0 to 11 do
      str := StringReplace(str, LCDTag[i], '', [rfReplaceAll, rfIgnoreCase]);

  RemoveLCDTag := str;
end;


constructor TplLCDScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque] - [csAcceptsControls, csSetCaption];

  FLines := TStringList.Create;

  FFont := TFont.Create;

  FLinesImage := TBitmap.Create;
  FLinesImage.PixelFormat := pf1bit;

  FPixelSize := pix2x2;
  FPixelSpacing := 1;
  FBorderSpace := 3;
  Width := 145;
  Height := 65;
  FBorderStyle := bsLowered;
  FColor := clInactiveBorder;
  FPixelOffColor := $00AAAAAA;
  FIntensity := 127;
  UpdateTrueColors;
  PixVRef := 0;
  PixHRef := 0;
  FLinesXOffset := 0;
  FLinesYOffset := 0;
  FBitmapXOffset := 0;
  FBitmapYOffset := 0;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 250;
  FTimer.OnTimer := @TimerOnTimer;
  FTimer.Enabled := False;

  FAnimationDelay := 251;
  FAnimationEnabled := False;
  FAnimationRepeating := False;

  FLines.OnChange := @LinesOnChange;
  FFont.OnChange := @FontOnChange;

  FBitmap := TBitmap.Create;
  FDisplayMode := dmText;

  SetCorrectSize;
end;


destructor TplLCDScreen.Destroy;
begin
  FBitmap.Destroy;
  FLinesImage.Destroy;
  FTimer.Destroy;
  FLines.Destroy;
  FFont.Destroy;
  inherited Destroy;
end;

procedure TplLCDScreen.SetCorrectSize;
begin
  if PixelSize <> pixCustom then
  begin  { Predefined width selected - make square pixels. }
    FPixelWidth := Ord(FPixelSize);
    FPixelHeight := FPixelWidth;
  end;

  FNbXPixels := Trunc((Width - 2 * FBorderSpace + FPixelSpacing) / (FPixelWidth + FPixelSpacing));
  FNbYPixels := Trunc((Height - 2 * FBorderSpace + FPixelSpacing) / (FPixelHeight + FPixelSpacing));

  FWidth := 2 * FBorderSpace + FNbXPixels * FPixelWidth + (FNbXPixels - 1) * FPixelSpacing;
  FHeight := 2 * FBorderSpace + FNbYPixels * FPixelHeight + (FNbYPixels - 1) * FPixelSpacing;
  Width := FWidth;
  Height := FHeight;
end;

procedure TplLCDScreen.Paint;
var
  tempBitMap, tempBitmap2: TBitMap;
begin
  if Visible   { Draw only real graphics if display is marked visible. } then
  begin
    tempBitmap := TBitMap.Create;
    tempBitmap.Height := Height;
    tempBitmap.Width := Width;

    with tempBitmap.Canvas do
    begin                   { Border drawing on tempBitmap. }
      Brush.Color := FColor;
      FillRect(ClipRect);
      if FBorderStyle <> bsNone then
      begin
        case FBorderStyle of
          bsRaised: Pen.Color := clBtnShadow;
          bsLowered: Pen.Color := clBtnHighlight;
        end;
        PolyLine([Point(Width - 1, 0), Point(Width - 1, Height - 1), Point(-1, Height - 1)]);

        case FBorderStyle of
          bsRaised: Pen.Color := clBtnHighlight;
          bsLowered: Pen.Color := clBtnShadow;
        end;
        PolyLine([Point(Width - 1, 0), Point(0, 0), Point(0, Height - 1)]);
      end;
    end;

    if Enabled then
    begin
      tempBitmap2 := TBitMap.Create;
      tempBitmap2.Height := Height - 2 * FBorderSpace;
      tempBitmap2.Width := Width - 2 * FBorderSpace;
      with tempBitmap2.Canvas do
      begin
        Brush.Color := FColor;
        FillRect(ClipRect);
      end;

                { Characters drawing on tempBitmap2.
                  The use of bitmap2 is justified to avoid flickering. }

      DrawDisplayCharacters(tempBitmap2);

      { Copy characters from tempBitmap2 to tempBitmap bitmap. }

      BitBlt(tempBitmap.Canvas.Handle,
        FBorderSpace, FBorderSpace,
        Width - 2 * FBorderSpace, Height - 2 * FBorderSpace,
        tempBitMap2.Canvas.Handle, 0, 0, SRCCOPY);

      tempBitmap2.Free;
    end;

    { Copy characters + border from tempBitmap to Window bitmap. }

    BitBlt(Canvas.Handle, 0, 0, Width, Height, tempBitMap.Canvas.Handle, 0, 0, srcCopy);

    tempBitMap.Free;
  end;
end;

procedure TplLCDScreen.DrawDisplayCharacters(BitMap: TBitMap);
var
  i, j, z, lx, ly: integer;
  w1, w2, maxwidth, maxheight: integer;
  onbitmap, offbitmap, offlinebitmap, displaybitmap: TBitMap;
  PByteRow: PByteArray;
  EightPixels: byte;
  PixRect: TRect;
  //RowBmp: TLazIntfImage;

  procedure DrawOnePixel(var b: TBitmap; PixelShape: TPixelShape; Color1, Color2: TColor);
  begin
    with b.Canvas do
    begin
      Brush.Color := Color2;
      FillRect(Rect(0, 0, b.Width, b.Height));

      if (b.Height = 1) and (b.Width = 1) then
        Pixels[0, 0] := Color1
      else
      begin
        Pen.Color := Color1;
        Brush.Color := Color1;
        case PixelShape of
          psSquare: Rectangle(b.Canvas.ClipRect); { Standard square pixels}
          psRound: Ellipse(b.Canvas.ClipRect);   { Round pixels }
        end;
      end;
    end;
  end;

begin
  SetCorrectSize;

  displaybitmap := TBitmap.Create;
  displaybitmap.Width := FNbXPixels;
  displaybitmap.Height := FNbYPixels;
  displaybitmap.PixelFormat := pf1bit;

  { Create a b&w image of the text and/or bitmap using animation parameters (VRef, Href). }
  case FDisplayMode of
    dmText:
    begin
      if pixHRef + FLinesXOffset < -FLinesImage.Width then
        pixHRef := pixHref + FLinesImage.Width + FNbXPixels;
      if pixHRef + FLinesXOffset > FNbXPixels then
        pixHref := pixHref - FLinesImage.Width - FNbXPixels;// - FLinesXOffset;
      if pixVRef + FLinesYOffset < -FLinesImage.Height then
        pixVRef := pixVref + FLinesImage.Height + FNbYPixels;
      if pixVRef + FLinesYOffset > FNbYPixels then
        pixVref := pixVref - FLinesImage.Height - FNbYPixels;// - FLinesXOffset;

      if FLinesAnimMode = amDynamic then
        displaybitmap.Canvas.CopyRect(Rect(pixHRef + FLinesXOffset, pixVRef + FLinesYOffset,
          FLinesImage.Width + pixHRef + FLinesXOffset,
          FLinesImage.Height + pixVRef + FLinesYOffset),
          FLinesImage.Canvas,
          Rect(0, 0, FLinesImage.Width, FLinesImage.Height))
      else
        displaybitmap.Canvas.CopyRect(Rect(FLinesXOffset, FLinesYOffset,
          FLinesImage.Width + FLinesXOffset,
          FLinesImage.Height + FLinesYOffset),
          FLinesImage.Canvas,
          Rect(0, 0, FLinesImage.Width, FLinesImage.Height));
    end;
    dmBitmap:
    begin
      if pixHRef + FBitmapXOffset < -FBitmap.Width then
        pixHRef := pixHref + FBitmap.Width + FNbXPixels;
      if pixHRef + FBitmapXOffset > FNbXPixels then
        pixHref := pixHref - FBitmap.Width - FNbXPixels;// - FBitmapXOffset;
      if pixVRef + FBitmapYOffset < -FBitmap.Height then
        pixVRef := pixVref + FBitmap.Height + FNbYPixels;
      if pixVRef + FBitmapYOffset > FNbYPixels then
        pixVref := pixVref - FBitmap.Height - FNbYPixels;// - FBitmapXOffset;

      if FBitmapAnimMode = amDynamic then
        displaybitmap.Canvas.CopyRect(Rect(pixHRef + FBitmapXOffset, pixVRef + FBitmapYOffset,
          FBitmap.Width + pixHRef + FBitmapXOffset,
          FBitmap.Height + pixVRef + FBitmapYOffset),
          FBitmap.Canvas,
          Rect(0, 0, FBitmap.Width, FBitmap.Height))
      else
        displaybitmap.Canvas.CopyRect(Rect(FBitmapXOffset, FBitmapYOffset,
          FBitmap.Width + FBitmapXOffset,
          FBitmap.Height + FBitmapYOffset),
          FBitmap.Canvas,
          Rect(0, 0, FBitmap.Width, FBitmap.Height));
    end
    else
    begin {dmBoth}
      maxwidth := max(FLinesImage.Width, FBitmap.Width);
      maxheight := max(FLinesImage.Height, FBitmap.Height);

      if (pixHRef + FLinesXOffset < -maxwidth) or (pixHRef + FBitmapXOffset < -maxwidth) then
        pixHRef := pixHref + maxwidth + FNbXPixels;
      if (pixHRef + FLinesXOffset > FNbXPixels) or (pixHRef + FBitmapXOffset > FNbXPixels) then
        pixHref := pixHref - maxwidth - FNbXPixels;
      if (pixVRef + FLinesYOffset < -maxheight) or (pixVRef + FBitmapYOffset < -maxheight) then
        pixVRef := pixVref + maxheight + FNbYPixels;
      if (pixVRef + FLinesYOffset > FNbYPixels) or (pixVRef + FBitmapYOffset > FNbYPixels) then
        pixVref := pixVref - maxheight - FNbYPixels;

      if FLinesAnimMode = amDynamic then
        displaybitmap.Canvas.CopyRect(Rect(pixHRef + FLinesXOffset, pixVRef + FLinesYOffset,
          FLinesImage.Width + pixHRef + FLinesXOffset,
          FLinesImage.Height + pixVRef + FLinesYOffset),
          FLinesImage.Canvas,
          Rect(0, 0, FLinesImage.Width, FLinesImage.Height))
      else
        displaybitmap.Canvas.CopyRect(Rect(FLinesXOffset, FLinesYOffset,
          FLinesImage.Width + FLinesXOffset,
          FLinesImage.Height + FLinesYOffset),
          FLinesImage.Canvas,
          Rect(0, 0, FLinesImage.Width, FLinesImage.Height));

      if FBitmapCopyMode = cmTransparent then
        displaybitmap.Canvas.CopyMode := cmSrcAnd;

      if FBitmapAnimMode = amDynamic then
        displaybitmap.Canvas.CopyRect(Rect(pixHRef + FBitmapXOffset, pixVRef + FBitmapYOffset,
          FBitmap.Width + pixHRef + FBitmapXOffset,
          FBitmap.Height + pixVRef + FBitmapYOffset),
          FBitmap.Canvas,
          Rect(0, 0, FBitmap.Width, FBitmap.Height))
      else
        displaybitmap.Canvas.CopyRect(Rect(FBitmapXOffset, FBitmapYOffset,
          FBitmap.Width + FBitmapXOffset,
          FBitmap.Height + FBitmapYOffset),
          FBitmap.Canvas,
          Rect(0, 0, FBitmap.Width, FBitmap.Height));
    end;
  end;


  { Create onand offbitmap, representing one on/off pixel.
    offlinebitmap is used for speed optimization: the idea is that, globally,
    there are more off pixels than on ones. }

  PixRect := Rect(0, 0, FPixelWidth, FPixelHeight);

  onbitmap := TBitmap.Create;
  onbitmap.Width := FPixelWidth;
  onbitmap.Height := FPixelHeight;
  DrawOnePixel(onbitmap, FPixelShape, FTrueOnColor, FTrueOffColor);

  offbitmap := TBitmap.Create;
  offbitmap.Width := FPixelWidth;
  offbitmap.Height := FPixelHeight;
  DrawOnePixel(offbitmap, FPixelShape, FTrueOffColor, FTrueOffColor);

  offlinebitmap := TBitmap.Create;
  offlinebitmap.Width := Bitmap.Width;
  offlinebitmap.Height := FPixelHeight;
  offlinebitmap.Canvas.Brush.Color := Color;
  offlinebitmap.Canvas.FillRect(offlinebitmap.Canvas.ClipRect);

  offlinebitmap.Canvas.CopyRect(PixRect, offbitmap.Canvas, PixRect);

  w1 := FPixelWidth + FPixelSpacing;
  w2 := 2 * w1 - FPixelSpacing;
  repeat
    offlinebitmap.Canvas.CopyRect(Rect(w1, 0, w2, FPixelHeight), offLinebitmap.Canvas, Rect(0, 0, w1 - FPixelSpacing, FPixelHeight));
    w1 := 2 * w1;
    w2 := 2 * w1 - FPixelSpacing;
  until w1 >= bitmap.Width;

  ly := 0;
  for j := 0 to displaybitmap.Height - 1 do
  begin

    { Draw a offpixels line. }

    Bitmap.Canvas.CopyRect(Rect(0, ly, Bitmap.Width, ly + FPixelHeight), offlinebitmap.Canvas, offlinebitmap.Canvas.ClipRect);

    { 8 per 8 pixels analysis: only switch on the needed pixels. }

    // RowBmp:=displaybitmap.CreateIntfImage;

    // PByteRow := pByteArray(RowBmp.GetDataLineStart(j));
    PByteRow := pByteArray(displaybitmap.ScanLine[j]);

    lx := 7 * (FPixelWidth + FPixelSpacing);
    for i := 0 to (displaybitmap.Width div 7) - 1 do
    begin
      EightPixels := PByteRow^[i];
      if EightPixels <> 255 then
      begin
        for z := 7 downto 0 do
        begin
          if (EightPixels mod 2) = 0 then
            BitMap.Canvas.CopyRect(rect(lx, ly, lx + FPixelWidth, ly + FPixelHeight), onbitmap.Canvas, PixRect);
          EightPixels := EightPixels shr 1;
          lx := lx - FPixelWidth - FPixelSpacing;
        end;
        lx := lx + 16 * (FPixelWidth + FPixelSpacing);
      end
      else
        lx := lx + 8 * (FPixelWidth + FPixelSpacing);
    end;
    ly := ly + FPixelHeight + FPixelSpacing;
  end;
  //    displaybitmap.LoadFromIntfImage(RowBmp);
  //    RowBmp.free;

  displaybitmap.Free;
  offlinebitmap.Free;
  offbitmap.Free;
  onbitmap.Free;

end;



procedure TplLCDScreen.SetBorderStyle(Value: TLCDBorder);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;


procedure TplLCDScreen.SetPixelShape(Value: TPixelShape);
begin
  if Value <> FPixelShape then
  begin
    FPixelShape := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;


procedure TplLCDScreen.SetPixelSpacing(Value: shortint);
begin
  if Value <> FPixelSpacing then
  begin
    FPixelSpacing := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;


procedure TplLCDScreen.SetPixelSize(Value: TPixelSize);
begin
  if Value <> FPixelSize then
  begin
    FPixelSize := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetBorderSpace(Value: shortint);
begin
  if Value <> FBorderSpace then
  begin
    FBorderSpace := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetSpecialEffects(Value: TSpecialEffects);
begin
  if Value <> FSpecialEffects then
  begin
    FSpecialEffects := Value;
    UpdateFLinesImage;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetPixelWidth(Value: shortint);
begin
  if FPixelSize = pixCustom then
    if Value <> FPixelWidth then
      if Value < 1 then
        MessageDlg('Value must be between 1 and 255.', mtError, [mbOK], 0)
      else
      begin
        FPixelWidth := Value;
        if not FAnimationEnabled then
          Invalidate;
      end;
end;


procedure TplLCDScreen.SetPixelHeight(Value: shortint);
begin
  if FPixelSize = pixCustom then
    if Value <> FPixelHeight then
      if Value < 1 then
        MessageDlg('Value must be between 1 and 255.', mtError, [mbOK], 0)
      else
      begin
        FPixelHeight := Value;
        if not FAnimationEnabled then
          Invalidate;
      end;
end;

procedure TplLCDScreen.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetPixelOffColor(Value: TColor);
begin
  if Value <> FPixelOffColor then
  begin
    FPixelOffColor := Value;
    UpdateTrueColors;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetIntensity(Value: shortint);
begin
  if Value <> FIntensity then
  begin
    FIntensity := Value;
    UpdateTrueColors;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetDisplayMode(Value: TDisplayMode);
begin
  if Value <> FDisplayMode then
  begin
    FDisplayMode := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetFont(Value: TFont);
begin
  if Value <> FFont then
    FFont.Assign(Value);
end;


procedure TplLCDScreen.FontOnChange(Sender: TObject);
begin
  UpdateTrueColors;
  UpdateFLinesImage;
  if not FAnimationEnabled then
    Invalidate;
end;


procedure TplLCDScreen.UpdateTrueColors;
var
  DeltaRed, DeltaGreen, DeltaBlue: integer;
  R, G, B: byte;
begin
  DeltaRed := GetRValue(ColorToRGB(FPixelOffColor)) - GetRValue(ColorToRGB(FFont.Color));
  DeltaGreen := GetGValue(ColorToRGB(FPixelOffColor)) - GetGValue(ColorToRGB(FFont.Color));
  DeltaBlue := GetBValue(ColorToRGB(FPixelOffColor)) - GetBValue(ColorToRGB(FFont.Color));
  R := MulDiv(255 - Abs(FIntensity), DeltaRed, 255);
  G := MulDiv(255 - Abs(FIntensity), DeltaGreen, 255);
  B := MulDiv(255 - Abs(FIntensity), DeltaBlue, 255);

  if FIntensity >= 0 then
  begin
    FTrueOnColor := RGB(R, G, B);
    FTrueOffColor := FPixelOffColor;
  end
  else
  begin
    FTrueOnColor := FPixelOffColor;
    FTrueOffColor := RGB(R, G, B);
  end;
end;

procedure TplLCDScreen.SetLines(Value: TStringList);
begin
  FLines.Assign(Value);
end;


procedure TplLCDScreen.LinesOnChange(Sender: TObject);
var
  line, col, offset: byte;
  tempstr, istag: string;
  tempsp: longword;
  underline, strike, bold, italic, blink, inverse, isitatag: boolean;
begin
  Fillchar(Display, sizeof(Display), 0);

  underline := False;
  strike := False;
  bold := False;
  italic := False;
  blink := False;
  inverse := False;

  { Lines per lines, then char per char analysis of Flines.
    Update Display area using the tags.
    Each Display.Char is affected with its Display.SpeEff font effect. }

  for line := 0 to FLines.Count - 1 do
  begin
    tempstr := FLines[line];
    if Length(tempstr) <> 0 then
    begin
      col := 1;
      offset := 0;
      while tempstr[col + offset] <> char(0) do
      begin
        if tempstr[col + offset] <> '<' then
        begin
          Display[line, col].TheChar := tempstr[col + offset];
          tempsp := 0;
          if underline then
            tempsp := 1;
          if strike then
            tempsp := tempsp + 2;
          if bold then
            tempsp := tempsp + 4;
          if italic then
            tempsp := tempsp + 8;
          if blink then
            tempsp := tempsp + 16;
          if inverse then
            tempsp := tempsp + 32;
          Display[line, col].SpEff := tempsp;
        end

        else
        begin
          isitatag := False;
          istag := Copy(tempstr, col + offset, 3);

          if istag = startunderline_tag // '<u>'
          then
          begin
            underline := True;
            Inc(offset, 3);
            Dec(col);
            isitatag := True;
          end;
          if istag = startstrike_tag // '<s>'
          then
          begin
            strike := True;
            Inc(offset, 3);
            Dec(col);
            isitatag := True;
          end;
          if istag = startbold_tag // '<b>'
          then
          begin
            bold := True;
            Inc(offset, 3);
            Dec(col);
            isitatag := True;
          end;
          if istag = startitalic_tag // '<i>'
          then
          begin
            italic := True;
            Inc(offset, 3);
            Dec(col);
            isitatag := True;
          end;

          istag := Copy(tempstr, col + offset, 4);

          if istag = startblinking_tag // '<bl>'
          then
          begin
            blink := True;
            Inc(offset, 4);
            Dec(col);
            isitatag := True;
          end;
          if istag = stopunderline_tag // '</u>'
          then
          begin
            underline := False;
            Inc(offset, 4);
            Dec(col);
            isitatag := True;
          end;

          if istag = stopstrike_tag // '</s>'
          then
          begin
            strike := False;
            Inc(offset, 4);
            Dec(col);
            isitatag := True;
          end;

          if istag = stopbold_tag // '</b>'
          then
          begin
            bold := False;
            Inc(offset, 4);
            Dec(col);
            isitatag := True;
          end;
          if istag = stopitalic_tag // '</i>'
          then
          begin
            italic := False;
            Inc(offset, 4);
            Dec(col);
            isitatag := True;
          end;

          istag := Copy(tempstr, col + offset, 5);

          if istag = startinverse_tag // '<inv>'
          then
          begin
            inverse := True;
            Inc(offset, 5);
            Dec(col);
            isitatag := True;
          end;
          if istag = stopblinking_tag // '</bl>'
          then
          begin
            blink := False;
            Inc(offset, 5);
            Dec(col);
            isitatag := True;
          end;

          istag := Copy(tempstr, col + offset, 6);

          if istag = stopinverse_tag // '</inv>'
          then
          begin
            inverse := False;
            Inc(offset, 6);
            Dec(col);
            isitatag := True;
          end;

          if not isitatag    { special code for '<' char; } then
          begin
            Display[line, col].TheChar := tempstr[col + offset];
            tempsp := 0;
            if underline then
              tempsp := 1;
            if strike then
              tempsp := tempsp + 2;
            if bold then
              tempsp := tempsp + 4;
            if italic then
              tempsp := tempsp + 8;
            if blink then
              tempsp := tempsp + 16;
            if inverse then
              tempsp := tempsp + 32;
            Display[line, col].SpEff := tempsp;
          end;
        end;
        Inc(col);
      end;
    end;
  end;
  UpdateFLinesImage;
  if not FAnimationEnabled then
    Invalidate;
end;

procedure TplLCDScreen.SetLinesXOffset(Value: shortint);
begin
  if Value <> FLinesXOffset then
  begin
    FLinesXOffset := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;



procedure TplLCDScreen.SetLinesYOffset(Value: shortint);
begin
  if Value <> FLinesYOffset then
  begin
    FLinesYOffset := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;


procedure TplLCDScreen.SetLinesAnimMode(Value: TAnimMode);
begin
  if Value <> FLinesAnimMode then
  begin
    FLinesAnimMode := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.UpdateFLinesImage;
var
  line, col, ypos, oldwidth: integer;
  str: string;
  strbitmap: TBitmap;
begin
  FLinesImage.Height := 0;
  FLinesImage.Width := 0;

  strbitmap := TBitmap.Create;
  strbitmap.Width := 0;
  strbitmap.Canvas.Font.Assign(FFont);

  ypos := 0;

  { Lines per lines, then char per char analysis using Display.Char affected off
    its Display.SpeEff font effect. }

  for line := 0 to FLines.Count - 1 do
  begin
    col := 1;
    strbitmap.Width := 0;
    strbitmap.Height := 0;


    while Display[line, col].TheChar <> #0 do
    begin
      strbitmap.Canvas.Font.Color := clBlack;
      strbitmap.Canvas.Brush.Color := clWhite;
      strbitmap.Canvas.Font.Style := [];
      str := Display[line, col].TheChar;
      if Display[line, col].SpEff <> 0 then
      begin
        if (Display[line, col].SpEff and 1 = 1) and (spUnderline in FSpecialEffects) then
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style + [fsUnderline]
        else
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style - [fsUnderline];

        if (Display[line, col].SpEff and 2 <> 0) and (spStrike in FSpecialEffects) then
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style + [fsStrikeOut]
        else
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style - [fsStrikeOut];

        if (Display[line, col].SpEff and 4 <> 0) and (spBold in FSpecialEffects) then
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style + [fsBold]
        else
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style - [fsBold];

        if (Display[line, col].SpEff and 8 <> 0) and (spItalic in FSpecialEffects) then
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style + [fsItalic]
        else
          strbitmap.Canvas.Font.Style := strbitmap.Canvas.Font.Style - [fsItalic];

        if (((Display[line, col].SpEff and 16) <> 0) and (spBlink in FSpecialEffects) and FBlinkingStatus) or
          (((Display[line, col].SpEff and 32) <> 0) and (spInverse in FSpecialEffects)) then
        begin
          strbitmap.Canvas.Font.Color := clWhite;
          strbitmap.Canvas.Brush.Color := clBlack;
        end;
      end;

      oldwidth := strbitmap.Width;
      strbitmap.Width := strbitmap.Width + strbitmap.Canvas.TextWidth(str);
      ;
      strbitmap.Height := max(strbitmap.Height, strbitmap.Canvas.TextHeight(str));
      strbitmap.Canvas.TextOut(oldwidth, 0, str);
      Inc(col);
    end;

    if strbitmap.Height = 0 then
      strbitmap.Height := Abs(FFont.Height);

    FLinesImage.Height := FLinesImage.Height + strbitmap.Height;
    FLinesImage.Width := max(FLinesImage.Width, strbitmap.Width);
    FLinesImage.Canvas.CopyRect(Rect(0, ypos, strbitmap.Width, ypos + strbitmap.Height),
      strbitmap.Canvas,
      strbitmap.Canvas.ClipRect);

    ypos := ypos + strbitmap.Height;
  end;

  strbitmap.Free;
end;


procedure TplLCDScreen.SetBitmap(Value: TBitmap);
begin
  if Value <> FBitmap then
  begin
    FBitmap.Assign(Value);
    FBitmap.Monochrome := True;
    Invalidate;
  end;
end;


procedure TplLCDScreen.SetBitmapCopyMode(Value: TBitmapCopyMode);
begin
  if Value <> FBitmapCopyMode then
  begin
    FBitmapCopyMode := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;


procedure TplLCDScreen.SetBitmapXOffset(Value: shortint);
begin
  if Value <> FBitmapxOffset then
  begin
    FBitmapxOffset := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SetBitmapYOffset(Value: shortint);
begin
  if Value <> FBitmapyOffset then
  begin
    FBitmapyOffset := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;


procedure TplLCDScreen.SetBitmapAnimMode(Value: TAnimMode);
begin
  if Value <> FBitmapAnimMode then
  begin
    FBitmapAnimMode := Value;
    if not FAnimationEnabled then
      Invalidate;
  end;
end;

procedure TplLCDScreen.SeTplLCDAnimator(Value: TplLCDAnimator);
var
  tmr: TModalResult;
begin
  if Value <> FLCDAnimator then
  begin
    if Value <> nil then
    begin
      tmr := mrIgnore;

      if Value.CodeErrorFound and (csDesigning in ComponentState) then
        tmr := MessageDlg('Code synthax error(s) detected in this TplLCDAnimator.' + #13 + #10 +
          #13 + #10 + 'Continue anyway?', mtWarning, [mbAbort, mbIgnore], 0);
      if tmr = mrIgnore then
        FTimer.OnTimer := @TimerOnTimer;
    end;
    FLCDAnimator := Value;
  end;
end;

procedure TplLCDScreen.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(FLCDAnimator) and (AComponent = FLCDAnimator) then
    FLCDAnimator := nil;
end;

procedure TplLCDScreen.SetAnimationDelay(Value: cardinal);
begin
  if Value <> FAnimationDelay then
  begin
    FAnimationDelay := Value;
    FTimer.Interval := Value;
  end;
end;

procedure TplLCDScreen.SetAnimationEnabled(Value: boolean);
begin
  if Value <> FAnimationEnabled then
  begin
    FAnimationEnabled := Value;
    FTimer.Enabled := Value;
  end;
end;


procedure TplLCDScreen.SetAnimationRepeating(Value: boolean);
begin
  if Value <> FAnimationRepeating then
    FAnimationRepeating := Value;
end;


procedure TplLCDScreen.Reset(Value: TResetMode);
begin
  case Value of
    rmDisplay:
    begin
      PixVRef := 0;
      PixHRef := 0;

      LinesOnChange(Self);
    end;

    rmCode: FLCDAnimator.CurrentLine := 0;

    else
    begin {rmDisplayAndCode}
      PixVRef := 0;
      PixHRef := 0;

      LinesOnChange(Self);
      FLCDAnimator.CurrentLine := 0;
    end;
  end;
end;


procedure TplLCDScreen.TimerOnTimer(Sender: TObject);
var
  tempcode: string;
  i: integer;

  ////////////
  function ExtractWord(Value: string): string;
  begin
    ExtractWord := Trim(Copy(Value, 1, Pos('(', Value) - 1));
  end;
  ////////////
  function ExtractParam(Value: string): integer;
  begin
    ExtractParam := StrToInt(Copy(Value, Length(ExtractWord(Value)) + 2, Pos(')', Value) -
      Length(ExtractWord(Value)) - 2));
  end;
  ////////////
  procedure ExtractCode(tempcode: string; var CodeInstructions: array of TCodeInstruction;
    NbOfElement: byte);
  var
    i, n: integer;
    s, m: string;
  begin
    tempcode := Trim(LowerCase(Copy(tempcode, 2, Length(tempcode) - 2)));
    for i := 0 to NbOfElement - 1 do
    begin
      s := Trim(Copy(tempcode, 1, Pos(';', tempcode)));
      m := ExtractWord(s);
      n := ExtractParam(s);
      CodeInstructions[i].word := m;
      CodeInstructions[i].Param := n;
      Delete(tempcode, 1, Length(s));
      tempcode := Trim(tempcode);
    end;
  end;
  ////////////

begin
  if FLCDAnimator <> nil then
  begin
    if FLCDAnimator.Code.Count <> 0 then
      tempcode := FLCDAnimator.Code[FLCDAnimator.CurrentLine];
    P := AllocMem(NbOfThings(tempcode, ';') * SizeOf(TCodeInstruction)); //Removing dynamic Arrays...
    ExtractCode(tempcode, P^, NbOfThings(tempcode, ';'));

    for i := 1 to NbOfThings(tempcode, ';') do
    begin
      if P^[i].word = 'horzscroll' then
        HorzScroll(P^[i].Param);
      if P^[i].word = 'vertscroll' then
        VertScroll(P^[i].Param);
      if P^[i].word = 'setintensity' then
        SetIntensity(P^[i].Param);
      if P^[i].word = 'animationdelay' then
        SetAnimationDelay(P^[i].Param);
      if P^[i].word = 'gotoline' then
        FLCDAnimator.CurrentLine := Min(P^[i].Param, FLCDAnimator.Code.Count);
      if P^[i].word = 'resetdisplay' then
        Reset(rmDisplay);
    end;
    if Assigned(FLCDAnimator.FOnLineExecuted) then
      FLCDAnimator.FOnLineExecuted(FLCDAnimator, FLCDAnimator.CurrentLine);

    if FLCDAnimator.CurrentLine = FLCDAnimator.Code.Count - 1 then
    begin
      FLCDAnimator.CurrentLine := 0;
      if not FAnimationRepeating then
        SetAnimationEnabled(False);
      if Assigned(FLCDAnimator.FOnEndCode) then
        FLCDAnimator.FOnEndCode(FLCDAnimator);
    end
    else
      FLCDAnimator.CurrentLine := FLCDAnimator.CurrentLine + 1;
    FreeMem(P); //Removing dynamic Arrays...

    FBlinkingStatus := not FBlinkingStatus;
    if (spBlink in FSpecialEffects) and (FDisplayMode <> dmBitmap) then
      UpdateFLinesImage;

    Paint;
  end;
end;

procedure TplLCDScreen.HorzScroll(Value: shortint);
begin
  pixHRef := pixHRef + Value;
end;


procedure TplLCDScreen.VertScroll(Value: shortint);
begin
  pixVRef := pixVRef + Value;
end;

//==================== TplLCDAnimator ============================

constructor TplLCDAnimator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCode := TStringList.Create;
  FCodeErrorFound := False;
  FCurrentLine := 0;
end;


destructor TplLCDAnimator.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

procedure TplLCDAnimator.SetCode(Value: TStrings);
begin
  FCode.Assign(Value);
end;


procedure TplLCDAnimator.SetCurrentLine(Value: smallint);
begin
  if Value <> FCurrentLine then
  begin
    if Value > Code.Count then
      Value := Max(0, Code.Count - 1);
    FCurrentLine := Value;
  end;
end;


end.
