unit SpkGUITools;

{$mode ObjFpc}
{$H+}
{$DEFINE SPKGUITOOLS}
{.$define EnhancedRecordSupport}
//the fpcbugworkaround is only necessary when using inline for DrawRoundRect
{.$define FpcBugWorkAround}

interface

{$MESSAGE HINT 'Every rect in this module are exact rectanges (not like in WINAPI without right and bottom)'}

uses
  LCLType, Graphics, SysUtils, Classes, Controls, StdCtrls, SpkGraphTools, SpkMath;

type
  TCornerPos = (cpLeftTop, cpRightTop, cpLeftBottom, cpRightBottom);
  TCornerKind = (cpRound, cpNormal);
  TBackgroundKind = (bkSolid, bkVerticalGradient, bkHorizontalGradient,
                    bkConcave);

  TSpkCheckboxStyle = (cbsCheckbox, cbsRadioButton);
  TSpkCheckboxState = (cbsIdle, cbsHotTrack, cbsPressed, cbsDisabled);

  TGUITools = class(TObject)
  protected
    class procedure FillGradientRectangle(ACanvas: TCanvas; Rect: T2DIntRect; ColorFrom: TColor;
      ColorTo: TColor; GradientKind: TBackgroundKind);
    class procedure SaveClipRgn(DC : HDC; var OrgRgnExists : boolean; var OrgRgn : HRGN);
    class procedure RestoreClipRgn(DC : HDC; OrgRgnExists : boolean; var OrgRgn : HRGN);
  public
    // *** Lines ***

    // Performance:
    // w/ClipRect:  Bitmap is faster (2x)
    // wo/ClipRect: Canvas is faster (a little)
    class procedure DrawHLine(ABitmap : TBitmap;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor); overload;
    class procedure DrawHLine(ABitmap : TBitmap;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;
    class procedure DrawHLine(ACanvas : TCanvas;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor); overload;
    class procedure DrawHLine(ACanvas : TCanvas;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;


    // Performance:
    // w/ClipRect:  Bitmap is faster (2x)
    // wo/ClipRect: Canvas is faster (a little)
    class procedure DrawVLine(ABitmap : TBitmap;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor); overload;
    class procedure DrawVLine(ABitmap : TBitmap;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;
    class procedure DrawVLine(ACanvas : TCanvas;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor); overload;
    class procedure DrawVLine(ACanvas : TCanvas;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;

    // *** Background and frame tools ***

    // Performance:
    // w/ClipRect:  Bitmap is faster (extremely)
    // wo/ClipRect: Bitmap is faster (extremely)
    class procedure DrawAARoundCorner(ABitmap : TBitmap;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor); overload;
    class procedure DrawAARoundCorner(ABitmap : TBitmap;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor;
                                     ClipRect : T2DIntRect); overload;
    class procedure DrawAARoundCorner(ACanvas : TCanvas;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor); overload;
    class procedure DrawAARoundCorner(ACanvas : TCanvas;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor;
                                     ClipRect : T2DIntRect); overload;

    // Performance:
    // w/ClipRect:  Bitmap is faster (extremely)
    // wo/ClipRect: Bitmap is faster (extremely)
    class procedure DrawAARoundFrame(ABitmap : TBitmap;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor); overload;
    class procedure DrawAARoundFrame(ABitmap : TBitmap;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor;
                                    ClipRect : T2DIntRect); overload;
    class procedure DrawAARoundFrame(ACanvas : TCanvas;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor); overload;
    class procedure DrawAARoundFrame(ACanvas : TCanvas;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor;
                                    ClipRect : T2DIntRect); overload;

    class procedure RenderBackground(ABuffer : TBitmap;
                                    Rect : T2DIntRect;
                                    Color1, Color2 : TColor;
                                    BackgroundKind : TBackgroundKind);

    class procedure CopyRoundCorner(ABuffer : TBitmap;
                                   ABitmap : TBitmap;
                                   SrcPoint : T2DIntVector;
                                   DstPoint : T2DIntVector;
                                   Radius : integer;
                                   CornerPos : TCornerPos;
                                   Convex : boolean = true); overload;
    class procedure CopyRoundCorner(ABuffer : TBitmap;
                                   ABitmap : TBitmap;
                                   SrcPoint : T2DIntVector;
                                   DstPoint : T2DIntVector;
                                   Radius : integer;
                                   CornerPos : TCornerPos;
                                   ClipRect : T2DIntRect;
                                   Convex : boolean = true); overload;

    class procedure CopyCorner(ABuffer : TBitmap;
                              ABitmap: TBitmap;
                              SrcPoint : T2DIntVector;
                              DstPoint: T2DIntVector;
                              Radius: integer); overload; inline;
    class procedure CopyCorner(ABuffer : TBitmap;
                              ABitmap: TBitmap;
                              SrcPoint : T2DIntVector;
                              DstPoint: T2DIntVector;
                              Radius: integer;
                              ClipRect : T2DIntRect); overload; inline;

    class procedure CopyRectangle(ABuffer : TBitmap;
                                 ABitmap: TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint: T2DIntVector;
                                 Width: integer;
                                 Height : integer); overload;
    class procedure CopyRectangle(ABuffer : TBitmap;
                                 ABitmap : TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint : T2DIntVector;
                                 Width : integer;
                                 Height : integer;
                                 ClipRect : T2DIntRect); overload;
    class procedure CopyMaskRectangle(ABuffer : TBitmap;
                                     AMask : TBitmap;
                                     ABitmap : TBitmap;
                                     SrcPoint : T2DIntVector;
                                     DstPoint : T2DIntVector;
                                     Width : integer;
                                     Height : integer); overload;
    class procedure CopyMaskRectangle(ABuffer : TBitmap;
                                     AMask : TBitmap;
                                     ABitmap : TBitmap;
                                     SrcPoint : T2DIntVector;
                                     DstPoint : T2DIntVector;
                                     Width : integer;
                                     Height : integer;
                                     ClipRect : T2DIntRect); overload;

    // Performance (RenderBackground + CopyRoundRect vs DrawRoundRect):
    // w/ClipRect  : Bitmap faster for smaller radiuses, Canvas faster for larger
    // wo/ClipRect : Bitmap faster for smaller radiuses, Canvas faster for larger
    class procedure CopyRoundRect(ABuffer : TBitmap;
                                 ABitmap : TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint : T2DIntVector;
                                 Width, Height : integer;
                                 Radius : integer;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;
    class procedure CopyRoundRect(ABuffer : TBitmap;
                                 ABitmap : TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint : T2DIntVector;
                                 Width, Height : integer;
                                 Radius : integer;
                                 ClipRect : T2DIntRect;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;


    class procedure DrawRoundRect(ACanvas : TCanvas;
                                 Rect : T2DIntRect;
                                 Radius : integer;
                                 ColorFrom : TColor;
                                 ColorTo : TColor;
                                 GradientKind : TBackgroundKind;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;
    class procedure DrawRoundRect(ACanvas : TCanvas;
                                 Rect : T2DIntRect;
                                 Radius : integer;
                                 ColorFrom : TColor;
                                 ColorTo : TColor;
                                 GradientKind : TBackgroundKind;
                                 ClipRect : T2DIntRect;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;

    class procedure DrawRegion(ACanvas : TCanvas;
                              Region : HRGN;
                              Rect : T2DIntRect;
                              ColorFrom : TColor;
                              ColorTo : TColor;
                              GradientKind : TBackgroundKind); overload;
    class procedure DrawRegion(ACanvas : TCanvas;
                              Region : HRGN;
                              Rect : T2DIntRect;
                              ColorFrom : TColor;
                              ColorTo : TColor;
                              GradientKind : TBackgroundKind;
                              ClipRect : T2DIntRect); overload;

    // Imagelist tools
    class procedure DrawImage(ABitmap : TBitmap;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector); overload; inline;
    class procedure DrawImage(ABitmap : TBitmap;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector;
                             ClipRect : T2DIntRect); overload; inline;
    class procedure DrawImage(ACanvas : TCanvas;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector); overload; inline;
    class procedure DrawImage(ACanvas : TCanvas;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector;
                             ClipRect : T2DIntRect); overload;

    class procedure DrawDisabledImage(ABitmap : TBitmap;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector); overload; inline;
    class procedure DrawDisabledImage(ABitmap : TBitmap;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector;
                                     ClipRect : T2DIntRect); overload; inline;
    class procedure DrawDisabledImage(ACanvas : TCanvas;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector); overload;
    class procedure DrawDisabledImage(ACanvas : TCanvas;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector;
                                     ClipRect : T2DIntRect); overload; inline;

    // Checkbox
    class procedure DrawCheckbox(ACanvas: TCanvas;
                                 x,y: Integer;
                                 AState: TCheckboxState;
                                 ACheckboxState: TSpkCheckboxState;
                                 AStyle: TSpkCheckboxStyle); overload;
    class procedure DrawCheckbox(ACanvas: TCanvas;
                                 x,y: Integer;
                                 AState: TCheckboxState;
                                 ACheckboxState: TSpkCheckboxState;
                                 AStyle: TSpkCheckboxStyle;
                                 ClipRect: T2DIntRect); overload;

    // Text tools
    class procedure DrawText(ABitmap : TBitmap;
                        x, y : integer;
                        const AText : string;
                        TextColor: TColor); overload;
    class procedure DrawText(ABitmap : TBitmap;
                        x, y : integer;
                        const AText : string;
                        TextColor : TColor;
                        ClipRect: T2DIntRect); overload;
    class procedure DrawMarkedText(ACanvas : TCanvas;
                                  x, y : integer;
                                  const AText, AMarkPhrase : string;
                                  TextColor : TColor;
                                  CaseSensitive : boolean = false); overload;
    class procedure DrawMarkedText(ACanvas : TCanvas;
                                  x, y : integer;
                                  const AText, AMarkPhrase : string;
                                  TextColor : TColor;
                                  ClipRect : T2DIntRect;
                                  CaseSensitive : boolean = false); overload;
    class procedure DrawText(ACanvas : TCanvas;
                        x, y : integer;
                        const AText : string;
                        TextColor : TColor); overload;
    class procedure DrawText(ACanvas : TCanvas;
                        x, y : integer;
                        const AText : string;
                        TextColor : TColor;
                        ClipRect : T2DIntRect); overload;
    class procedure DrawFitWText(ABitmap : TBitmap;
                                x1, x2 : integer;
                                y : integer;
                                const AText : string;
                                TextColor : TColor;
                                Align : TAlignment); overload;
    class procedure DrawFitWText(ACanvas : TCanvas;
                                x1, x2 : integer;
                                y : integer;
                                const AText : string;
                                TextColor : TColor;
                                Align : TAlignment); overload;

    class procedure DrawOutlinedText(ABitmap : TBitmap;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor); overload;
    class procedure DrawOutlinedText(ABitmap : TBitmap;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor;
                                    ClipRect : T2DIntRect); overload;
    class procedure DrawOutlinedText(ACanvas : TCanvas;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor); overload;
    class procedure DrawOutlinedText(ACanvas : TCanvas;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor;
                                    ClipRect : T2DIntRect); overload;
    class procedure DrawFitWOutlinedText(ABitmap: TBitmap;
                                        x1, x2 : integer;
                                        y: integer;
                                        const AText: string;
                                        TextColor,
                                        OutlineColor: TColor;
                                        Align: TAlignment); overload;
    class procedure DrawFitWOutlinedText(ACanvas: TCanvas;
                                        x1, x2 : integer;
                                        y: integer;
                                        const AText: string;
                                        TextColor,
                                        OutlineColor: TColor;
                                        Align: TAlignment); overload;
end;

implementation

uses
 types, LCLIntf, IntfGraphics, Math, Themes;

{ TSpkGUITools }

class procedure TGUITools.CopyRoundCorner(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Radius: integer; CornerPos: TCornerPos;
  ClipRect: T2DIntRect; Convex: boolean);

var BufferRect, BitmapRect, TempRect : T2DIntRect;
    OrgSrcRect, UnClippedDstRect, OrgDstRect : T2DIntRect;
    SrcRect: T2DIntRect;
    Offset: T2DIntVector;
    Center: T2DIntVector;
    y: Integer;
    SrcLine: Pointer;
    DstLine: Pointer;
    SrcPtr, DstPtr : PByte;
    x: Integer;
    Dist : double;
    SrcImg, DestImg: TLazIntfImage;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if Radius<1 then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

//todo minimize use of temps here
{$ifdef EnhancedRecordSupport}
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Radius-1,
                                                   SrcPoint.y+Radius-1),
                                 OrgSrcRect)) then exit;
{$else}
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
TempRect.Create(SrcPoint.x, SrcPoint.y, SrcPoint.x+Radius-1, SrcPoint.y+Radius-1);
if not(BufferRect.IntersectsWith(TempRect, OrgSrcRect)) then exit;
{$endif}

{$ifdef EnhancedRecordSupport}
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Radius-1,
                                                   DstPoint.y+Radius-1),
                                 UnClippedDstRect)) then exit;
{$else}
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
//todo: calling create twice
TempRect.Create(DstPoint.x, DstPoint.y, DstPoint.x+Radius-1, DstPoint.y+Radius-1);
if not(BitmapRect.IntersectsWith(TempRect, UnClippedDstRect)) then exit;
{$endif}

if not(ClipRect.IntersectsWith(UnClippedDstRect, OrgDstRect)) then
   exit;

Offset:=DstPoint - SrcPoint;

if not(OrgSrcRect.IntersectsWith(OrgDstRect - Offset, SrcRect)) then exit;

// Ustalamy pozycjê œrodka ³uku
{$ifdef EnhancedRecordSupport}
case CornerPos of
     cpLeftTop: Center:=T2DIntVector.create(SrcPoint.x + radius - 1, SrcPoint.y + Radius - 1);
     cpRightTop: Center:=T2DIntVector.create(SrcPoint.x, SrcPoint.y + Radius - 1);
     cpLeftBottom: Center:=T2DIntVector.Create(SrcPoint.x + radius - 1, SrcPoint.y);
     cpRightBottom: Center:=T2DIntVector.Create(SrcPoint.x, SrcPoint.y);
end;
{$else}
case CornerPos of
     cpLeftTop: Center.create(SrcPoint.x + radius - 1, SrcPoint.y + Radius - 1);
     cpRightTop: Center.create(SrcPoint.x, SrcPoint.y + Radius - 1);
     cpLeftBottom: Center.Create(SrcPoint.x + radius - 1, SrcPoint.y);
     cpRightBottom: Center.Create(SrcPoint.x, SrcPoint.y);
end;
{$endif}

// Czy jest cokolwiek do przetworzenia?
if Convex then
   begin
   //todo: remove the check since is not necessary
   if (SrcRect.left<=SrcRect.right) and (SrcRect.top<=SrcRect.bottom) then
   begin
     SrcImg := ABuffer.CreateIntfImage;
     DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.top to SrcRect.bottom do
          begin
          SrcLine:=SrcImg.GetDataLineStart(y);
          DstLine:=DestImg.GetDataLineStart(y+Offset.y);

          SrcPtr:=pointer(PtrInt(SrcLine) + 3*SrcRect.left);
          DstPtr:=pointer(PtrInt(DstLine) + 3*(SrcRect.left + Offset.x));
          for x := SrcRect.left to SrcRect.right do
              begin
              {$ifdef EnhancedRecordSupport}
              Dist:=Center.DistanceTo(T2DIntVector.create(x, y));
              {$else}
              Dist:=Center.DistanceTo(x, y);
              {$endif}
              if Dist <= (Radius-1) then
                 Move(SrcPtr^,DstPtr^,3);

              inc(SrcPtr,3);
              inc(DstPtr,3);
              end;
          end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
   end;
   end
else
   begin
   if (SrcRect.left<=SrcRect.right) and (SrcRect.top<=SrcRect.bottom) then
   begin
     SrcImg := ABuffer.CreateIntfImage;
     DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.top to SrcRect.bottom do
          begin
          SrcLine:=SrcImg.GetDataLineStart(y);
          DstLine:=DestImg.GetDataLineStart(y+Offset.y);

          SrcPtr:=pointer(PtrInt(SrcLine) + 3*SrcRect.left);
          DstPtr:=pointer(PtrInt(DstLine) + 3*(SrcRect.left + Offset.x));
          for x := SrcRect.left to SrcRect.right do
              begin
              {$ifdef EnhancedRecordSupport}
              Dist:=Center.DistanceTo(T2DIntVector.create(x, y));
              {$else}
              Dist:=Center.DistanceTo(x, y);
              {$endif}
              if Dist >= (Radius-1) then
                 Move(SrcPtr^,DstPtr^,3);

              inc(SrcPtr,3);
              inc(DstPtr,3);
              end;
          end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
   end;
   end;
end;

class procedure TGUITools.CopyRoundRect(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height, Radius: integer; ClipRect: T2DIntRect;
  LeftTopRound, RightTopRound, LeftBottomRound, RightBottomRound: boolean);

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyBackground: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzamy poprawnoœæ
if Radius<0 then
   exit;

if (Radius>Width div 2) or (Radius>Height div 2) then exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$REGION 'Wype³niamy prostok¹ty'}
// Góra
CopyRectangle(ABuffer,
              ABitmap,
              {$ifdef EnhancedRecordSupport}
              T2DIntPoint.create(SrcPoint.x + radius, SrcPoint.y),
              T2DIntPoint.create(DstPoint.x + radius, DstPoint.y),
              {$else}
              Create2DIntPoint(SrcPoint.x + radius, SrcPoint.y),
              Create2DIntPoint(DstPoint.x + radius, DstPoint.y),
              {$endif}
              width - 2*radius,
              radius,
              ClipRect);
// Dó³
CopyRectangle(ABuffer,
              ABitmap,
              {$IFDEF EnhancedRecordSupport}
              T2DIntPoint.create(SrcPoint.x + radius, SrcPoint.y + height - radius),
              T2DIntPoint.create(DstPoint.x + radius, DstPoint.y + height - radius),
              {$ELSE}
              Create2DIntPoint(SrcPoint.x + radius, SrcPoint.y + height - radius),
              Create2DIntPoint(DstPoint.x + radius, DstPoint.y + height - radius),
              {$ENDIF}
              width - 2*radius,
              radius,
              ClipRect);
// Œrodek
CopyRectangle(ABuffer,
              ABitmap,
              {$IFDEF EnhancedRecordSupport}
              T2DIntPoint.create(SrcPoint.x, SrcPoint.y + radius),
              T2DIntPoint.create(DstPoint.x, DstPoint.y + radius),
              {$ELSE}
              Create2DIntPoint(SrcPoint.x, SrcPoint.y + radius),
              Create2DIntPoint(DstPoint.x, DstPoint.y + radius),
              {$ENDIF}
              width,
              height - 2*radius,
              ClipRect);
{$ENDREGION}

// Wype³niamy naro¿niki

{$REGION 'Lewy górny'}
if LeftTopRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                             T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                             Create2DIntPoint(DstPoint.x, DstPoint.y),
                             {$ENDIF}
                             Radius,
                             cpLeftTop,
                             ClipRect,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                        T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                        Create2DIntPoint(DstPoint.x, DstPoint.y),
                        {$ENDIF}
                        Radius,
                        ClipRect);
{$ENDREGION}

{$REGION 'Prawy górny'}
if RightTopRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                             T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                             Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                             {$ENDIF}
                             Radius,
                             cpRightTop,
                             ClipRect,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                        T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                        Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                        {$ENDIF}
                        Radius,
                        ClipRect);
{$ENDREGION}

{$REGION 'Lewy dolny'}
if LeftBottomRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                             T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                             Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                             {$ENDIF}
                             Radius,
                             cpLeftBottom,
                             ClipRect,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                        T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                        Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                        {$ENDIF}
                        Radius,
                        ClipRect);
{$ENDREGION}

{$REGION 'Prawy dolny'}
if RightBottomRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                             T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                             Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                             {$ENDIF}
                             Radius,
                             cpRightBottom,
                             ClipRect,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                        T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                        Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                        {$ENDIF}
                        Radius,
                        ClipRect);
{$ENDREGION'}
end;

class procedure TGUITools.CopyRoundRect(ABuffer : TBitmap; ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height, Radius: integer; LeftTopRound,
  RightTopRound, LeftBottomRound, RightBottomRound: boolean);

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyBackground: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzamy poprawnoœæ
if Radius<0 then
   exit;

if (Radius>Width div 2) or (Radius>Height div 2) then exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$REGION 'Wype³niamy prostok¹ty'}
// Góra
CopyRectangle(ABuffer,
              ABitmap,
              {$IFDEF EnhancedRecordSupport}
              T2DIntPoint.create(SrcPoint.x + radius, SrcPoint.y),
              T2DIntPoint.create(DstPoint.x + radius, DstPoint.y),
              {$ELSE}
              Create2DIntPoint(SrcPoint.x + radius, SrcPoint.y),
              Create2DIntPoint(DstPoint.x + radius, DstPoint.y),
              {$ENDIF}
              width - 2*radius,
              radius);
// Dó³
CopyRectangle(ABuffer,
              ABitmap,
              {$IFDEF EnhancedRecordSupport}
              T2DIntPoint.create(SrcPoint.x + radius, SrcPoint.y + height - radius),
              T2DIntPoint.create(DstPoint.x + radius, DstPoint.y + height - radius),
              {$ELSE}
              Create2DIntPoint(SrcPoint.x + radius, SrcPoint.y + height - radius),
              Create2DIntPoint(DstPoint.x + radius, DstPoint.y + height - radius),
              {$ENDIF}
              width - 2*radius,
              radius);
// Œrodek
CopyRectangle(ABuffer,
              ABitmap,
              {$IFDEF EnhancedRecordSupport}
              T2DIntPoint.create(SrcPoint.x, SrcPoint.y + radius),
              T2DIntPoint.create(DstPoint.x, DstPoint.y + radius),
              {$ELSE}
              Create2DIntPoint(SrcPoint.x, SrcPoint.y + radius),
              Create2DIntPoint(DstPoint.x, DstPoint.y + radius),
              {$ENDIF}
              width,
              height - 2*radius);
{$ENDREGION}

// Wype³niamy naro¿niki
{$REGION 'Lewy górny'}
if LeftTopRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                             T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                             Create2DIntPoint(DstPoint.x, DstPoint.y),
                             {$ENDIF}
                             Radius,
                             cpLeftTop,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                        T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                        Create2DIntPoint(DstPoint.x, DstPoint.y),
                        {$ENDIF}
                        Radius);
{$ENDREGION}

{$REGION 'Prawy górny'}
if RightTopRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                             T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                             Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                             {$ENDIF}
                             Radius,
                             cpRightTop,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                        T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                        Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                        {$ENDIF}
                        Radius);
{$ENDREGION}

{$REGION 'Lewy dolny'}
if LeftBottomRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                             T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                             Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                             {$ENDIF}
                             Radius,
                             cpLeftBottom,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                        T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                        Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                        {$ENDIF}
                        Radius);
{$ENDREGION}

{$REGION 'Prawy dolny'}
if RightBottomRound then
   TGUITools.CopyRoundCorner(ABuffer,
                             ABitmap,
                             {$IFDEF EnhancedRecordSupport}
                             T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                             T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                             {$ELSE}
                             Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                             Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                             {$ENDIF}
                             Radius,
                             cpRightBottom,
                             true)
else
   TGUITools.CopyCorner(ABuffer,
                        ABitmap,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                        T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                        {$ELSE}
                        Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                        Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                        {$ENDIF}
                        Radius);
{$ENDREGION'}
end;

class procedure TGUITools.CopyRectangle(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height: integer);

var BufferRect, BitmapRect : T2DIntRect;
    SrcRect, DstRect : T2DIntRect;
    ClippedSrcRect : T2DIntRect;
    Offset : T2DIntVector;
    y: Integer;
    SrcLine: Pointer;
    DstLine: Pointer;
    SrcImg: TLazIntfImage;
    DestImg: TLazIntfImage;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if (Width<1) or (Height<1) then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$IFDEF EnhancedRecordSupport}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);

if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ELSE}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);

if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ENDIF}

// Liczymy offset Ÿród³owego do docelowego recta
Offset:=DstPoint - SrcPoint;

// Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
// offset maj¹ jak¹œ czêœæ wspóln¹
if not(SrcRect.IntersectsWith(DstRect - Offset, ClippedSrcRect)) then exit;

// Jeœli jest cokolwiek do przetworzenia, wykonaj operacjê
  if (ClippedSrcRect.left<=ClippedSrcRect.right) and (ClippedSrcRect.top<=ClippedSrcRect.bottom) then
  begin
    SrcImg := ABuffer.CreateIntfImage;
    DestImg := ABitmap.CreateIntfImage;
   for y := ClippedSrcRect.top to ClippedSrcRect.bottom do
       begin
       SrcLine:=SrcImg.GetDataLineStart(y);
       DstLine:=DestImg.GetDataLineStart(y+Offset.y);

       Move(pointer(PtrInt(SrcLine) + 3*ClippedSrcRect.left)^,
            pointer(PtrInt(DstLine) + 3*(ClippedSrcRect.left + Offset.x))^,
            3*ClippedSrcRect.Width);
      end;
     ABitmap.LoadFromIntfImage(DestImg);
     SrcImg.Destroy;
     DestImg.Destroy;
  end;
end;

class procedure TGUITools.CopyCorner(ABuffer : TBitmap; ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Radius: integer);

begin
CopyRectangle(ABuffer, ABitmap, SrcPoint, DstPoint, Radius, Radius);
end;

class procedure TGUITools.CopyCorner(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Radius: integer; ClipRect: T2DIntRect);
begin
CopyRectangle(ABuffer, ABitmap, SrcPoint, DstPoint, Radius, Radius, ClipRect);
end;

class procedure TGUITools.CopyMaskRectangle(ABuffer, AMask, ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Width, Height: integer);

var BufferRect, BitmapRect : T2DIntRect;
    SrcRect, DstRect : T2DIntRect;
    ClippedSrcRect : T2DIntRect;
    Offset : T2DIntVector;
    y: Integer;
    SrcLine: Pointer;
    MaskLine: Pointer;
    DstLine: Pointer;
    SrcImg: TLazIntfImage;
    MaskImg: TLazIntfImage;
    DestImg: TLazIntfImage;
    i: Integer;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

if (AMask.PixelFormat<>pf8bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 8-bitowe maski s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if (Width<1) or (Height<1) then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

if (ABuffer.Width<>AMask.Width) or
   (ABuffer.Height<>AMask.Height) then exit;

{$IFDEF EnhancedRecordSupport}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);

if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ELSE}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);

if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ENDIF}

// Liczymy offset Ÿród³owego do docelowego recta
Offset:=DstPoint - SrcPoint;

// Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
// offset maj¹ jak¹œ czêœæ wspóln¹
if not(SrcRect.IntersectsWith(DstRect - Offset, ClippedSrcRect)) then exit;

// Jeœli jest cokolwiek do przetworzenia, wykonaj operacjê
if (ClippedSrcRect.left<=ClippedSrcRect.right) and (ClippedSrcRect.top<=ClippedSrcRect.bottom) then
begin
  SrcImg := ABuffer.CreateIntfImage;
  DestImg := ABitmap.CreateIntfImage;
  MaskImg := AMask.CreateIntfImage;
   for y := ClippedSrcRect.top to ClippedSrcRect.bottom do
       begin
       SrcLine:=SrcImg.GetDataLineStart(y);
       SrcLine:=pointer(PtrInt(SrcLine) + 3 * ClippedSrcRect.left);

       MaskLine:=MaskImg.GetDataLineStart(y);
       MaskLine:=pointer(PtrInt(MaskLine) + ClippedSrcRect.left);

       DstLine:=DestImg.GetDataLineStart(y+Offset.y);
       DstLine:=pointer(PtrInt(DstLine) + 3 * (ClippedSrcRect.left + Offset.x));

       for i := 0 to ClippedSrcRect.Width - 1 do
           begin
           if PByte(MaskLine)^<128 then
              Move(SrcLine^,DstLine^,3);

           SrcLine:=pointer(PtrInt(SrcLine)+3);
           DstLine:=pointer(PtrInt(DstLine)+3);
           MaskLine:=pointer(PtrInt(MaskLine)+1);
           end;
       end;
   ABitmap.LoadFromIntfImage(DestImg);
   DestImg.Destroy;
   SrcImg.Destroy;
   MaskImg.Destroy;
end;
end;

class procedure TGUITools.CopyMaskRectangle(ABuffer, AMask, ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Width, Height: integer;
  ClipRect: T2DIntRect);

var BufferRect, BitmapRect : T2DIntRect;
    SrcRect, DstRect : T2DIntRect;
    ClippedSrcRect, ClippedDstRect : T2DIntRect;
    Offset : T2DIntVector;
    y: Integer;
    SrcImg: TLazIntfImage;
    MaskImg: TLazIntfImage;
    DestImg: TLazIntfImage;
    SrcLine: Pointer;
    DstLine: Pointer;
    i: Integer;
    MaskLine: Pointer;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyMaskRectangle: Tylko 24-bitowe bitmapy s¹ akceptowane!');
if AMask.PixelFormat<>pf8bit then
   raise exception.create('TSpkGUITools.CopyMaskRectangle: Tylko 8-bitowe maski s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if (Width<1) or (Height<1) then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

if (ABuffer.Width<>AMask.Width) or
   (ABuffer.Height<>AMask.Height) then
   raise exception.create('TSpkGUITools.CopyMaskRectangle: Maska ma nieprawid³owe rozmiary!');

{$IFDEF EnhancedRecordSupport}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ELSE}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ENDIF}

// Dodatkowo przycinamy docelowy rect
if not(DstRect.IntersectsWith(ClipRect, ClippedDstRect)) then
   Exit;

// Liczymy offset Ÿród³owego do docelowego recta
Offset:=DstPoint - SrcPoint;

// Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
// offset maj¹ jak¹œ czêœæ wspóln¹
if not(SrcRect.IntersectsWith(ClippedDstRect - Offset, ClippedSrcRect)) then exit;

// Jeœli jest cokolwiek do przetworzenia, wykonaj operacjê
if (ClippedSrcRect.left<=ClippedSrcRect.right) and (ClippedSrcRect.top<=ClippedSrcRect.bottom) then
begin
  SrcImg := ABuffer.CreateIntfImage;
  DestImg := ABitmap.CreateIntfImage;
  MaskImg := ABitmap.CreateIntfImage;
   for y := ClippedSrcRect.top to ClippedSrcRect.bottom do
       begin
       SrcLine:=SrcImg.GetDataLineStart(y);
       SrcLine:=pointer(PtrInt(SrcLine) + 3 * ClippedSrcRect.left);

       MaskLine:=MaskImg.GetDataLineStart(y);
       MaskLine:=pointer(PtrInt(MaskLine) + ClippedSrcRect.left);

       DstLine:=DestImg.GetDataLineStart(y+Offset.y);
       DstLine:=pointer(PtrInt(DstLine) + 3 * (ClippedSrcRect.left + Offset.x));

       for i := 0 to ClippedSrcRect.width - 1 do
           begin
           if PByte(MaskLine)^<128 then
              Move(SrcLine^, DstLine^, 3);

           SrcLine:=pointer(PtrInt(SrcLine)+3);
           DstLine:=pointer(PtrInt(DstLine)+3);
           MaskLine:=pointer(PtrInt(MaskLine)+1);
           end;
       end;
   ABitmap.LoadFromIntfImage(DestImg);
   SrcImg.Destroy;
   DestImg.Destroy;
   MaskImg.Destroy;
end;
end;

class procedure TGUITools.CopyRectangle(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height: integer; ClipRect: T2DIntRect);

var BufferRect, BitmapRect : T2DIntRect;
    SrcRect, DstRect : T2DIntRect;
    ClippedSrcRect, ClippedDstRect : T2DIntRect;
    Offset : T2DIntVector;
    y: Integer;
    DestImg: TLazIntfImage;
    SrcImg: TLazIntfImage;
    SrcLine: Pointer;
    DstLine: Pointer;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if (Width<1) or (Height<1) then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$IFDEF EnhancedRecordSupport}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ELSE}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ENDIF}

// Dodatkowo przycinamy docelowy rect
if not(DstRect.IntersectsWith(ClipRect, ClippedDstRect)) then
   Exit;

// Liczymy offset Ÿród³owego do docelowego recta
Offset:=DstPoint - SrcPoint;

// Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
// offset maj¹ jak¹œ czêœæ wspóln¹
if not(SrcRect.IntersectsWith(ClippedDstRect - Offset, ClippedSrcRect)) then exit;

// Jeœli jest cokolwiek do przetworzenia, wykonaj operacjê
if (ClippedSrcRect.left<=ClippedSrcRect.right) and (ClippedSrcRect.top<=ClippedSrcRect.bottom) then
begin
  SrcImg := ABuffer.CreateIntfImage;
  DestImg := ABitmap.CreateIntfImage;
   for y := ClippedSrcRect.top to ClippedSrcRect.bottom do
       begin
       SrcLine:=SrcImg.GetDataLineStart(y);
       DstLine:=DestImg.GetDataLineStart(y+Offset.y);

       Move(pointer(PtrInt(SrcLine) + 3*ClippedSrcRect.left)^,
            pointer(PtrInt(DstLine) + 3*(ClippedSrcRect.left + Offset.x))^,
            3*ClippedSrcRect.Width);
       end;
  ABitmap.LoadFromIntfImage(DestImg);
  DestImg.Destroy;
  SrcImg.Destroy;
end;
end;

class procedure TGUITools.CopyRoundCorner(ABuffer: TBitmap; ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Radius: integer; CornerPos: TCornerPos;
  Convex: boolean);

var BufferRect, BitmapRect : T2DIntRect;
    OrgSrcRect, OrgDstRect : T2DIntRect;
    SrcRect : T2DIntRect;
    Offset : T2DIntVector;
    Center: T2DIntVector;
    y: Integer;
    SrcImg: TLazIntfImage;
    DestImg: TLazIntfImage;
    SrcLine: Pointer;
    DstLine: Pointer;
    SrcPtr, DstPtr : PByte;
    x: Integer;
    Dist : double;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if Radius<1 then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$IFDEF EnhancedRecordSupport}
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Radius-1,
                                                   SrcPoint.y+Radius-1),
                                 OrgSrcRect)) then exit;

BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Radius-1,
                                                   DstPoint.y+Radius-1),
                                 OrgDstRect)) then exit;
{$ELSE}
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Radius-1,
                                                   SrcPoint.y+Radius-1),
                                 OrgSrcRect)) then exit;

BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Radius-1,
                                                   DstPoint.y+Radius-1),
                                 OrgDstRect)) then exit;
{$ENDIF}

Offset:=DstPoint - SrcPoint;

if not(OrgSrcRect.IntersectsWith(OrgDstRect - Offset, SrcRect)) then exit;

// Ustalamy pozycjê œrodka ³uku

{$IFDEF EnhancedRecordSupport}
case CornerPos of
     cpLeftTop: Center:=T2DIntVector.create(SrcPoint.x + radius - 1, SrcPoint.y + Radius - 1);
     cpRightTop: Center:=T2DIntVector.create(SrcPoint.x, SrcPoint.y + Radius - 1);
     cpLeftBottom: Center:=T2DIntVector.Create(SrcPoint.x + radius - 1, SrcPoint.y);
     cpRightBottom: Center:=T2DIntVector.Create(SrcPoint.x, SrcPoint.y);
end;
{$ELSE}
case CornerPos of
     cpLeftTop: Center.create(SrcPoint.x + radius - 1, SrcPoint.y + Radius - 1);
     cpRightTop: Center.create(SrcPoint.x, SrcPoint.y + Radius - 1);
     cpLeftBottom: Center.Create(SrcPoint.x + radius - 1, SrcPoint.y);
     cpRightBottom: Center.Create(SrcPoint.x, SrcPoint.y);
end;
{$ENDIF}

// Czy jest cokolwiek do przetworzenia?
if Convex then
   begin
   if (SrcRect.left<=SrcRect.right) and (SrcRect.top<=SrcRect.bottom) then
   begin
     SrcImg := ABuffer.CreateIntfImage;
     DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.top to SrcRect.bottom do
          begin
          SrcLine:=SrcImg.GetDataLineStart(y);
          DstLine:=DestImg.GetDataLineStart(y+Offset.y);

          SrcPtr:=pointer(PtrInt(SrcLine) + 3*SrcRect.left);
          DstPtr:=pointer(PtrInt(DstLine) + 3*(SrcRect.left + Offset.x));
          for x := SrcRect.left to SrcRect.right do
              begin
              {$IFDEF EnhancedRecordSupport}
              Dist:=Center.DistanceTo(T2DVector.create(x, y));
              {$ELSE}
              Dist:=Center.DistanceTo(x, y);
              {$ENDIF}
              if Dist <= (Radius-1) then
                 Move(SrcPtr^,DstPtr^,3);

              inc(SrcPtr,3);
              inc(DstPtr,3);
              end;
          end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
    end;
   end
else
   begin
   if (SrcRect.left<=SrcRect.right) and (SrcRect.top<=SrcRect.bottom) then
   begin
     SrcImg := ABuffer.CreateIntfImage;
     DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.top to SrcRect.bottom do
          begin
          SrcLine:=SrcImg.GetDataLineStart(y);
          DstLine:=DestImg.GetDataLineStart(y+Offset.y);

          SrcPtr:=pointer(PtrInt(SrcLine) + 3*SrcRect.left);
          DstPtr:=pointer(PtrInt(DstLine) + 3*(SrcRect.left + Offset.x));
          for x := SrcRect.left to SrcRect.right do
              begin
              {$IFDEF EnhancedRecordSupport}
              Dist:=Center.DistanceTo(T2DVector.create(x, y));
              {$ELSE}
              Dist:=Center.DistanceTo(x, y);
              {$ENDIF}
              if Dist >= (Radius-1) then
                 Move(SrcPtr^,DstPtr^,3);

              inc(SrcPtr,3);
              inc(DstPtr,3);
              end;
          end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
    end;
   end;
end;

class procedure TGUITools.DrawAARoundCorner(ABitmap: TBitmap; Point: T2DIntVector;
  Radius: integer; CornerPos: TCornerPos; Color: TColor);
var
  CornerRect: T2DIntRect;
  Center: T2DIntVector;
  Line: PByte;
  Ptr: PByte;
  colorR, colorG, colorB: byte;
  x, y: integer;
  RadiusDist: double;
  OrgCornerRect: T2DIntRect;
  BitmapRect: T2DIntRect;
  cr, cg, cb: Byte;
begin
  if ABitmap.PixelFormat <> pf24bit then
    raise Exception.Create('TSpkGUITools.DrawAARoundCorner: Bitmapa musi byæ w trybie 24-bitowym!');

  // Sprawdzamy poprawnoœæ
  if Radius < 1 then
    exit;
  if (ABitmap.Width=0) or (ABitmap.Height=0) then
    exit;

  {$IFDEF EnhancedRecordSupport}
  // �?ród³owy rect...
  OrgCornerRect := T2DIntRect.Create(Point.x,
                                     Point.y,
                                     Point.x + radius - 1,
                                     Point.y + radius - 1);

  // ...przycinamy do rozmiarów bitmapy
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ELSE}
  // �?ród³owy rect...
  OrgCornerRect.Create(Point.x,
                       Point.y,
                       Point.x + radius - 1,
                       Point.y + radius - 1);

  // ...przycinamy do rozmiarów bitmapy
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ENDIF}

  if not BitmapRect.intersectsWith(OrgCornerRect, CornerRect) then
    exit;

  // Jeœli nie ma czego rysowaæ, wychodzimy
  if (CornerRect.Left > CornerRect.Right) or (CornerRect.Top > CornerRect.Bottom) then
    exit;

  // Szukamy œrodka ³uku - zale¿nie od rodzaju naro¿nika
  {$IFDEF EnhancedRecordSupport}
  case CornerPos of
    cpLeftTop:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center := T2DIntVector.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center := T2DIntVector.Create(Point.x, Point.y);
  end;
  {$ELSE}
  case CornerPos of
    cpLeftTop:
      Center.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center.Create(Point.x, Point.y);
  end;
  {$ENDIF}

  Color := ColorToRGB(Color);

  colorR := GetRValue(Color);
  colorG := GetGValue(Color);
  colorB := GetBValue(Color);

  for y := CornerRect.Top to CornerRect.Bottom do
  begin
    for x := CornerRect.Left to CornerRect.Right do
    begin
      {$IFDEF EnhancedRecordSupport}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(T2DIntVector.create(x, y)));
      {$ELSE}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(x, y));
      {$ENDIF}
      if RadiusDist > 0 then
      begin
        RedGreenBlue(ColorToRGB(ABitmap.Canvas.Pixels[x,y]), cr, cg, cb);
        cb := round(cb + (ColorB - cb) * RadiusDist);
        cg := round(cg + (ColorG - cg) * RadiusDist);
        cr := round(cr + (ColorR - cr) * RadiusDist);
        ABitmap.Canvas.Pixels[x,y] := RGBToColor(cr,cg,cb);
      end;
    end;
  end;
end;

class procedure TGUITools.DrawAARoundCorner(ABitmap: TBitmap;
  Point: T2DIntVector; Radius: integer; CornerPos: TCornerPos; Color: TColor;
  ClipRect: T2DIntRect);
var
  CornerRect: T2DIntRect;
  Center: T2DIntVector;
  Line: PByte;
  Ptr: PByte;
  colorR, colorG, colorB: byte;
  x, y: integer;
  RadiusDist: double;
  OrgCornerRect: T2DIntRect;
  UnClippedCornerRect : T2DIntRect;
  BitmapRect: T2DIntRect;
  cr,cb,cg: byte;
begin
  if ABitmap.PixelFormat<>pf24bit then
    raise exception.create('TSpkGUITools.DrawAARoundCorner: Bitmapa musi byæ w trybie 24-bitowym!');

  // Sprawdzamy poprawnoœæ
  if Radius < 1 then
    exit;
  if (ABitmap.Width=0) or (ABitmap.Height=0) then
    exit;

  {$IFDEF EnhancedRecordSupport}
  // �?ród³owy rect...
  OrgCornerRect:=T2DIntRect.create(Point.x,
                                   Point.y,
                                   Point.x + radius - 1,
                                   Point.y + radius - 1);

  // ...przycinamy do rozmiarów bitmapy
  BitmapRect := T2DIntRect.create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ELSE}
  // �?ród³owy rect...
  OrgCornerRect.Create(Point.x,
                       Point.y,
                       Point.x + radius - 1,
                       Point.y + radius - 1);

  // ...przycinamy do rozmiarów bitmapy
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ENDIF}

  if not BitmapRect.IntersectsWith(OrgCornerRect, UnClippedCornerRect) then
    exit;

  // ClipRect
  if not UnClippedCornerRect.IntersectsWith(ClipRect, CornerRect) then
    exit;

  // Jeœli nie ma czego rysowaæ, wychodzimy
  if (CornerRect.Left > CornerRect.Right) or (CornerRect.Top > CornerRect.Bottom) then
    exit;

  // Szukamy œrodka ³uku - zale¿nie od rodzaju naro¿nika
  {$IFDEF EnhancedRecordSupport}
  case CornerPos of
    cpLeftTop:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center := T2DIntVector.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center := T2DIntVector.Create(Point.x, Point.y);
  end;
  {$ELSE}
  case CornerPos of
    cpLeftTop:
      Center.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center.Create(Point.x, Point.y);
  end;
  {$ENDIF}

  Color := ColorToRGB(Color);

  colorR := GetRValue(Color);
  colorG := GetGValue(Color);
  colorB := GetBValue(Color);

  for y := CornerRect.Top to CornerRect.Bottom do
  begin
    for x := CornerRect.Left to CornerRect.Right do
    begin
      {$IFDEF EnhancedRecordSupport}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(T2DIntVector.create(x, y)));
      {$ELSE}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(x, y));
      {$ENDIF}
      if RadiusDist > 0 then
      begin
        RedGreenBlue(ColorToRGB(ABitmap.Canvas.Pixels[x,y]), cr, cg, cb);
        cb := round(cb + (ColorB - cb) * RadiusDist);
        cg := round(cg + (ColorG - cg) * RadiusDist);
        cr := round(cr + (ColorR - cr) * RadiusDist);
        ABitmap.Canvas.Pixels[x,y] := RGBToColor(cr,cg,cb);
      end;
    end;
  end;
end;

class procedure TGUITools.DrawAARoundCorner(ACanvas: TCanvas;
  Point: T2DIntVector; Radius: integer; CornerPos: TCornerPos; Color: TColor);

var Center : T2DIntVector;
    OrgColor : TColor;
    x, y : integer;
    RadiusDist : double;
    CornerRect: T2DIntRect;

begin
// Sprawdzamy poprawnoœæ
if Radius<1 then
   exit;

{$IFDEF EnhancedRecordSupport}
// �?ród³owy rect...
CornerRect:=T2DIntRect.create(Point.x,
                              Point.y,
                              Point.x + radius - 1,
                              Point.y + radius - 1);

// Szukamy œrodka ³uku - zale¿nie od rodzaju naro¿nika
case CornerPos of
     cpLeftTop: Center:=T2DIntVector.create(Point.x + radius - 1, Point.y + Radius - 1);
     cpRightTop: Center:=T2DIntVector.create(Point.x, Point.y + Radius - 1);
     cpLeftBottom: Center:=T2DIntVector.Create(Point.x + radius - 1, Point.y);
     cpRightBottom: Center:=T2DIntVector.Create(Point.x, Point.y);
end;
{$ELSE}
// �?ród³owy rect...
CornerRect.create(Point.x,
                              Point.y,
                              Point.x + radius - 1,
                              Point.y + radius - 1);

// Szukamy œrodka ³uku - zale¿nie od rodzaju naro¿nika
case CornerPos of
     cpLeftTop: Center.create(Point.x + radius - 1, Point.y + Radius - 1);
     cpRightTop: Center.create(Point.x, Point.y + Radius - 1);
     cpLeftBottom: Center.Create(Point.x + radius - 1, Point.y);
     cpRightBottom: Center.Create(Point.x, Point.y);
end;
{$ENDIF}

Color:=ColorToRGB(Color);

for y := CornerRect.top to CornerRect.bottom do
    begin
    for x := CornerRect.left to CornerRect.right do
        begin
        {$IFDEF EnhancedRecordSupport}
        RadiusDist:=1 - abs((Radius - 1) - Center.DistanceTo(T2DIntVector.create(x, y)));
        {$ELSE}
        RadiusDist:=1 - abs((Radius - 1) - Center.DistanceTo(x, y));
        {$ENDIF}
        if RadiusDist>0 then
           begin
           OrgColor:=ACanvas.Pixels[x, y];
           ACanvas.Pixels[x, y]:=TColorTools.Shade(OrgColor, Color, RadiusDist);
           end;
        end;
    end;
end;

class procedure TGUITools.DrawAARoundCorner(ACanvas: TCanvas;
  Point: T2DIntVector; Radius: integer; CornerPos: TCornerPos; Color: TColor;
  ClipRect: T2DIntRect);

var UseOrgClipRgn : boolean;
    ClipRgn : HRGN;
    OrgRgn : HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

DrawAARoundCorner(ACanvas, Point, Radius, CornerPos, Color);

// Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawAARoundFrame(ABitmap: TBitmap; Rect: T2DIntRect;
  Radius: integer; Color: TColor; ClipRect: T2DIntRect);
begin
if ABitmap.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.DrawAARoundFrame: Bitmapa musi byæ w trybie 24-bitowym!');

if (Radius<1) then
   exit;

if (Radius>Rect.width div 2) or (Radius>Rect.height div 2) then
   exit;

// DrawAARoundCorner jest zabezpieczony przed rysowaniem poza obszarem
{$IFDEF EnhancedRecordSupport}
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.left, Rect.top), Radius, cpLeftTop, Color, ClipRect);
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.right - Radius + 1, Rect.top), Radius, cpRightTop, Color, ClipRect);
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.left, Rect.bottom - Radius + 1), Radius, cpLeftBottom, Color, ClipRect);
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color, ClipRect);
{$ELSE}
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.left, Rect.top), Radius, cpLeftTop, Color, ClipRect);
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.right - Radius + 1, Rect.top), Radius, cpRightTop, Color, ClipRect);
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.left, Rect.bottom - Radius + 1), Radius, cpLeftBottom, Color, ClipRect);
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color, ClipRect);
{$ENDIF}

ABitmap.Canvas.Pen.color:=Color;
ABitmap.Canvas.pen.style:=psSolid;

// Draw*Line s¹ zabezpieczone przed rysowaniem poza obszarem
DrawVLine(ABitmap, Rect.left, rect.top + Radius, rect.bottom - Radius, Color, ClipRect);
DrawVLine(ABitmap, Rect.right, rect.top + Radius, rect.bottom - Radius, Color, ClipRect);
DrawHLine(ABitmap, Rect.left + Radius, Rect.right - Radius, rect.top, Color, ClipRect);
DrawHLine(ABitmap, Rect.left + Radius, Rect.right - Radius, rect.bottom, Color, ClipRect);
end;

class procedure TGUITools.DrawAARoundFrame(ABitmap: TBitmap; Rect: T2DIntRect;
  Radius: integer; Color: TColor);
  
begin
if ABitmap.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.DrawAARoundFrame: Bitmapa musi byæ w trybie 24-bitowym!');

if (Radius<1) then
   exit;

if (Radius>Rect.width div 2) or (Radius>Rect.height div 2) then
   exit;

// DrawAARoundCorner jest zabezpieczony przed rysowaniem poza obszarem
{$IFDEF EnhancedRecordSupport}
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.left, Rect.top), Radius, cpLeftTop, Color);
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.right - Radius + 1, Rect.top), Radius, cpRightTop, Color);
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.left, Rect.bottom - Radius + 1), Radius, cpLeftBottom, Color);
DrawAARoundCorner(ABitmap, T2DIntVector.create(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
{$ELSE}
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.left, Rect.top), Radius, cpLeftTop, Color);
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.right - Radius + 1, Rect.top), Radius, cpRightTop, Color);
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.left, Rect.bottom - Radius + 1), Radius, cpLeftBottom, Color);
DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
{$ENDIF}

ABitmap.canvas.Pen.color:=Color;
ABitmap.canvas.pen.style:=psSolid;

// Draw*Line s¹ zabezpieczone przed rysowaniem poza obszarem
DrawVLine(ABitmap, Rect.left, rect.top + Radius, rect.bottom - Radius, Color);
DrawVLine(ABitmap, Rect.right, rect.top + Radius, rect.bottom - Radius, Color);
DrawHLine(ABitmap, Rect.left + Radius, Rect.right - Radius, rect.top, Color);
DrawHLine(ABitmap, Rect.left + Radius, Rect.right - Radius, rect.bottom, Color);
end;

class procedure TGUITools.DrawFitWText(ABitmap: TBitmap; x1, x2, y: integer;
  const AText: string; TextColor: TColor; Align : TAlignment);

var tw : integer;
    s : string;

begin
with ABitmap.Canvas do
     begin
     s:=AText;
     tw:=TextWidth(s);
     // Jeœli tekst siê zmieœci, rysujemy
     if tw<=(x2-x1+1) then
        case Align of
             taLeftJustify : TextOut(x1,y,AText);
             taRightJustify : TextOut(x2-tw+1,y,AText);
             taCenter : TextOut(x1 + ((x2-x1 - tw) div 2), y, AText);
        end
     else
        begin
        while (s<>'') and (tw>(x2-x1+1)) do
              begin
              delete(s,length(s),1);
              tw:=TextWidth(s+'...');
              end;
        if tw<=(x2-x1+1) then
           TextOut(x1, y, s+'...');
        end;
     end;
end;

class procedure TGUITools.DrawHLine(ACanvas: TCanvas; x1, x2, y: integer;
  Color: TColor);

var tmp : integer;

begin
if x2<x1 then
   begin
   tmp:=x1;
   x1:=x2;
   x2:=tmp;
   end;

ACanvas.pen.color:=Color;
ACanvas.moveto(x1, y);
ACanvas.lineto(x2+1,y);
end;

class procedure TGUITools.DrawHLine(ACanvas: TCanvas; x1, x2, y: integer;
  Color: TColor; ClipRect: T2DIntRect);

var UseOrgClipRgn : boolean;
    ClipRgn : HRGN;
    OrgRgn : HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

DrawHLine(ACanvas, x1, x2, y, Color);

// Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawImage(ABitmap: TBitmap; Imagelist: TImageList;
  ImageIndex: integer; Point : T2DIntVector; ClipRect: T2DIntRect);

begin
DrawImage(ABitmap.Canvas, ImageList, ImageIndex, Point, ClipRect);
end;

class procedure TGUITools.DrawImage(ABitmap: TBitmap; Imagelist: TImageList;
  ImageIndex: integer; Point: T2DIntVector);
begin
DrawImage(ABitmap.Canvas, ImageList, ImageIndex, Point);
end;

class procedure TGUITools.DrawImage(ACanvas: TCanvas; Imagelist: TImageList;
  ImageIndex: integer; Point : T2DIntVector; ClipRect: T2DIntRect);

var UseOrgClipRgn: Boolean;
    OrgRgn: HRGN;
    ClipRgn: HRGN;
    ImageIcon: TIcon;
    ImageBitmap: TBitmap;

begin
// Storing original ClipRgn and applying a new one
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

// avoid exclusive draw. draw with local canvas itself.
//ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex);
{$IfDef LCLWin32}
ImageIcon := TIcon.Create;
ImageList.GetIcon(ImageIndex, ImageIcon);
ACanvas.Draw(Point.x, Point.y, ImageIcon);
ImageIcon.Free;
{$Else}
ImageBitmap := TBitmap.Create;
ImageList.GetBitmap(ImageIndex, ImageBitmap);
ACanvas.Draw(Point.x, Point.y, ImageBitmap);
ImageBitmap.Free;
{$EndIf}

RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawMarkedText(ACanvas: TCanvas; x, y: integer; const AText,
  AMarkPhrase: string; TextColor : TColor; ClipRect: T2DIntRect; CaseSensitive: boolean);

var UseOrgClipRgn: Boolean;
    OrgRgn: HRGN;
    ClipRgn: HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

DrawMarkedText(ACanvas, x, y, AText, AMarkPhrase, TextColor, CaseSensitive);

RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawMarkedText(ACanvas: TCanvas; x, y: integer; const AText,
  AMarkPhrase: string; TextColor : TColor; CaseSensitive : boolean);

var TextToDraw : string;
    BaseText : string;
    MarkText : string;
    MarkPos: Integer;
    x1 : integer;
    s: string;
    MarkTextLength: Integer;

begin
TextToDraw:=AText;
if CaseSensitive then
   begin
   BaseText:=AText;
   MarkText:=AMarkPhrase;
   end
else
   begin
   BaseText:=AnsiUpperCase(AText);
   MarkText:=AnsiUpperCase(AMarkPhrase);
   end;

x1:=x;
MarkTextLength:=length(MarkText);

ACanvas.Font.Color:=TextColor;
ACanvas.Brush.Style:=bsClear;

MarkPos:=pos(MarkText, BaseText);
while MarkPos>0 do
      begin
      if MarkPos>1 then
         begin
         // Rysowanie tekstu przed wyró¿nionym
         ACanvas.Font.Style:=ACanvas.Font.Style - [fsBold];
         s:=copy(TextToDraw, 1, MarkPos-1);

         ACanvas.TextOut(x1, y, s);
         inc(x1, ACanvas.TextWidth(s)+1);

         delete(TextToDraw, 1, MarkPos-1);
         delete(BaseText, 1, MarkPos-1);
         end;

      // Rysowanie wyró¿nionego tekstu
      ACanvas.Font.Style:=ACanvas.Font.Style + [fsBold];
      s:=copy(TextToDraw, 1, MarkTextLength);

      ACanvas.TextOut(x1, y, s);
      inc(x1, ACanvas.TextWidth(s)+1);

      delete(TextToDraw, 1, MarkTextLength);
      delete(BaseText, 1, MarkTextLength);

      MarkPos:=pos(MarkText, BaseText);
      end;

if Length(BaseText)>0 then
   begin
   ACanvas.Font.Style:=ACanvas.Font.Style - [fsBold];
   ACanvas.TextOut(x1, y, TextToDraw);
   end;
end;

class procedure TGUITools.DrawImage(ACanvas: TCanvas; Imagelist: TImageList;
  ImageIndex: integer; Point: T2DIntVector);
begin
ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex);
end;

class procedure TGUITools.DrawOutlinedText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor);
begin
with Acanvas do
     begin
     brush.style:=bsClear;
     font.color:=OutlineColor;
     TextOut(x-1, y-1, AText);
     TextOut(x, y-1, AText);
     TextOut(x+1, y-1, AText);
     TextOut(x-1, y, AText);
     TextOut(x+1, y, AText);
     TextOut(x-1, y+1, AText);
     TextOut(x, y+1, AText);
     TextOut(x+1, y+1, AText);

     font.color:=TextColor;
     TextOut(x, y, AText);
     end;
end;

class procedure TGUITools.DrawOutlinedText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor; ClipRect: T2DIntRect);

var WinAPIClipRect : TRect;

begin
WinAPIClipRect:=ClipRect.ForWinAPI;
with ACanvas do
     begin
     brush.style:=bsClear;
     font.color:=OutlineColor;
     TextRect(WinAPIClipRect, x-1, y-1, AText);
     TextRect(WinAPIClipRect, x, y-1, AText);
     TextRect(WinAPIClipRect, x+1, y-1, AText);
     TextRect(WinAPIClipRect, x-1, y, AText);
     TextRect(WinAPIClipRect, x+1, y, AText);
     TextRect(WinAPIClipRect, x-1, y+1, AText);
     TextRect(WinAPIClipRect, x, y+1, AText);
     TextRect(WinAPIClipRect, x+1, y+1, AText);

     font.color:=TextColor;
     TextRect(WinAPIClipRect, x, y, AText);
     end;
end;

class procedure TGUITools.DrawHLine(ABitmap: TBitmap; x1, x2, y: integer;
  Color: TColor);

var LineRect : T2DIntRect;
    BitmapRect : T2DIntRect;
  tmp: Integer;

begin
if ABitmap.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.DrawHLine: Bitmapa musi byæ w trybie 24-bitowym!');

if x2<x1 then
   begin
   tmp:=x1;
   x1:=x2;
   x2:=tmp;
   end;

{$IFDEF EnhancedRecordSupport}
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(x1, y, x2, y), LineRect)) then
   exit;
{$ELSE}
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(x1, y, x2, y), LineRect)) then
   exit;
{$ENDIF}

ABitmap.canvas.pen.color:=Color;
ABitmap.canvas.pen.style:=psSolid;
ABitmap.canvas.moveto(LineRect.left, LineRect.Top);
ABitmap.canvas.lineto(LineRect.right+1, Linerect.top);
end;

class procedure TGUITools.DrawHLine(ABitmap: TBitmap; x1, x2, y: integer;
  Color: TColor; ClipRect: T2DIntRect);

var OrgLineRect : T2DIntRect;
    LineRect : T2DIntRect;
    BitmapRect : T2DIntRect;
  tmp: Integer;

begin
if ABitmap.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.DrawHLine: Bitmapa musi byæ w trybie 24-bitowym!');

if x2<x1 then
   begin
   tmp:=x1;
   x1:=x2;
   x2:=tmp;
   end;

{$IFDEF EnhancedRecordSupport}
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(x1, y, x2, y), OrgLineRect)) then
   exit;
{$ELSE}
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(x1, y, x2, y), OrgLineRect)) then
   exit;
{$ENDIF}

if not(OrgLineRect.IntersectsWith(ClipRect, LineRect)) then
   exit;

ABitmap.canvas.pen.color:=Color;
ABitmap.canvas.pen.style:=psSolid;
ABitmap.canvas.moveto(LineRect.left, LineRect.Top);
ABitmap.canvas.lineto(LineRect.right+1, Linerect.top);
end;

class procedure TGUITools.DrawOutlinedText(ABitmap: TBitmap; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor; ClipRect: T2DIntRect);

var WinAPIClipRect : TRect;

begin
WinAPIClipRect:=ClipRect.ForWinAPI;
with ABitmap.canvas do
     begin
     brush.style:=bsClear;
     font.color:=OutlineColor;
     TextRect(WinAPIClipRect, x-1, y-1, AText);
     TextRect(WinAPIClipRect, x, y-1, AText);
     TextRect(WinAPIClipRect, x+1, y-1, AText);
     TextRect(WinAPIClipRect, x-1, y, AText);
     TextRect(WinAPIClipRect, x+1, y, AText);
     TextRect(WinAPIClipRect, x-1, y+1, AText);
     TextRect(WinAPIClipRect, x, y+1, AText);
     TextRect(WinAPIClipRect, x+1, y+1, AText);

     font.color:=TextColor;
     TextRect(WinAPIClipRect, x, y, AText);
     end;
end;

class procedure TGUITools.DrawRegion(ACanvas: TCanvas; Region: HRGN; Rect : T2DIntRect; ColorFrom,
  ColorTo: TColor; GradientKind: TBackgroundKind);

var UseOrgClipRgn: Boolean;
    OrgRgn: HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
SelectClipRgn(ACanvas.Handle, Region);

FillGradientRectangle(ACanvas, Rect, ColorFrom, ColorTo, GradientKind);

RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
end;

class procedure TGUITools.DrawRegion(ACanvas: TCanvas; Region: HRGN; Rect : T2DIntRect; ColorFrom,
  ColorTo: TColor; GradientKind: TBackgroundKind; ClipRect: T2DIntRect);

var UseOrgClipRgn : boolean;
    ClipRgn : HRGN;
    OrgRgn : HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

DrawRegion(ACanvas, Region, Rect, ColorFrom, ColorTo, GradientKind);

// Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawRoundRect(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind;
  ClipRect: T2DIntRect; LeftTopRound, RightTopRound, LeftBottomRound,
  RightBottomRound: boolean);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawRoundRect(ACanvas, Rect, Radius, ColorFrom, ColorTo, GradientKind, LeftTopRound, RightTopRound, LeftBottomRound, RightBottomRound);

  // Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor: TColor);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Color := TextColor;
    TextOut(x, y, AText);
  end;
end;

class procedure TGUITools.DrawText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor: TColor; ClipRect: T2DIntRect);

var WinAPIClipRect : TRect;

begin
WinAPIClipRect:=ClipRect.ForWinAPI;
with Acanvas do
     begin
     brush.style:=bsClear;
     font.color:=TextColor;
     TextRect(WinAPIClipRect, x, y, AText);
     end;
end;

class procedure TGUITools.DrawRoundRect(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind;
  LeftTopRound, RightTopRound, LeftBottomRound, RightBottomRound: boolean);
var
  RoundRgn: HRGN;
  TmpRgn: HRGN;
  OrgRgn: HRGN;
  UseOrgClipRgn: Boolean;
begin
  if Radius < 0 then
    exit;

  if Radius > 0 then
  begin
    //WriteLn('Radius: ', Radius, ' Rect.Width: ', Rect.Width, ' Rect.Height: ', Rect.Height);

    // There's a bug in fpc that evaluates the expression below erroneous when using inline
    // Radius = 3 and Rect.Width >= 128 and <= 261 will evaluate to true
    {$ifdef FpcBugWorkAround}
    if (CompareValue(Radius*2, Rect.width) > 0) and (CompareValue(Radius*2, Rect.Height) > 0) then
      exit;
    {$else}
    if (Radius*2 > Rect.Width) or (Radius*2 > Rect.Height) then
      exit;
    {$endif}

    // Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
    SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

    if not(LeftTopRound) and
       not(RightTopRound) and
       not(LeftBottomRound) and
       not (RightBottomRound) then
    begin
      RoundRgn := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right + 1, Rect.Bottom + 1);
    end
    else
    begin
      RoundRgn := CreateRoundRectRgn(Rect.Left, Rect.Top, Rect.Right +2, Rect.Bottom + 2, Radius*2, Radius*2);

      if not(LeftTopRound) then
      begin
        TmpRgn := CreateRectRgn(Rect.Left, Rect.Top, Rect.Left + Radius, Rect.Top + Radius);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;

      if not(RightTopRound) then
      begin
        TmpRgn := CreateRectRgn(Rect.Right - Radius + 1, Rect.Top, Rect.Right + 1, Rect.Top + Radius);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;

      if not(LeftBottomRound) then
      begin
        TmpRgn := CreateRectRgn(Rect.Left, Rect.Bottom - Radius + 1, Rect.Left + Radius, Rect.Bottom + 1);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;

      if not(RightBottomRound) then
      begin
        TmpRgn := CreateRectRgn(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1, Rect.Right + 1, Rect.Bottom + 1);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;
    end;

    if UseOrgClipRgn then
      CombineRgn(RoundRgn, RoundRgn, OrgRgn, RGN_AND);

    SelectClipRgn(ACanvas.Handle, RoundRgn);
  end;  // if Radius > 0

  ColorFrom := ColorToRGB(ColorFrom);
  ColorTo := ColorToRGB(ColorTo);

  FillGradientRectangle(ACanvas, Rect, ColorFrom, ColorTo, GradientKind);

  if Radius > 0 then
  begin
    // Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
    RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
    DeleteObject(RoundRgn);
  end;
end;

class procedure TGUITools.DrawOutlinedText(ABitmap: TBitmap; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor);
begin
with ABitmap.canvas do
     begin
     brush.style:=bsClear;
     font.color:=OutlineColor;
     TextOut(x-1, y-1, AText);
     TextOut(x, y-1, AText);
     TextOut(x+1, y-1, AText);
     TextOut(x-1, y, AText);
     TextOut(x+1, y, AText);
     TextOut(x-1, y+1, AText);
     TextOut(x, y+1, AText);
     TextOut(x+1, y+1, AText);

     font.color:=TextColor;
     TextOut(x, y, AText);
     end;
end;

class procedure TGUITools.DrawText(ABitmap: TBitmap; x, y: integer;
  const AText: string; TextColor: TColor; ClipRect: T2DIntRect);

var WinAPIClipRect : TRect;

begin
WinAPIClipRect:=ClipRect.ForWinAPI;
with ABitmap.canvas do
     begin
     brush.style:=bsClear;
     font.color:=TextColor;
     TextRect(WinAPIClipRect, x, y, AText);
     end;
end;

class procedure TGUITools.DrawFitWOutlinedText(ABitmap: TBitmap; x1, x2, y: integer;
  const AText: string; TextColor, OutlineColor: TColor; Align : TAlignment);

var tw : integer;
    s : string;

begin
with ABitmap.Canvas do
     begin
     s:=AText;
     tw:=TextWidth(s) + 2;
     // Jeœli tekst siê zmieœci, rysujemy
     if tw<=(x2-x1+1) then
        case Align of
             taLeftJustify : TGUITools.DrawOutlinedText(ABitmap,x1, y, AText, TextColor, OutlineColor);
             taRightJustify : TGUITools.DrawOutlinedText(ABitmap,x2-tw+1, y, AText, TextColor, OutlineColor);
             taCenter : TGUITools.DrawOutlinedText(ABitmap,x1 + ((x2-x1 - tw) div 2), y, AText, TextColor, OutlineColor);
        end
     else
        begin
        while (s<>'') and (tw>(x2-x1+1)) do
              begin
              delete(s,length(s),1);
              tw:=TextWidth(s+'...')+2;
              end;
        if tw<=(x2-x1+1) then
           TGUITools.DrawOutlinedText(ABitmap, x1, y, s+'...', TextColor, OutlineColor);
        end;
     end;
end;

class procedure TGUITools.DrawFitWOutlinedText(ACanvas: TCanvas; x1, x2,
  y: integer; const AText: string; TextColor, OutlineColor: TColor;
  Align: TAlignment);

var tw : integer;
    s : string;

begin
with ACanvas do
     begin
     s:=AText;
     tw:=TextWidth(s) + 2;
     // Jeœli tekst siê zmieœci, rysujemy
     if tw<=(x2-x1+1) then
        case Align of
             taLeftJustify : TGUITools.DrawOutlinedText(ACanvas,x1, y, AText, TextColor, OutlineColor);
             taRightJustify : TGUITools.DrawOutlinedText(ACanvas,x2-tw+1, y, AText, TextColor, OutlineColor);
             taCenter : TGUITools.DrawOutlinedText(ACanvas,x1 + ((x2-x1 - tw) div 2), y, AText, TextColor, OutlineColor);
        end
     else
        begin
        while (s<>'') and (tw>(x2-x1+1)) do
              begin
              delete(s,length(s),1);
              tw:=TextWidth(s+'...')+2;
              end;
        if tw<=(x2-x1+1) then
           TGUITools.DrawOutlinedText(ACanvas, x1, y, s+'...', TextColor, OutlineColor);
        end;
     end;
end;

class procedure TGUITools.FillGradientRectangle(ACanvas: TCanvas; Rect: T2DIntRect; ColorFrom: TColor; ColorTo: TColor; GradientKind: TBackgroundKind);
var
  Mesh: array of GRADIENTRECT;
  GradientVertice: array of TRIVERTEX;
  ConcaveColor: TColor;
begin
  case GradientKind of
    bkSolid:
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.brush.color := ColorFrom;
        ACanvas.fillrect(Rect.ForWinAPI);
      end;
    bkVerticalGradient, bkHorizontalGradient:
      begin
        setlength(GradientVertice, 2);
        with GradientVertice[0] do
        begin
          x := Rect.left;
          y := Rect.top;
          Red := GetRValue(ColorFrom) shl 8;
          Green := GetGValue(ColorFrom) shl 8;
          Blue := GetBValue(ColorFrom) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[1] do
        begin
          x := Rect.Right + 1;
          y := Rect.bottom + 1;
          Red := GetRValue(ColorTo) shl 8;
          Green := GetGValue(ColorTo) shl 8;
          Blue := GetBValue(ColorTo) shl 8;
          Alpha := 255 shl 8;
        end;
        setlength(Mesh, 1);
        Mesh[0].UpperLeft := 0;
        Mesh[0].LowerRight := 1;
        if GradientKind = bkVerticalGradient then
          GradientFill(ACanvas.Handle, @GradientVertice[0], 2, @Mesh[0], 1, GRADIENT_FILL_RECT_V)
        else
          GradientFill(ACanvas.Handle, @GradientVertice[0], 2, @Mesh[0], 1, GRADIENT_FILL_RECT_H);
      end;
    bkConcave:
      begin
        ConcaveColor:=TColorTools.Brighten(ColorFrom, 20);

        setlength(GradientVertice, 4);
        with GradientVertice[0] do
        begin
          x := Rect.left;
          y := Rect.top;
          Red := GetRValue(ColorFrom) shl 8;
          Green := GetGValue(ColorFrom) shl 8;
          Blue := GetBValue(ColorFrom) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[1] do
        begin
          x := Rect.Right + 1;
          y := Rect.Top + (Rect.height) div 4;
          Red := GetRValue(ConcaveColor) shl 8;
          Green := GetGValue(ConcaveColor) shl 8;
          Blue := GetBValue(ConcaveColor) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[2] do
        begin
          x := Rect.left;
          y := Rect.Top + (Rect.height) div 4;
          Red := GetRValue(ColorTo) shl 8;
          Green := GetGValue(ColorTo) shl 8;
          Blue := GetBValue(ColorTo) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[3] do
        begin
          x := Rect.Right + 1;
          y := Rect.bottom + 1;
          Red := GetRValue(ColorFrom) shl 8;
          Green := GetGValue(ColorFrom) shl 8;
          Blue := GetBValue(ColorFrom) shl 8;
          Alpha := 255 shl 8;
        end;
        setlength(Mesh, 2);
        Mesh[0].UpperLeft := 0;
        Mesh[0].LowerRight := 1;
        Mesh[1].UpperLeft := 2;
        Mesh[1].LowerRight := 3;
        GradientFill(ACanvas.Handle, @GradientVertice[0], 4, @Mesh[0], 2, GRADIENT_FILL_RECT_V);
      end;
  end;
end;

class procedure TGUITools.DrawFitWText(ACanvas: TCanvas; x1, x2, y: integer;
  const AText: string; TextColor: TColor; Align: TAlignment);

var tw : integer;
    s : string;

begin
with ACanvas do
     begin
     s:=AText;
     tw:=TextWidth(s);
     // Jeœli tekst siê zmieœci, rysujemy
     if tw<=(x2-x1+1) then
        case Align of
             taLeftJustify : TextOut(x1,y,AText);
             taRightJustify : TextOut(x2-tw+1,y,AText);
             taCenter : TextOut(x1 + ((x2-x1 - tw) div 2), y, AText);
        end
     else
        begin
        while (s<>'') and (tw>(x2-x1+1)) do
              begin
              delete(s,length(s),1);
              tw:=TextWidth(s+'...');
              end;
        if tw<=(x2-x1+1) then
           TextOut(x1, y, s+'...');
        end;
     end;
end;

class procedure TGUITools.RenderBackground(ABuffer: TBitmap;
  Rect: T2DIntRect; Color1, Color2: TColor; BackgroundKind: TBackgroundKind);

var TempRect : T2DIntRect;

begin
if ABuffer.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.RenderBackground: Bitmapa musi byæ w trybie 24-bitowym!');
if (rect.left>rect.right) or (rect.top>rect.bottom) then
   exit;

// Zarówno metoda FillRect jak i WinAPI'owe rysowanie gradientów jest
// zabezpieczone przed rysowaniem poza obszarem p³ótna.
case BackgroundKind of
     bkSolid: begin
              ABuffer.Canvas.brush.Color:=Color1;
              ABuffer.Canvas.brush.style:=bsSolid;
              ABuffer.Canvas.Fillrect(Rect.ForWinAPI);
              end;
     bkVerticalGradient: begin
                         TGradientTools.VGradient(ABuffer.canvas, Color1, Color2, Rect.ForWinAPI);
                         end;
     bkHorizontalGradient: begin
                           TGradientTools.HGradient(ABuffer.canvas, Color1, Color2, Rect.ForWinAPI);
                           end;
     bkConcave: begin
                {$IFDEF EnhancedRecordSupport}
                TempRect:=T2DIntRect.create(rect.Left,
                                            rect.top,
                                            rect.right,
                                            rect.Top + (rect.bottom - rect.top) div 4);
                {$ELSE}
                TempRect.create(rect.Left,
                                            rect.top,
                                            rect.right,
                                            rect.Top + (rect.bottom - rect.top) div 4);
                {$ENDIF}
                TGradientTools.VGradient(ABuffer.Canvas,
                                         Color1,
                                         TColorTools.Shade(Color1, Color2, 20),
                                         TempRect.ForWinAPI
                                         );

                {$IFDEF EnhancedRecordSupport}
                TempRect:=T2DIntRect.create(rect.Left,
                                            rect.top + (rect.bottom - rect.top) div 4 + 1,
                                            rect.right,
                                            rect.bottom);
                {$ELSE}
                TempRect.create(rect.Left,
                                            rect.top + (rect.bottom - rect.top) div 4 + 1,
                                            rect.right,
                                            rect.bottom);
                {$ENDIF}
                TGradientTools.VGradient(ABuffer.Canvas,
                                         Color2,
                                         Color1,
                                         TempRect.ForWinAPI
                                         );
                end;
end;

end;

class procedure TGUITools.RestoreClipRgn(DC: HDC; OrgRgnExists: boolean;
  var OrgRgn: HRGN);

begin
if OrgRgnExists then
   SelectClipRgn(DC, OrgRgn) else
   SelectClipRgn(DC, 0);
DeleteObject(OrgRgn);
end;

class procedure TGUITools.SaveClipRgn(DC: HDC; var OrgRgnExists: boolean;
  var OrgRgn: HRGN);

var i : integer;

begin
OrgRgn:=CreateRectRgn(0, 0, 1, 1);
i:=GetClipRgn(DC, OrgRgn);
OrgRgnExists:=(i=1);
end;

class procedure TGUITools.DrawText(ABitmap: TBitmap; x, y: integer; const AText: string;
  TextColor: TColor);
begin
with ABitmap.canvas do
     begin
     brush.style:=bsClear;
     font.color:=TextColor;
     TextOut(x, y, AText);
     end;
end;

class procedure TGUITools.DrawVLine(ABitmap: TBitmap; x, y1, y2: integer;
  Color: TColor);

var LineRect : T2DIntRect;
    BitmapRect : T2DIntRect;
  tmp: Integer;

begin
if ABitmap.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.DrawHLine: Bitmapa musi byæ w trybie 24-bitowym!');

if y2<y1 then
   begin
   tmp:=y1;
   y1:=y2;
   y2:=tmp;
   end;

{$IFDEF EnhancedRecordSupport}
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(x, y1, x, y2), LineRect)) then
   exit;
{$ELSE}
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(x, y1, x, y2), LineRect)) then
   exit;
{$ENDIF}

ABitmap.canvas.pen.color:=Color;
ABitmap.canvas.pen.style:=psSolid;
ABitmap.canvas.moveto(LineRect.left, LineRect.Top);
ABitmap.canvas.lineto(LineRect.left, Linerect.bottom+1);
end;

class procedure TGUITools.DrawVLine(ABitmap: TBitmap; x, y1, y2: integer;
  Color: TColor; ClipRect: T2DIntRect);

var OrgLineRect : T2DIntRect;
    LineRect : T2DIntRect;
    BitmapRect : T2DIntRect;
  tmp: Integer;

begin
if ABitmap.PixelFormat<>pf24bit then
   raise exception.create('TGUITools.DrawHLine: Bitmapa musi byæ w trybie 24-bitowym!');

if y2<y1 then
   begin
   tmp:=y1;
   y1:=y2;
   y2:=tmp;
   end;

{$IFDEF EnhancedRecordSupport}
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(x, y1, x, y2), OrgLineRect)) then
   exit;
{$ELSE}
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(x, y1, x, y2), OrgLineRect)) then
   exit;
{$ENDIF}

if not(OrgLineRect.IntersectsWith(ClipRect, LineRect)) then
   exit;

ABitmap.canvas.pen.color:=Color;
ABitmap.canvas.pen.style:=psSolid;
ABitmap.canvas.moveto(LineRect.left, LineRect.Top);
ABitmap.canvas.lineto(LineRect.left, Linerect.bottom+1);
end;

class procedure TGUITools.DrawVLine(ACanvas: TCanvas; x, y1, y2: integer;
  Color: TColor);

var tmp : integer;

begin
if y2<y1 then
   begin
   tmp:=y1;
   y1:=y2;
   y2:=tmp;
   end;

ACanvas.pen.color:=Color;
ACanvas.moveto(x, y1);
ACanvas.lineto(x, y2+1);
end;

class procedure TGUITools.DrawVLine(ACanvas: TCanvas; x, y1, y2: integer;
  Color: TColor; ClipRect: T2DIntRect);

var UseOrgClipRgn : boolean;
    ClipRgn : HRGN;
    OrgRgn : HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

DrawVLine(ACanvas, x, y1, y2, Color);

// Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawAARoundFrame(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; Color: TColor);

begin
if (Radius<1) then
   exit;

if (Radius>Rect.width div 2) or (Radius>Rect.height div 2) then
   exit;

// DrawAARoundCorner jest zabezpieczony przed rysowaniem poza obszarem
{$IFDEF EnhancedRecordSupport}
DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.left, Rect.top), Radius, cpLeftTop, Color);
DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.right - Radius + 1, Rect.top), Radius, cpRightTop, Color);
DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.left, Rect.bottom - Radius + 1), Radius, cpLeftBottom, Color);
DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
{$ELSE}
DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.left, Rect.top), Radius, cpLeftTop, Color);
DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.right - Radius + 1, Rect.top), Radius, cpRightTop, Color);
DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.left, Rect.bottom - Radius + 1), Radius, cpLeftBottom, Color);
DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
{$ENDIF}

ACanvas.Pen.color:=Color;
ACanvas.pen.style:=psSolid;

// Draw*Line s¹ zabezpieczone przed rysowaniem poza obszarem
DrawVLine(ACanvas, Rect.left, rect.top + Radius, rect.bottom - Radius, Color);
DrawVLine(ACanvas, Rect.right, rect.top + Radius, rect.bottom - Radius, Color);
DrawHLine(ACanvas, Rect.left + Radius, Rect.right - Radius, rect.top, Color);
DrawHLine(ACanvas, Rect.left + Radius, Rect.right - Radius, rect.bottom, Color);
end;

class procedure TGUITools.DrawAARoundFrame(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; Color: TColor; ClipRect: T2DIntRect);

var UseOrgClipRgn : boolean;
    ClipRgn : HRGN;
    OrgRgn : HRGN;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

DrawAARoundFrame(ACanvas, Rect, Radius, Color);

// Przywracanie poprzedniego ClipRgn i usuwanie wykorzystanych regionów
RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawDisabledImage(ABitmap: TBitmap;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector;
  ClipRect: T2DIntRect);
begin
DrawDisabledImage(ABitmap.Canvas, ImageList, ImageIndex, Point, ClipRect);
end;

class procedure TGUITools.DrawDisabledImage(ABitmap: TBitmap;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector);
begin
DrawDisabledImage(ABitmap.Canvas, ImageList, ImageIndex, Point);
end;

class procedure TGUITools.DrawDisabledImage(ACanvas: TCanvas;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector;
  ClipRect: T2DIntRect);

var UseOrgClipRgn: Boolean;
    OrgRgn: HRGN;
    ClipRgn: HRGN;
    DCStackPos : integer;

begin
// Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

ClipRgn:=CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
if UseOrgClipRgn then
   CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

SelectClipRgn(ACanvas.Handle, ClipRgn);

// Hack poprawiaj¹cy b³¹d w ImageList.Draw, który nie przywraca poprzedniego
// koloru czcionki dla p³ótna
DcStackPos:=SaveDC(ACanvas.Handle);
ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex, false);
RestoreDC(ACanvas.Handle, DcStackPos);

RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawDisabledImage(ACanvas: TCanvas;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector);

var DCStackPos : integer;

begin
//todo: see if is necessary to save the DC
DcStackPos:=SaveDC(ACanvas.Handle);
ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex, false);
RestoreDC(ACanvas.Handle, DcStackPos);
end;

class procedure TGUITools.DrawCheckbox(ACanvas:TCanvas; x,y: Integer;
  AState: TCheckboxState; ACheckboxState:TSpkCheckboxState;
  AStyle: TSpkCheckboxStyle; ClipRect:T2DIntRect);
var
  UseOrgClipRgn: Boolean;
  OrgRgn: HRGN;
  ClipRgn: HRGN;
  te: TThemedElementDetails;
  Rect: TRect;
begin
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);
  SelectClipRgn(ACanvas.Handle, ClipRgn);
  DrawCheckbox(ACanvas, x,y, AState, ACheckboxState, AStyle);
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawCheckbox(ACanvas: TCanvas; x,y: Integer;
  AState: TCheckboxState; ACheckboxState: TSpkCheckboxState;
  AStyle:TSpkCheckboxStyle);
const
  UNTHEMED_FLAGS: array [TSpkCheckboxStyle, TCheckboxState] of Integer = (
    (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED, DFCS_BUTTONCHECK or DFCS_BUTTON3STATE),
    (DFCS_BUTTONRADIO, DFCS_BUTTONRADIO or DFCS_CHECKED, DFCS_BUTTONRADIO or DFCS_BUTTON3STATE)
  );
  THEMED_FLAGS: array [TSpkCheckboxStyle, TCheckboxState, TSpkCheckboxState] of TThemedButton = (
    ( (tbCheckboxUncheckedNormal, tbCheckboxUncheckedHot, tbCheckboxUncheckedPressed, tbCheckboxUncheckedDisabled),
      (tbCheckboxCheckedNormal, tbCheckboxCheckedHot, tbCheckboxCheckedPressed, tbCheckboxCheckedDisabled),
      (tbCheckboxMixedNormal, tbCheckboxMixedHot, tbCheckboxMixedPressed, tbCheckboxMixedDisabled)
    ),
    ( (tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled),
      (tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled),
      (tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled)
    )
  );
var
  R: TRect;
  w: Integer;
  sz: TSize;
  te: TThemedElementDetails;
begin
  if ThemeServices.ThemesEnabled then begin
    te := ThemeServices.GetElementDetails(THEMED_FLAGS[AStyle, AState, ACheckboxState]);
    sz := ThemeServices.GetDetailSize(te);
    R := Bounds(x, y, sz.cx, sz.cy);
    InflateRect(R, 1, 1);
    ThemeServices.DrawElement(ACanvas.Handle, te, R);
  end else begin
    w := GetSystemMetrics(SM_CYMENUCHECK);
    R := Bounds(x, y, w, w);
    DrawFrameControl(
      ACanvas.Handle, R, DFC_BUTTON, UNTHEMED_FLAGS[AStyle, AState]);
  end;
end;

end.
