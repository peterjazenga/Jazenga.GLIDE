
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Controls;

{$MODE Delphi}

interface

uses

  LCLIntf, LCLType,
  Messages,LMessages,
  SysUtils, Classes, 
  Graphics, Controls
  , Forms
  , Menus
  , GR32
  , GR32_Blend
  , GR32_Filters
  , GR32_Resamplers
  , XGR32_BitmapEx
  , XGR32_Graphics
  , XGR32_GraphUtils
  , XGR32_FilterEx
  ;

type
  TControlAccess = class(TControl);
  TParentControl = class(TWinControl);
  TMouseButtons = set of TMouseButton;

type
  TPanelBevel = TBevelCut;
  PEraseBackgroundInfo = ^TEraseBackgroundInfo;
  TGRCustomControl = class;
  TGRGraphicControl = class;
  TGRBGCustomControl = class;
  TGRBGGraphicControl = class;
  TGRStyleController = class;
  TGRPaintEvent = procedure (Sender: TObject; Canvas: TCanvas) of object;
  TEraseBackgroundInfo = packed record
    { Summary Rect }
    Rect: TRect;
  end;

  { Summary the abstract Graphic32 WinControl. }
  { Description
  }
  TGRCustomControl = class(TCustomControl)
  private
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FDragAsTitle: Boolean;
    FFastDraw: Boolean;
    FFrame: TGRFrame;
    FFrameHot: TGRFrame;
    FMouseButtons: TMouseButtons;
    FMouseInControl: Boolean;
    FMouseXPos: Integer;
    FMouseYPos: Integer;
    FOnChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TGRPaintEvent;
    FShiftState: TShiftState;
    FStyleController: TGRStyleController;
    FTransparent: Boolean;
    FUpdateCount: Integer;
    function GetColor: TColor;
    function GetMousePosition: TPoint;
    procedure SetCaptionFont(const Value: TFont32);
    procedure SetFastDraw(const Value: Boolean);
    procedure SetFrame(const Value: TGRFrame);
    procedure SetFrameHot(const Value: TGRFrame);
    procedure SetStyleController(const Value: TGRStyleController);
  protected

    FBuffer: TBitmap32;

    FBufferDirty: Boolean;
    FCaptionFont: TFont32;
    FSelfBuffer: TBItmap32;

    FSelfBufferDirty: Boolean;
    procedure AfterPaintBuffer(aBitmap32: TBitmap32); virtual;

    procedure BeforePaintBuffer(aBitmap32: TBitmap32); virtual;
    procedure ClearDefaultPopupMenu(const aPopupMenu: TPopupMenu); dynamic;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentFontChanged(var Message: TMessage); message  CM_PARENTFONTCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoDefaultPopupMenu(const PopupMenu: TPopupMenu); dynamic;
    procedure DoFrameChanged(Sender: TObject); dynamic;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure FontChanged(Sender: TObject); virtual;
    function GetPopupMenu: TPopupMenu; override;
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; virtual;
    function IsStyleEnabled: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Paint; override;
    procedure PaintParentBackground(aBitmap32: TBitmap32); overload; virtual;
    procedure SetAlphaBlend(const Value: Boolean); virtual;
    procedure SetAlphaBlendValue(const Value: Byte); virtual;
    procedure SetTransparent(const Value: Boolean);
    procedure UpdateTracking;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TLMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    //procedure WMSetText(var Message: TMessage); message CM_SETTEXT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WndProc(var Message: TMessage); override;
    property CaptionFont: TFont32 read FCaptionFont write SetCaptionFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetBorderHeight: Integer;
    function GetBorderWidth: Integer;
    function GetBottomBorderSize: Integer;
    { Summary return the current paint Frame. }
    function GetCurrentStyle: TGRStyle; virtual;
    function GetLeftBorderSize: Integer;
    { Summary return the current paint Frame. }
    function GetPaintFrame: TGRFrame; virtual;
    function GetRightBorderSize: Integer;
    function GetTopBorderSize: Integer;
    procedure Invalidate; override;
    procedure InvalidateBuffer;
    procedure InvalidateChild(const aChild: TControl);
    procedure InvalidateSelfBuffer;
    function IsBufferDirty: Boolean;
    procedure PaintBuffer(NeedParentBackground: Boolean = True);
    { Summary paint the all children on the control. }
    { Description
    aBuffer.Size Must >= ClientRect.
    }
    procedure PaintChildsTo(aBuffer: TBitmap32; const aPaintAll: Boolean =
      False); overload;
    { Summary paint the children in the aBuffer's srcRect. }
    { Description
    »ζΦΖΤΪsrcRect·¶Ξ§ΔΪµΔΛωΣΠΧΣΏΨΌώ΅£
    aBuffer.Size Must >= ClientRect.
    }
    procedure PaintChildsTo(aBuffer: TBitmap32; const srcRect: TRect; const
      aFirstControl: Integer = 0; const  aPaintAll: Boolean = False); overload;
    procedure PaintEffects(aBitmap32: TBitmap32);
    { Description
    Paint Self to a Buffer at the pos(Left, Right) of aBuffer include its
    children.
    }
    procedure PaintSelfTo(aBuffer: TBitmap32); overload;
    { Description
    Paint Self to a Buffer at the pos(Left, Right) of aBuffer include its
    children.

    only paint the GRControls child.
    }
    procedure PaintSelfTo(aBuffer: TBitmap32; const srcRect: TRect); overload;
    { Summary Paint the Component Foreground itself(No parent background, or
      sons). }
    { Description
    Φ»»ζΦΖΗ°Ύ°£¨²»°όΐ¨ΛόµΔΈΈ±³Ύ°ΊΝΛόΟΒΚτµΔΧΣΤΌώ£©

    this would be clear and resize the aBuffer to the size of the control
    }
    function PaintSelfToBuffer(aBuffer: TBitmap32; const aPaintAll: Boolean =
      False): Boolean; overload;
    { Summary Paint the Component Foreground itself(No parent background) in
      the srcRect. }
    { Description
    »ζΦΖΈΓΤΌώµΔΦΈ¶¨·¶Ξ§ΔΪµΔΗ°Ύ°£¨²»°όΐ¨ΛόµΔΈΈ±³Ύ°£©µ½Buffer (0,0).


    this would be clear and resize the aBuffer to the size of the control
    }
    function PaintSelfToBuffer(aBuffer: TBitmap32; const srcRect: TRect; const
      aPaintAll: Boolean = False): Boolean; overload;
    { Summary Paint itself to DC }
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£

    NOTE: this method do not paint its sub-controls to DC!
    }
    procedure PaintTo(DC: HDC); overload;
    { Summary Paint itself to DC }
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£

    NOTE: this method do not paint its sub-controls to DC!
    }
    procedure PaintTo(DC: HDC; srcRect: TRect); overload;
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£
    }
    procedure PaintTo(Dst: TBitmap32); overload;
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£

    }
    procedure PaintTo(Dst: TBitmap32; srcRect: TRect); overload;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Color read GetColor;
    { Summary ΝΟ·ΈΓΏΨΌώΌ΄ΏΙΝΟ¶―ΥϋΈφ΄°Με΅£ }
    property DragAsTitle: Boolean read FDragAsTitle write FDragAsTitle;
    { Summary if true, only paint GRControls in it to speedup. }
    property FastDraw: Boolean read FFastDraw write SetFastDraw;
    property Frame: TGRFrame read FFrame write SetFrame;
    property FrameHot: TGRFrame read FFrameHot write SetFrameHot;
    { Summary the current mouse state }
    property MouseButtons: TMouseButtons read FMouseButtons;
    property MouseInControl: Boolean read FMouseInControl;
    property MousePosition: TPoint read GetMousePosition;
    property MouseXPos: Integer read FMouseXPos;
    property MouseYPos: Integer read FMouseYPos;
    { Summary the current shift state }
    property ShiftState: TShiftState read FShiftState;
    property StyleController: TGRStyleController read FStyleController write
      SetStyleController;
    property UpdateCount: Integer read FUpdateCount;
    property OnPaint: TGRPaintEvent read FOnPaint write FOnPaint;
  published
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    { Summary Specifies the degree of translucency on a translucent control. }
    { Description
    Set AlphaBlendValue to a value between 0 and 255 to indicate the degree of 
    translucency when the AlphaBlend property is true. A value of 0 indicates 
    a completely transparent control. A value of 255 indicates complete opacity.

    Note:	AlphaBlendValue only has an effect when the AlphaBlend property is
    true.
    }
    property AlphaBlendValue: Byte read FAlphaBlendValue write
      SetAlphaBlendValue default 255;
    property Transparent: Boolean read FTransparent write SetTransparent;
    { Summary when EndUpdate(Invalidate). }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  { Summary the abstract Graphic32 GraphicControl. }
  { Description
  !+ Focused message supports:
    CM_GOTFOCUS
  }
  TGRGraphicControl = class(TGraphicControl)
  private
    FFrame: TGRFrame;
    FFrameHot: TGRFrame;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TGRPaintEvent;
    FStyleController: TGRStyleController;
    function GetActiveTextColor: TColor;
    function GetColor: TColor;
    procedure SetCaptionFont(const Value: TFont32);
    procedure SetFrame(const Value: TGRFrame);
    procedure SetFrameHot(const Value: TGRFrame);
    procedure SetStyleController(const Value: TGRStyleController);
  protected
    FActiveTextColor: TColor;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    { Summary ±£΄ζµΔΚΗ±³Ύ°ΊΝΤΌώΧΤΙνµΔ½ηΓζ£΅ }
    { Description
    ²»°όΐ¨ΛόµΔΧΣΤΌώµΔ½ηΓζ΅£

    See Also FBufferDirty, DoubleBuffered
    }
    FBuffer: TBitmap32;
    { Summary ±νΚΎΠθΦΨΠΒ»ζΦΖ»Ί³εΗψ }
    { Description
    See Also FBuffer
    }
    FBufferDirty: Boolean;
    FCaptionFont: TFont32;
    FMouseInControl: Boolean;
    FSelfBuffer: TBitmap32;
    { Summary ±νΚΎΠθΦΨΠΒ»ζΦΖ»Ί³εΗψ }
    { Description
    See Also FBuffer
    }
    FSelfBufferDirty: Boolean;
    FTransparent: Boolean;
    FUpdateCount: Integer;
    procedure AfterPaintBuffer(aBitmap32: TBitmap32); virtual;
    { Summary Assign the CaptionFont to the Buffer. }
    { Description
    See Also InternalPaintBuffer, AfterPaintBuffer, PaintBuffer
    }
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); virtual;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentFontChanged(var Message: TMessage); message
      CM_PARENTFONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure DoFrameChanged(Sender: TObject);
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure FontChanged(Sender: TObject); virtual;
    { Summary the derived class should override this to paint itself. }
    { Description

    the result specify the paint is ok or not. the default is ok(true).
    When u paint it failed, u should set result as false!

    See Also: BeforePaintBuffer, AfterPaintBuffer, PaintBuffer
    }
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; virtual;
    function IsFrameEnabled: Boolean;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintParentBackground(aBitmap32: TBitmap32); overload; virtual;
    procedure Resize; override;
    procedure SetAlphaBlend(const Value: Boolean); virtual;
    procedure SetAlphaBlendValue(const Value: Byte); virtual;
    procedure SetTransparent(const Value: Boolean);
    procedure UpdateTracking;
    { Description
    if Messsage.Unused = nil then
      Update the background
    else 
      Paint background to TEraseBackgroundInfo(Messsage.Unused).Bitmap32
    }
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    property ActiveTextColor: TColor read GetActiveTextColor write
      FActiveTextColor default clNone;
    property CaptionFont: TFont32 read FCaptionFont write SetCaptionFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetBorderHeight: Integer;
    function GetBorderWidth: Integer;
    function GetBottomBorderSize: Integer;
    { Summary return the current paint Frame. }
    function GetCurrentStyle: TGRStyle; virtual;
    function GetLeftBorderSize: Integer;
    { Summary return the current paint Frame. }
    function GetPaintFrame: TGRFrame;
    function GetRightBorderSize: Integer;
    function GetTopBorderSize: Integer;
    procedure Invalidate; override;
    procedure InvalidateBuffer;
    procedure InvalidateSelfBuffer;
    function IsBufferDirty: Boolean;
    procedure PaintBuffer(NeedParentBackground: Boolean = True);
    procedure PaintEffects(aBitmap32: TBitmap32);
    { Description
    Paint Self to a Buffer at the pos(Left, Right) of aBuffer include its
    children.
    }
    procedure PaintSelfTo(aBuffer: TBitmap32); overload;
    { Description
    Paint Self to a Buffer at the pos(Left, Right) of aBuffer include its
    children.
    }
    procedure PaintSelfTo(aBuffer: TBitmap32; DstRect, srcRect: TRect);
      overload;
    { Summary Paint the Component Foreground itself(No parent background, or
      sons). }
    { Description
    Φ»»ζΦΖΗ°Ύ°£¨²»°όΐ¨ΛόµΔΈΈ±³Ύ°ΊΝΛόΟΒΚτµΔΧΣΤΌώ£©
    }
    function PaintSelfToBuffer(aBuffer: TBitmap32): Boolean; overload;
    { Summary Paint the Component Foreground itself(No parent background) in
      the srcRect. }
    { Description
    »ζΦΖΈΓΤΌώµΔΦΈ¶¨·¶Ξ§ΔΪµΔΗ°Ύ°£¨²»°όΐ¨ΛόµΔΈΈ±³Ύ°£©µ½Buffer (0,0).


    this would be clear and resize the aBuffer to the size of the control
    }
    function PaintSelfToBuffer(aBuffer: TBitmap32; const srcRect: TRect):
      Boolean; overload;
    { Summary Paint itself to DC }
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£

    NOTE: this method do not paint its sub-controls to DC!
    }
    procedure PaintTo(DC: HDC); overload;
    { Summary Paint itself to DC }
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£

    NOTE: this method do not paint its sub-controls to DC!
    }
    procedure PaintTo(DC: HDC; srcRect: TRect); overload;
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£
    }
    procedure PaintTo(Dst: TBitmap32); overload;
    { Description
    srcRect: witch to paint. ±νΓχΠθ»ζΦΖΈΓΤΌώµΔΔΗ»²Ώ·ΦµΔΗψΣς΅£
    }
    procedure PaintTo(Dst: TBitmap32; srcRect: TRect); overload;
    property Color read GetColor;
    property Frame: TGRFrame read FFrame write SetFrame;
    property FrameHot: TGRFrame read FFrameHot write SetFrameHot;
    property MouseInControl: Boolean read FMouseInControl;
    property StyleController: TGRStyleController read FStyleController write
      SetStyleController;
    property OnPaint: TGRPaintEvent read FOnPaint write FOnPaint;
  published
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    { Summary Specifies the degree of translucency on a translucent control. }
    { Description
    Set AlphaBlendValue to a value between 0 and 255 to indicate the degree of 
    translucency when the AlphaBlend property is true. A value of 0 indicates 
    a completely transparent control. A value of 255 indicates complete opacity.

    Note:	AlphaBlendValue only has an effect when the AlphaBlend property is
    true.
    }
    property AlphaBlendValue: Byte read FAlphaBlendValue write
      SetAlphaBlendValue default 255;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  { Summary the abstract Graphics32 Custom Control with background supports. }
  TGRBGCustomControl = class(TGRCustomControl)
  private
    procedure SetBackground(const Value: TGRBackground);
  protected
    FBackground: TGRBackground;
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); override;
    procedure DoBackgroundChanged(Sender: TObject);
    function GetBackground: TGRBackground;
    class function GetBackgroundClass: TGRBackgroundClass; virtual;
    { Summary The control's Background. }
    property Background: TGRBackground read GetBackground write SetBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Summary the abstract Graphics32 GraphicControl with background supports. }
  TGRBGGraphicControl = class(TGRGraphicControl)
  private
    procedure SetBackground(const Value: TGRBackground);
  protected
    FBackground: TGRBackground;
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); override;
    procedure DoBackgroundChanged(Sender: TObject);
    function GetBackground: TGRBackground;
    class function GetBackgroundClass: TGRBackgroundClass; virtual;
    { Summary The control's Background. }
    property Background: TGRBackground read GetBackground write SetBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGRStyleController = class(TComponent)
  private
    FDownStyle: TGRStyle;
    FHotStyle: TGRStyle;
    { Summary Collect the ControlLayer }
    FList: TList;
    FNormalStyle: TGRStyle;
    procedure SetDownStyle(const Value: TGRStyle);
    procedure SetHotStyle(const Value: TGRStyle);
    procedure SetNormalStyle(const Value: TGRStyle);
  protected
    procedure DoNormalStyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FreeNotification(AComponent: TComponent);
    procedure RemoveFreeNotification(AComponent: TComponent);
    { Summary Mouse Down State Style }
    property DownStyle: TGRStyle read FDownStyle write SetDownStyle;
    property HotStyle: TGRStyle read FHotStyle write SetHotStyle;
    property NormalStyle: TGRStyle read FNormalStyle write SetNormalStyle;
  end;


  TGRDefaultEffectOptions = record
    Enabled: Boolean;
    //Animation: TTeAdvancedAnimation;
  end;

function DrawParentBackground(const aControl: TControl; const DC: HDC): Boolean;

var
  DefaultEffectOptions: TGRDefaultEffectOptions;
     
implementation

var
  GDefaultPopupMenu: TPopupMenu = nil;

type
  TMyShiftState  = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll,ssTriple,ssQuad,ssExtra1,ssExtra2);
  TMyShiftStates = set of TMyShiftState;

const
  sc_DragMove=$F012; //61458

//Not double click in shift state
function IsMouseButtonDown(Shift: TMyShiftStates; Button: TMyShiftState): Boolean;
begin
  Result := (Button in Shift) and not (ssDouble in Shift);
end;

function GetParentForm(aControl: TControl): TCustomForm;
begin
  Result := nil;
  WHile Assigned(aControl) do
  begin
    if aControl is TCustomForm then
    begin
      Result := TCustomForm(aControl);
      break;
    end;
    aControl := aControl.Parent;
  end;
end;

function DrawParentBackground(const aControl: TControl; const DC: HDC): Boolean;
var
  P: TPoint;
  SaveIndex: integer;

  // PaintParentSubControls to DC
  // Draw the Graphic Controls in the Parent control.
  Procedure PaintParentSubControls(Parent: TWinControl; DC: HDC);
  var
    i: integer;
    SaveIndex2: integer;
    ClipRectExists: Integer;
    vControl: TControl;
  Begin
    (*//
    {New way:
    1.First found the first control after it.
      Check whether Intersect Rect(aRect := IntersectRect). 
      if any add the rect to RectList(add(aRect)) and paint it.
    2. then found the seond control after it.
      Check whether Intersect Rect: aRect := IntersectRect. 
      if any, Check whether Intersect Rect with RectList:
        For J := 0 to RectList.Count - 1 do
          if InterectRect(outRect, aRect, RectList[i]) then
            //remove the RectList[i] in the aRect
            if RectList[i].Left aRect.Left
        if any, 
    } 
    i := Parent.ControlCount;
    if i = 0 then exit;
    while i > 0  do
    begin
      vControl := Parent.Controls[i]; 
      Dec(i);
      if vControl = aControl then break;
    end;
    While i >= 0 do
    begin
      vControl := Parent.Controls[i]; 
      {$ifdef Designtime_Supports}
      if not (vControl.Visible 
        or ((csDesigning in ComponentState) and not (csNoDesignVisible in ControlStyle))) then Continue;
      {$else} 
      if not vControl.Visible then Continue;
      {$endif} 
      Dec(i);
    end; //---NEW WAY END*)

      for i := 0 to Parent.ControlCount - 1 do
      begin
        vControl := Parent.Controls[i]; 
        if vControl = aControl then
        begin
          break;
        end;
        {$ifdef Designtime_Supports}
        if not (vControl.Visible 
          or ((csDesigning in ComponentState) and not (csNoDesignVisible in ControlStyle))) then Continue;
        {$else} 
        if not vControl.Visible then Continue;
        {$endif} 

        begin
          SaveIndex2 := SaveDC(DC);
          try
            MoveWindowOrg(DC, vControl.Left, vControl.Top);
            with vControl.ClientRect do
              ClipRectExists := IntersectClipRect(DC, Left, Top, Right, Bottom);
            If (ClipRectExists <> NULLREGION) and (ClipRectExists <> ERROR) Then
            Begin

                TControlAccess(vControl).Perform(WM_PAINT, DC, 0);
            End;
            MoveWindowOrg(DC, -vControl.Left, -vControl.Top);
          finally
            RestoreDC(DC, SaveIndex2);
          end;
        end;
      end;

  End;
begin
  Result := Assigned(aControl.Parent) and (aControl.Parent.HandleAllocated);
  if Result then
  begin
    SaveIndex := SaveDC(DC);
   try 
    if aControl.Parent is TGRCustomControl then
       with aControl.Parent as TGRCustomControl do
       begin
         P := aControl.ClientOrigin;
         ScreenToClient(P);
         MoveWindowOrg(DC, -P.X, -P.Y);
         PaintTo(DC, aControl.ClientRect);
         PaintParentSubControls(aControl.Parent, DC);
         MoveWindowOrg(DC, P.X, P.Y);
       end
    else if aControl.Parent is TWinControl then
      with aControl.Parent as TWinControl do begin
        P := ClientOrigin;
        ScreenToClient( P);
        MoveWindowOrg(DC, -P.X, -P.Y);
        Parent.Perform(WM_ERASEBKGND, DC, 0);
        Parent.Perform(WM_PAINT, DC, 0);
        PaintParentSubControls(TWinControl(aControl.Parent), DC);
        MoveWindowOrg(DC, P.X, P.Y);
      end;
   finally
    RestoreDC(DC, SaveIndex);
   end;
  end;  
end;

constructor TGRCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TBitmap32Ex.Create;
  FSelfBuffer := TBitmap32Ex.Create;
  //FBuffer.DrawMode := dmBlend;
  FSelfBuffer.DrawMode := dmBlend;
  //FSelfBuffer.CombineMode := cmMerge;
  FBufferDirty := True;
  FSelfBufferDirty := True;
  FAlphaBlendValue := 255;
  FCaptionFont := TFont32.Create;
  FCaptionFont.OnChange := FontChanged;

  FFrame := TGRFrame.Create(Self);
  FFrame.OnChanged := DoFrameChanged;

  FFrameHot := TGRFrame.Create(Self);
end;

destructor TGRCustomControl.Destroy;
begin
  FreeAndNil(FSelfBuffer);
  FreeAndNil(FBuffer);
  FreeAndNil(FCaptionFont);
  FreeAndNil(FFrame);
  FreeAndNil(FFrameHot);
  inherited Destroy;
end;

procedure TGRCustomControl.AfterPaintBuffer(aBitmap32: TBitmap32);
begin
  aBitmap32.ResetClipRect;
  GetPaintFrame.PaintTo(aBitmap32, aBitmap32.ClipRect);
end;

procedure TGRCustomControl.BeforePaintBuffer(aBitmap32: TBitmap32);
begin
  {with GetPaintFrame do
    if Enabled then
      //[Bug]: Fixed this would the Text disapear in the Panel!!!
      //Rect(0,0,Width, Height) to ClientRect.
      aBitmap32.ClipRect := CalcClientRect(ClientRect);
  //}
end;

procedure TGRCustomControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGRCustomControl.ClearDefaultPopupMenu(const aPopupMenu: TPopupMenu);
begin
  aPopupMenu.Items.Clear;
end;

procedure TGRCustomControl.CMMouseEnter(var Message: TMessage);
begin
  //inherited;
  //MouseEnter;
  if Message.LParam = 0 then
  begin
    if Parent <> nil then
    begin
      Message.LParam := LongInt(Self);
      Broadcast(Message);
      Parent.Broadcast(Message);
    end;
    MouseEnter;

  end
  else if (TControl(Message.LParam) <> Self) then
  begin
    //ShowMessage('MouseEnter');
    if FMouseInControl then
      MouseLeave
    else
      Broadcast(Message);
  end;
end;

procedure TGRCustomControl.CMMouseLeave(var Message: TMessage);
begin
  //inherited;

  MouseLeave;
end;

procedure TGRCustomControl.CMParentFontChanged(var Message: TMessage);
begin
  {if ParentFont then
  begin
    if Message.wParam <> 0 then
      FCaptionFont.Assign(TFont(Message.lParam))
    else begin
      if Parent is TGRCustomControl then
        FCaptionFont.Assign(TGRCustomControl(Parent).FCaptionFont)
      else
        FCaptionFont.Assign(TParentControl(Parent).Font);
    end;
  end; //}
  inherited;
end;

procedure TGRCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FTransparent then
      ExStyle := ExStyle or WS_EX_TRANSPARENT;
end;

procedure TGRCustomControl.DoDefaultPopupMenu(const PopupMenu: TPopupMenu);
begin
end;

procedure TGRCustomControl.DoFrameChanged(Sender: TObject);
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateSelfBuffer;
  end;
end;

procedure TGRCustomControl.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TGRCustomControl.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TGRCustomControl.EndUpdate;
begin
  Dec(FUpdateCount);
  InvalidateSelfBuffer;
end;

procedure TGRCustomControl.FontChanged(Sender: TObject);
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateSelfBuffer;
    //InvalidateBuffer;
  end;
end;

function TGRCustomControl.GetBorderHeight: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Left + LFrame.ImageFrame.Width.Right
    else
      Result := LFrame.Width * 2;
  end
  else
    Result := 0;
end;

function TGRCustomControl.GetBorderWidth: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Bottom + LFrame.ImageFrame.Width.Top
    else
      Result := LFrame.Width * 2;
  end
  else
    Result := 0;
end;

function TGRCustomControl.GetBottomBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Bottom
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRCustomControl.GetColor: TColor;
var
  LStyle: TGRStyle;
begin
  LStyle := GetCurrentStyle;
  if Assigned(LStyle) then
    Result := LStyle.Color
  else
   Result := inherited Color;
end;

function TGRCustomControl.GetCurrentStyle: TGRStyle;
begin
  Result := nil;
  if Assigned(StyleController) then
  begin
    if FMouseInControl and StyleController.HotStyle.Frame.Enabled then
      Result := StyleController.HotStyle
    else
      Result := StyleController.NormalStyle;
    if not Result.Enabled then
      Result := nil;
  end;
end;

function TGRCustomControl.GetLeftBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Left
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRCustomControl.GetMousePosition: TPoint;
begin
  Result := Point(FMouseXPos, FMouseYPos);
end;

function TGRCustomControl.GetPaintFrame: TGRFrame;
var
  LStyle: TGRStyle;
begin
  Result := nil;
  LStyle := GetCurrentStyle;
  if Assigned(LStyle) then
  begin
    if LStyle.Frame.Enabled then
      Result := LStyle.Frame;
  end;

  if not Assigned(Result) then
  begin
    if FMouseInControl and FrameHot.Enabled then
      Result := FrameHot
    else
      Result := Frame;
  end;
end;

function TGRCustomControl.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if Result = nil then
  begin
    ClearDefaultPopupMenu(GDefaultPopupMenu);
    DoDefaultPopupMenu(GDefaultPopupMenu);
    if GDefaultPopupMenu.Items.Count > 0 then
      Result := GDefaultPopupMenu;
  end;
end;

function TGRCustomControl.GetRightBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Right
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRCustomControl.GetTopBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Top
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRCustomControl.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
begin
  Result := True;
  //FBufferDirty := False;
end;

procedure TGRCustomControl.Invalidate;
begin
  if FUpdateCount <= 0 then
  begin
    inherited Invalidate;
    if Parent is TGRCustomControl then
    begin
      TGRCustomControl(Parent).InvalidateChild(Self);
    end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TGRCustomControl.InvalidateBuffer;
begin
  FBufferDirty := True;
  Invalidate;
end;

procedure TGRCustomControl.InvalidateChild(const aChild: TControl);
var
  I: Integer;
begin
  {// if the child is transparent then DONOT USE this
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin

    if Assigned(FSelfBuffer) and not FSelfBuffer.Empty then
    for i := 0 to ControlCount - 1 do
     if Controls[i] = aChild then
     begin
       PaintChildsTo(FSelfBuffer, aChild.BoundsRect, i);
       break;
     end;
    InvalidateBuffer;
  end;

  //}
  //FSelfBufferDirty := True;
  //FBufferDirty := True;
  //if Assigned(FOnChange) then FOnChange(Self);

  if (aChild is TGRGraphicControl) and not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    //FSelfBufferDirty := True;
    //vRect := aChild.BoundsRect;
    //InvalidateRect(Handle, @vRect, False);
    InvalidateSelfBuffer;
  end;
end;

procedure TGRCustomControl.InvalidateSelfBuffer;
begin
  //if not FSelfBuffer.Empty then FSelfBuffer.Delete;
  FSelfBufferDirty := True;
  FBufferDirty := True;
  //FSelfBuffer.Delete;
  Invalidate;
end;

function TGRCustomControl.IsBufferDirty: Boolean;
begin
  Result := FBufferDirty or FSelfBufferDirty;
end;

function TGRCustomControl.IsStyleEnabled: Boolean;
begin
  if Assigned(StyleController) then
  begin
    with StyleController do
      Result := HotStyle.Enabled or NormalStyle.Enabled or DownStyle.Enabled;
  end
  else begin
    Result := FrameHot.Enabled or Frame.Enabled;
  end;
end;

procedure TGRCustomControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FShiftState := Shift;
end;

procedure TGRCustomControl.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
end;

procedure TGRCustomControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  FShiftState := Shift;
end;

procedure TGRCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FShiftState := Shift;
  Include(FMouseButtons, Button);

  FMouseXPos := X;
  FMouseYPos := Y;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TGRCustomControl.MouseEnter;
begin
  if not FMouseInControl then
  begin
    FMouseInControl := true;
    if IsStyleEnabled then InvalidateSelfBuffer;
    DoMouseEnter;
  end;
end;

procedure TGRCustomControl.MouseLeave;
begin
  if FMouseInControl then
  begin
    FMouseInControl := false;
    if IsStyleEnabled then InvalidateSelfBuffer;
    DoMouseLeave;
  end;
end;

procedure TGRCustomControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LIsDragging: Boolean;
  LParentForm: TCustomForm;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragAsTitle then
  begin
    LIsDragging := IsMouseButtonDown(TMyShiftStates(Shift), ssLeft) and IsMouseButtonDown(TMyShiftStates(FShiftState), ssLeft)
      and ((Abs(X - FMouseXPos) >=2) or (Abs(Y - FMouseYPos) >=2));

    LParentForm := TCustomForm(GetParentForm(Parent));
    if LIsDragging and Assigned(LParentForm) then
    begin
    	//no the mouseUp will be disabled!!
      ReleaseCapture;
      SendMessage(LParentForm.Handle, WM_SYSCOMMAND, sc_DragMove, 0);
    end;
  end;

  FMouseXPos := X;
  FMouseYPos := Y;

  if not FMouseInControl then
    UpdateTracking;
end;

procedure TGRCustomControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FShiftState := Shift;
  Exclude(FMouseButtons, Button);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TGRCustomControl.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (aComponent = FStyleController) then
  begin
    FStyleController := nil;
    DoFrameChanged(Self);
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TGRCustomControl.Paint;
begin
  {$IFDEF FastDraw_Supports}
  if Parent is TGRCustomControl then
  begin
  // do not paint, this will be done by Parent.
  end
  else
  {$ENDIF}
  begin
    if FBufferDirty or FSelfBufferDirty then
    begin
      //PaintBuffer(not FTransparent or FAlphaBlend);
      PaintBuffer;
    end;

    {if Transparent and not FAlphaBlend then
      StretchToDCTransparentFunc(Canvas.Handle, 0,0, FBuffer.Width, FBuffer.Height,
      FBuffer, 0,0, FBuffer.Width, FBuffer.Height)
    else //}
      FBuffer.DrawTo(Canvas.Handle, 0, 0);
  end;

  if Assigned(FOnPaint) then FOnPaint(Self, Canvas);
end;

procedure TGRCustomControl.PaintBuffer(NeedParentBackground: Boolean = True);
begin
  if not HandleAllocated or (FUpdateCount > 0) then Exit;
  //LBuffer := TBitmap32.Create;
  FBuffer.BeginUpdate;
  try
    FBuffer.SetSize(Width, Height);
    //LBuffer.SetSize(Width, Height);
    if NeedParentBackground
      {$IFDEF FastDraw_Supports}
      and not FFastDraw
      {$ENDIF}
      and (Transparent or AlphaBlend) then
    begin
      PaintParentBackground(FBuffer);
    end
    else if Transparent {or AlphaBlend} then
    begin
      FBuffer.Clear(0);
    end
    else begin
      FBuffer.Clear(Color32(Color));
    end;

    {$IFDEF FastDraw_Supports}
    if FFastDraw or (Parent is TGRCustomControl) then
    begin
      PaintSelfTo(FBuffer);
    end
    else
    {$ENDIF}
    begin
      //FBufferDirty := FSelfBuffer.Empty;
      //TODO: I HAVE TO Disable for The GRGrid Control
      // the GRGrid CAN NOT UPDATE after Create.
      // It IS a Strange Bug!!!
      if FSelfBufferDirty then
      begin
        FSelfBufferDirty := PaintSelfToBuffer(FSelfBuffer);
        FBufferDirty := FSelfBufferDirty;
      end;

      if AlphaBlend then
      begin
        //LBuffer.MasterAlpha := AlphaBlendValue;
        FSelfBuffer.MasterAlpha := AlphaBlendValue;
      end
      else
        FSelfBuffer.MasterAlpha := 255;//}
      FSelfBuffer.DrawTo(FBuffer);
    end;
  finally
    FBuffer.EndUpdate;
  end;
end;

procedure TGRCustomControl.PaintChildsTo(aBuffer: TBitmap32; const aPaintAll:
  Boolean = False);
var
  srcRect: TRect;
begin
  srcRect := ClientRect;
  PaintChildsTo(aBuffer, srcRect, 0, aPaintAll);
end;

procedure TGRCustomControl.PaintChildsTo(aBuffer: TBitmap32; const srcRect:
  TRect; const aFirstControl: Integer = 0; const  aPaintAll: Boolean = False);
var
  I: Integer;
  aControl: TControl;
  aRect: TRect;
  LBMP: TBitmap32;
begin
  if ControlCount > 0 then
  begin
    {$ifdef GDI_Supports}
    LBmp := TBitmap32.Create;
    try
    {$endif}
    for i := aFirstControl to ControlCount - 1 do
    begin
      aControl := Controls[i];
      if not aControl.Visible or not GR32.IntersectRect(aRect, srcRect, aControl.BoundsRect) then continue;
      if aControl is TGRGraphicControl then
      begin
        TGRGraphicControl(aControl).PaintSelfTo(aBuffer);
      end
      else if aControl is TGRCustomControl then
      begin
        //¶ΤΣΪ ΧΣGRCustomControls»ΉΚΗΣΙwINDOWSΝκ³Ι΅£
        {.$IFDEF FastDraw_Supports}
        if aPaintAll then
          TGRCustomControl(aControl).PaintSelfTo(aBuffer);
        {.$ENDIF}
      end
      {$ifdef GDI_Supports}
      else begin
        LBmp.SetSize(aControl.Width, aControl.Height);
        TControlAccess(aControl).Perform(WM_PAINT, LBmp.Handle, 0);
        LBmp.ResetAlpha();
        OffsetRect(aRect, -aControl.Left, -aControl.Top);
        //LBmp.ClipRect := aRect;
        LBmp.DrawTo(aBuffer, aControl.Left, aControl.Top, aRect);
      end
      {$endif}
      ;
    end; //for
    {$ifdef GDI_Supports}
    finally
      LBmp.Free;
    end;
    {$endif}
  end;
end;

procedure TGRCustomControl.PaintEffects(aBitmap32: TBitmap32);
var
  LColor: TColor32;
begin
  LColor := Color;
  if FTransparent and (TColor(LColor) <> clNone) then
  begin
    //Blur the background like the glass
    LColor := Color32(TColor(LColor));
    TColor32Rec(LColor).rgbAlpha := AlphaBlendValue;
    if FMouseInControl then
      LColor := Lighten(LColor, 50);
    ApplyMiddleColor(aBitmap32, LColor);
    BlurEffect3x3(aBitmap32, 255);
    //SplitBlur(aBitmap32, 1);
    //AddNoise(aBitmap32, 5, true);
  end;
end;

procedure TGRCustomControl.PaintParentBackground(aBitmap32: TBitmap32);
begin
  if Transparent or AlphaBlend then
  begin
    if DrawParentBackground(Self, aBitmap32.Handle) then
      aBitmap32.ResetAlpha;
  end;

  (*
    {$ifdef debug}
    //if Name = 'SubPanel2' then
    begin
    sendDebug(Name+':Exit PaintBackground');
    end;
    {$endif}
  //*)
end;

procedure TGRCustomControl.PaintSelfTo(aBuffer: TBitmap32);
var
  srcRect: TRect;
begin
  srcRect := ClientRect;

  if FSelfBufferDirty then
  begin
    FBufferDirty := PaintSelfToBuffer(FSelfBuffer);
    FSelfBufferDirty := FBufferDirty;
  end;

  if AlphaBlend then
  begin
    //LBuffer.MasterAlpha := AlphaBlendValue;
    FSelfBuffer.MasterAlpha := AlphaBlendValue;
  end
  else
    FSelfBuffer.MasterAlpha := 255;
  FSelfBuffer.DrawTo(aBuffer, Left, Top, srcRect);
end;

procedure TGRCustomControl.PaintSelfTo(aBuffer: TBitmap32; const srcRect:
  TRect);
begin
  if FSelfBufferDirty then
  begin
    FBufferDirty := PaintSelfToBuffer(FSelfBuffer, srcRect);
    FSelfBufferDirty := FBufferDirty;
  end;


  if AlphaBlend then
  begin
    //LBuffer.MasterAlpha := AlphaBlendValue;
    FSelfBuffer.MasterAlpha := AlphaBlendValue;
  end
  else
    FSelfBuffer.MasterAlpha := 255;

  FSelfBuffer.DrawTo(aBuffer, Left, Top, srcRect);
end;

function TGRCustomControl.PaintSelfToBuffer(aBuffer: TBitmap32; const
  aPaintAll: Boolean = False): Boolean;
begin
  aBuffer.SetSize(Width, Height);
  aBuffer.Clear(0);
  aBuffer.Font := FCaptionFont;
  BeforePaintBuffer(aBuffer);
  PaintEffects(aBuffer);
  Result := not InternalPaintBuffer(aBuffer);


  //FSelfBufferDirty := FBufferDirty;
  AfterPaintBuffer(aBuffer);

  {x$IFDEF FastDraw_Supports}
  //if FFastDraw or (Parent is TGRCustomControl) then
  PaintChildsTo(aBuffer, aPaintAll);
  {x$ENDIF}

  {if AlphaBlend then
  begin
    aBuffer.DrawMode := dmBlend;
    aBuffer.MasterAlpha := AlphaBlendValue;
  end;//}
end;

function TGRCustomControl.PaintSelfToBuffer(aBuffer: TBitmap32; const srcRect:
  TRect; const aPaintAll: Boolean = False): Boolean;
begin
  aBuffer.SetSize(Width, Height);
  aBuffer.Clear(0);
  aBuffer.Font := FCaptionFont;
  aBuffer.ClipRect := srcRect;

  BeforePaintBuffer(aBuffer);
  PaintEffects(aBuffer);
  Result := not InternalPaintBuffer(aBuffer);


  //FSelfBufferDirty := FBufferDirty;
  AfterPaintBuffer(aBuffer);

  //if FFastDraw or (Parent is TGRCustomControl) then
  PaintChildsTo(aBuffer, srcRect, 0, aPaintAll);

  aBuffer.ResetClipRect;
end;

procedure TGRCustomControl.PaintTo(DC: HDC);
var
  srcRect: TRect;
  I: Integer;
begin
  i := GetClipBox(DC, @srcRect);
  //if (i = RGN_ERROR) or (i = RGN_ERROR) then exit; //SOS 9999
  PaintTo(DC, srcRect);
end;

procedure TGRCustomControl.PaintTo(DC: HDC; srcRect: TRect);
begin
  if IsBufferDirty then
    //PaintBuffer(not FTransparent or FAlphaBlend);
    PaintBuffer;

  {if Transparent and not FAlphaBlend then
    StretchToDCTransparentFunc(DC, SrcRect.Left, SrcRect.Top, FBuffer.Width, FBuffer.Height,
    FBuffer, 0,0, FBuffer.Width, FBuffer.Height)
  else //}
    FBuffer.DrawTo(DC, SrcRect.Left, SrcRect.Top);
end;

procedure TGRCustomControl.PaintTo(Dst: TBitmap32);
begin
  Dst.SetSize(Width, Height);
  PaintTo(Dst, ClientRect);
end;

procedure TGRCustomControl.PaintTo(Dst: TBitmap32; srcRect: TRect);
var
  I: Integer;
  aControl: TControl;
  aBMP: TBitmap32;
  aRect: TRect;
begin
  if IsBufferDirty then PaintBuffer({True});
  FBuffer.DrawTo(Dst, 0,0, srcRect);
  //TODO: Paint its sub-controls
  if ControlCount > 0 then
  begin
    aBMP := TBitmap32.Create;
    aBMP.DrawMode := dmBlend;
    try
    for i := 0 to ControlCount - 1 do
    begin
      aControl := Controls[i];
      if not aControl.Visible or not GR32.IntersectRect(aRect, srcRect, aControl.BoundsRect) then continue;
      if aControl is TGRCustomControl then
      begin
        with aRect do
          aBMP.SetSize(Right-Left, Bottom-Top);
        TGRCustomControl(aControl).PaintTo(aBMP, aRect);
        aBMP.DrawTo(Dst, aRect.Left, aRect.Top);
      end
      else if aControl is TGRGraphicControl then
      begin
        with aRect do
          aBMP.SetSize(Right-Left, Bottom-Top);
        TGRGraphicControl(aControl).PaintTo(aBMP, aRect);
        aBMP.DrawTo(Dst, aRect.Left, aRect.Top);
      end;
    end; //for
    finally
      aBMP.Free;
    end;
  end;
end;

procedure TGRCustomControl.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    if Value {or Transparet} then
      FBuffer.DrawMode := dmBlend
    else
      FBuffer.DrawMode := dmOpaque;
    {if Value or AlphaBlend then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque]; //}
    FBufferDirty := True;
    if not (csLoading in ComponentState) then
    begin
      InvalidateBuffer;
    end;
  end;
end;

procedure TGRCustomControl.SetAlphaBlendValue(const Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    FBuffer.MasterAlpha := Value;
    if not (csLoading in ComponentState) and FAlphaBlend then
    begin
      InvalidateBuffer;
    end;
  end;
end;

procedure TGRCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  //if Transparent or AlphaBlend then //should re-get the parent-background.
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
    if (ALeft <> Left) or (ATop <> Top) then
    begin
      FBufferDirty := True;

      //TODO: it is VERY SLOW.
      if Parent is TGRCustomControl then
       TGRCustomControl(Parent).InvalidateChild(Self);
    end;

  {if (AWidth <> Width) or (AHeight <> Height) then
  begin
     FSelfBufferDirty := True;
     //if not FSelfBuffer.Empty then FSelfBuffer.Delete;
     FBufferDirty := True;
  end; //}
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if FBufferDirty and HandleAllocated then
    InvalidateRect(Handle, nil, not (csOpaque in ControlStyle));
end;

procedure TGRCustomControl.SetCaptionFont(const Value: TFont32);
begin
  if FCaptionFont <> Value then
    FCaptionFont.Assign(Value);
end;

procedure TGRCustomControl.SetFastDraw(const Value: Boolean);
begin
  if FFastDraw <> Value then
  begin
  InvalidateBuffer;
  FFastDraw := Value;
  end;
end;

procedure TGRCustomControl.SetFrame(const Value: TGRFrame);
begin
  if FFrame <> Value then
    FFrame.Assign(Value);
end;

procedure TGRCustomControl.SetFrameHot(const Value: TGRFrame);
begin
  if FFrameHot <> Value then
    FFrameHot.Assign(Value);
end;

procedure TGRCustomControl.SetStyleController(const Value: TGRStyleController);
begin
  if FStyleController <> Value then
  begin
    if Assigned(FStyleController) then
      FStyleController.RemoveFreeNotification(Self);
  FStyleController := Value;
    if Assigned(FStyleController) then
      FStyleController.FreeNotification(Self);
  end;
end;

procedure TGRCustomControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if Value then
    begin
      ControlStyle := ControlStyle - [csOpaque];
      //that should be false!
      //FDoubleBuffered := False;
    end
    else
      ControlStyle := ControlStyle + [csOpaque];

    if HandleAllocated then self.Invalidate;// RecreateWnd; //SOS 9999

    FBufferDirty := True;
    FSelfBufferDirty := True;
    if not (csLoading in ComponentState) then
    begin
      InvalidateBuffer;
    end;
  end;
end;

procedure TGRCustomControl.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;//}
end;

procedure TGRCustomControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);

  {$ifdef AlphaAboveControl_Supports}
  var
    i: integer;
    vControl: TControl;
    ClipRect: TRect;
  {$endif}

begin
  (*
    {$ifdef debug}
    if Name = 'SubPanel2' then
    begin
    sendDebug(Name+':Enter WMEraseBkgnd');
    if (Message.DC <> 0) and HandleAllocated then
      sendInteger(Name+':GetClipBox', GetClipBox(Message.DC, ClipRect));
      sendInteger(Name+':WindowFromDC', WindowFromDC(Message.DC));
      sendInteger(Name+':SelfHandle', Handle);
      with ClipRect do
        sendDebug(Format(Name+':Left=%d;Right=%d;Top=%d;Bottom=%d'
          , [Left, Right, Top, Bottom]));
    end;
    {$endif}
  //*)

  if //False and //TODO: disable this temp...
     {$IFDEF FastDraw_Supports}not FFastDraw and{$ENDIF} (Message.DC <> 0) and HandleAllocated then
  begin
    if AlphaBlend or Transparent then
    begin
      FBufferDirty := True;
    {$ifdef debug}
    //sendDebug(Name+':Enter WMEraseBkgnd');
    {$endif}
    end;
    (*//notify its children for graphic control.
    for i := 0 to ControlCount - 1 do
    begin
      vControl := Controls[i];
      {$ifdef Designtime_Supports}
      if not (vControl.Visible
        or ((csDesigning in ComponentState) and not (csNoDesignVisible in ControlStyle)))
      then Continue;
      {$else}
      if not vControl.Visible
      then Continue;
      {$endif}
      if (vControl is TGRGraphicControl)
        and (TGRGraphicControl(vControl).AlphaBlend
             or TGRGraphicControl(vControl).Transparent)
      then
        TGRGraphicControl(vControl).Invalidate;
    end; //*)
    {$ifdef AlphaAboveControl_Supports}
    if Assigned(Parent) and Parent.HandleAllocated then
    for i := Parent.ControlCount - 1 downto 0 do
    begin
      vControl := Parent.Controls[i];
      if vControl = Self then break;
      (*
      {$ifdef Debug}
        if vControl.Visible and not IntersectRect(ClipRect, vControl.BoundsRect, BoundsRect)
        then
        begin
          //SendInteger('Name:'+vControl.Name+'.I', Integer(IntersectRect(ClipRect, vControl.BoundsRect, BoundsRect)));
          with vControl.BoundsRect do
            SendDebug(Format('Left=%d;Top=%d;Right=%d;Bottom=%d', [Left, Top, Right, Bottom]));
          with BoundsRect do
            SendDebug(Format('V:Left=%d;Top=%d;Right=%d;Bottom=%d', [Left, Top, Right, Bottom]));
        end;
      {$endif} // *)
      {$ifdef Designtime_Supports}
      if not (vControl.Visible
        or ((csDesigning in ComponentState) and not (csNoDesignVisible in ControlStyle)))
        or not IntersectRect(ClipRect, vControl.BoundsRect, BoundsRect)
      then Continue;
      {$else}
      if not vControl.Visible
        or not IntersectRect(ClipRect, vControl.BoundsRect, BoundsRect)
      then Continue;
      {$endif}
      if (vControl is TGRCustomControl)
        and (TGRCustomControl(vControl).AlphaBlend
             or TGRCustomControl(vControl).Transparent) then
      begin
        TGRCustomControl(vControl).Invalidate;
      end
      else if (vControl is TGRGraphicControl)
        and (TGRGraphicControl(vControl).AlphaBlend
             or TGRGraphicControl(vControl).Transparent) then
      begin
        TGRGraphicControl(vControl).Invalidate;
      end;
    end;
    {$endif}
  end;

  Message.Result := 1;
end;

procedure TGRCustomControl.WMKillFocus(var Message: TMessage);
begin
  DoExit;
  inherited;
end;

procedure TGRCustomControl.WMPaint(var Message: TLMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  (*
    {$ifdef debug}
    if Name = 'SubPanel2' then
    begin
    sendDebug(Name+':Enter Paint');
    {if (Message.DC <> 0) and HandleAllocated then
    beigin
      sendInteger(Name+':GetClipBox', GetClipBox(Message.DC, ClipRect));
      sendInteger(Name+':WindowFromDC', WindowFromDC(Message.DC));
      sendInteger(Name+':SelfHandle', Handle);
      with ClipRect do
        sendDebug(Format(Name+':Left=%d;Right=%d;Top=%d;Bottom=%d'
          , [Left, Right, Top, Bottom]));
    end; //}
    end;
    {$endif}
  //*)

  inherited;
  {DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        Paint;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end; //}
end;

procedure TGRCustomControl.WMSetFocus(var Message: TMessage);
begin
  //DoEnter;
  inherited;
end;


procedure TGRCustomControl.WMSize(var Message: TWMSize);
begin
  inherited;
  //if not FSelfBuffer.Empty then FSelfBuffer.Delete;
  //InvalidateBuffer;
  InvalidateSelfBuffer;
end;

procedure TGRCustomControl.WndProc(var Message: TMessage);
begin
  if FDragAsTitle then
    case Message.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        if IsControlMouseMsg(TLMMouse(Message)) then
      begin
        //first pass it to self to prevent from be processed by children  .
        Dispatch(Message);
      end;
    end;

  inherited;
end;

constructor TGRGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TBitmap32Ex.Create;
  FSelfBuffer := TBitmap32Ex.Create;
  FSelfBuffer.DrawMode := dmBlend;
  FSelfBufferDirty := True;
  FBufferDirty := True;
  FAlphaBlendValue := 255;
  FCaptionFont := TFont32.Create;
  FCaptionFont.OnChange := FontChanged;

  FFrame := TGRFrame.Create(Self);
  FFrame.OnChanged := DoFrameChanged;

  FFrameHot := TGRFrame.Create(Self);
end;

destructor TGRGraphicControl.Destroy;
begin
  FreeAndNil(FSelfBuffer);
  FreeAndNil(FBuffer);
  FreeAndNil(FCaptionFont);
  FreeAndNil(FFrame);
  FreeAndNil(FFrameHot);
  inherited Destroy;
end;

procedure TGRGraphicControl.AfterPaintBuffer(aBitmap32: TBitmap32);
begin
  aBitmap32.ResetClipRect;
  GetPaintFrame.PaintTo(aBitmap32, aBitmap32.ClipRect);

  {if Assigned(FStyleController) then
    FStyleController.Frame.PaintTo(aBitmap32, aBitmap32.ClipRect)
  else
    FFrame.PaintTo(aBitmap32, aBitmap32.ClipRect);
  }
end;

procedure TGRGraphicControl.BeforePaintBuffer(aBitmap32: TBitmap32);
begin
  {with GetPaintFrame do
    if Enabled then
      //[Bug]: Fixed this would the Text disapear in the Panel!!!
      //Rect(0,0,Width, Height) to ClientRect.
      aBitmap32.ClipRect := CalcClientRect(ClientRect);
  //}
end;

procedure TGRGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGRGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  //inherited;
  {if TControl(Message.LParam) = Self then
  begin
  end;
  //}
  if Message.LParam = 0 then
  begin
    if Parent <> nil then
    begin
      Message.LParam := LongInt(Self);
      Parent.Broadcast(Message);
    end;
    MouseEnter;

  end
  else if (TControl(Message.LParam) <> Self) and FMouseInControl then
  begin
    MouseLeave;
  end;
end;

procedure TGRGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  //inherited;
  MouseLeave;
end;

procedure TGRGraphicControl.CMParentFontChanged(var Message: TMessage);
begin
  {if ParentFont then
  begin
    if Message.wParam <> 0 then
      FCaptionFont.Assign(TFont(Message.lParam))
    else begin
      if Parent is TGRCustomControl then
        FCaptionFont.Assign(TGRCustomControl(Parent).FCaptionFont)
      else
        FCaptionFont.Assign(TParentControl(Parent).Font);
    end;
  end; //}
  inherited;
end;

procedure TGRGraphicControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  //InvalidateBuffer;
  {$ifdef debug}
  SendDebug(ClassName+' Caption Text Changed:'+Caption);
  {$endif}
  FBufferDirty := True;
  FSelfBufferDirty := True;
end;

procedure TGRGraphicControl.DoFrameChanged(Sender: TObject);
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateSelfBuffer;
  end;
end;

procedure TGRGraphicControl.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TGRGraphicControl.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TGRGraphicControl.EndUpdate;
begin
  Dec(FUpdateCount);
  //FSelfBufferDirty := True;
  //Invalidate;
  InvalidateSelfBuffer;
end;

procedure TGRGraphicControl.FontChanged(Sender: TObject);
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateSelfBuffer;
  end;
end;

function TGRGraphicControl.GetActiveTextColor: TColor;
begin
  Result := FActiveTextColor;
  if Result = clNone then
    Result := CaptionFont.Color;
end;

function TGRGraphicControl.GetBorderHeight: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Left + LFrame.ImageFrame.Width.Right
    else
      Result := LFrame.Width * 2;
  end
  else
    Result := 0;
end;

function TGRGraphicControl.GetBorderWidth: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Bottom + LFrame.ImageFrame.Width.Top
    else
      Result := LFrame.Width * 2;
  end
  else
    Result := 0;
end;

function TGRGraphicControl.GetBottomBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Bottom
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRGraphicControl.GetColor: TColor;
var
  LStyle: TGRStyle;
begin
  LStyle := GetCurrentStyle;
  if Assigned(LStyle) then
    Result := LStyle.Color
  else
   Result := inherited Color;
end;

function TGRGraphicControl.GetCurrentStyle: TGRStyle;
begin
  Result := nil;
  if Assigned(StyleController) then
  begin
    if FMouseInControl and StyleController.HotStyle.Frame.Enabled then
      Result := StyleController.HotStyle
    else
      Result := StyleController.NormalStyle;
    if not Result.Enabled then
      Result := nil;
  end;
end;

function TGRGraphicControl.GetLeftBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Left
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRGraphicControl.GetPaintFrame: TGRFrame;
var
  LStyle: TGRStyle;
begin
  Result := nil;
  LStyle := GetCurrentStyle;
  if Assigned(LStyle) then
  begin
    if LStyle.Frame.Enabled then
      Result := LStyle.Frame;
  end;

  if not Assigned(Result) then
  begin
    if FMouseInControl and FrameHot.Enabled then
      Result := FrameHot
    else
      Result := Frame;
  end;
end;

function TGRGraphicControl.GetRightBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Right
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRGraphicControl.GetTopBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Top
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TGRGraphicControl.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
begin
  Result := True;
  //FBufferDirty := False;
end;

procedure TGRGraphicControl.Invalidate;
begin
  if FUpdateCount <= 0 then
  begin
    inherited Invalidate;
    if Parent is TGRCustomControl then
    begin
      TGRCustomControl(Parent).InvalidateChild(Self);
    end;
  end;
end;

procedure TGRGraphicControl.InvalidateBuffer;
begin
  FBufferDirty := True;
  Invalidate;
end;

procedure TGRGraphicControl.InvalidateSelfBuffer;
begin
  //if not FSelfBuffer.Empty then FSelfBuffer.Delete;
  FSelfBufferDirty := True;
  FBufferDirty := True;
  Invalidate;
end;

function TGRGraphicControl.IsBufferDirty: Boolean;
begin
  Result := FBufferDirty or FSelfBufferDirty;
end;

function TGRGraphicControl.IsFrameEnabled: Boolean;
begin
  if Assigned(StyleController) then
  begin
    with StyleController do
      Result := HotStyle.Frame.Enabled or NormalStyle.Frame.Enabled;
  end
  else begin
    Result := FrameHot.Enabled or Frame.Enabled;
  end;
end;

procedure TGRGraphicControl.MouseEnter;
begin
  if not FMouseInControl then
  begin
    FMouseInControl := true;
    if IsFrameEnabled then InvalidateSelfBuffer;
    DoMouseEnter;
  end;
end;

procedure TGRGraphicControl.MouseLeave;
begin
  if FMouseInControl then
  begin
    FMouseInControl := false;

    if IsFrameEnabled then InvalidateSelfBuffer;
    DoMouseLeave;
  end;
end;

procedure TGRGraphicControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not FMouseInControl then
    UpdateTracking;
end;

procedure TGRGraphicControl.Paint;
begin
  (*
    {$ifdef debug}
    //if Name = 'SubPanel2' then
    begin
    sendDebug(Name+':Enter Paint');
    end;
    {$endif}
  //*)
  {x$IFDEF FastDraw_Supports}
  if Parent is TGRCustomControl then
    //Paint by its parent.
  else
  {x$ENDIF}
  begin
    if FBufferDirty or FSelfBufferDirty then
    begin
      //Note:Never paint background for transparent or alpha.
      PaintBuffer(False);
    end;

    if Transparent or FAlphaBlend then
      StretchToDCTransparentFunc(Canvas.Handle, 0,0, FBuffer.Width, FBuffer.Height,
      FBuffer, 0,0, FBuffer.Width, FBuffer.Height)
    else
      FBuffer.DrawTo(Canvas.Handle, 0, 0);
    //BitBlt(Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height, FBuffer.Width, FBuffer.Handle, 0, 0, SRCCOPY);
  end;

  if Assigned(FOnPaint) then FOnPaint(Self, Canvas);
end;

procedure TGRGraphicControl.PaintBuffer(NeedParentBackground: Boolean = True);
begin
  FBuffer.BeginUpdate;
  try
    FBuffer.SetSize(Width, Height);

    if NeedParentBackground and (Transparent or AlphaBlend) then
    begin
      PaintParentBackground(FBuffer);
    end
    else if Transparent {or AlphaBlend} then
    begin
      FBuffer.Clear(0);
    end
    else begin
      FBuffer.Clear(Color32(Color));
    end;

    if FSelfBufferDirty then
    begin
      FBufferDirty := PaintSelfToBuffer(FSelfBuffer);
      FSelfBufferDirty := FBufferDirty;
    end;

    {if AlphaBlend then
    begin
      FSelfBuffer.MasterAlpha := AlphaBlendValue;
    end
    else
      FSelfBuffer.MasterAlpha := 255;//}

    FSelfBuffer.DrawTo(FBuffer);
  finally
    FBuffer.BeginUpdate;
  end;
end;

procedure TGRGraphicControl.PaintEffects(aBitmap32: TBitmap32);
var
  LColor: TColor32;
begin
  LColor := Color;
  if FTransparent and (TColor(LColor) <> clNone) then
  begin
    //like the glass
    LColor := Color32(TColor(LColor));
    TColor32Rec(LColor).rgbAlpha := AlphaBlendValue;
    if FMouseInControl then
      LColor := Lighten(LColor, 50);
    ApplyMiddleColor(aBitmap32, LColor);
    BlurEffect3x3(aBitmap32, 255);
    //SplitBlur(aBitmap32, 1);
    AddNoise(aBitmap32, 30, true);
  end;
end;

procedure TGRGraphicControl.PaintParentBackground(aBitmap32: TBitmap32);
begin
  if Transparent or AlphaBlend then
  begin
    if DrawParentBackground(Self, aBitmap32.Handle) then
      aBitmap32.ResetAlpha;
  end; //}

  (*
    {$ifdef debug}
    //if Name = 'SubPanel2' then
    begin
    sendDebug(Name+':Exit PaintBackground');
    end;
    {$endif}
  //*)
end;

procedure TGRGraphicControl.PaintSelfTo(aBuffer: TBitmap32);
var
  I: Integer;
  aControl: TControl;
  aBMP: TBitmap32;
  aRect: TRect;
  DstRect, srcRect: TRect;
begin
  srcRect := ClientRect;

  if FSelfBufferDirty then
  begin
    FBufferDirty := PaintSelfToBuffer(FSelfBuffer);
    FSelfBufferDirty := FBufferDirty;
  end;

  if AlphaBlend then
  begin
    //LBuffer.MasterAlpha := AlphaBlendValue;
    FSelfBuffer.MasterAlpha := AlphaBlendValue;
  end
  else
    FSelfBuffer.MasterAlpha := 255;
  FSelfBuffer.DrawTo(aBuffer, Left, Top, srcRect);
end;

procedure TGRGraphicControl.PaintSelfTo(aBuffer: TBitmap32; DstRect, srcRect:
  TRect);
var
  I: Integer;
  aControl: TControl;
  aBMP: TBitmap32;
  aRect, ClipRect: TRect;
  LDstRect: TRect;
begin
  if FSelfBufferDirty then
  begin
    FBufferDirty := PaintSelfToBuffer(FSelfBuffer, srcRect);
    FSelfBufferDirty := FBufferDirty;
  end;


  if AlphaBlend then
  begin
    //LBuffer.MasterAlpha := AlphaBlendValue;
    FSelfBuffer.MasterAlpha := AlphaBlendValue;
  end
  else
    FSelfBuffer.MasterAlpha := 255;

  FSelfBuffer.DrawTo(aBuffer, Left, Top, srcRect);
end;

function TGRGraphicControl.PaintSelfToBuffer(aBuffer: TBitmap32): Boolean;
begin
  aBuffer.SetSize(Width, Height);
  aBuffer.Clear(0);
  aBuffer.Font := FCaptionFont;
  BeforePaintBuffer(aBuffer);
  PaintEffects(aBuffer);
  Result := not InternalPaintBuffer(aBuffer);
  //FSelfBufferDirty := FBufferDirty;
  AfterPaintBuffer(aBuffer);


  {if AlphaBlend then
  begin
    aBuffer.DrawMode := dmBlend;
    aBuffer.MasterAlpha := AlphaBlendValue;
  end;//}
end;

function TGRGraphicControl.PaintSelfToBuffer(aBuffer: TBitmap32; const srcRect:
  TRect): Boolean;
begin
  aBuffer.SetSize(Width, Height);
  aBuffer.Clear(0);
  aBuffer.Font := FCaptionFont;
  aBuffer.ClipRect := srcRect;

  BeforePaintBuffer(aBuffer);
  PaintEffects(aBuffer);
  Result := not InternalPaintBuffer(aBuffer);


  //FSelfBufferDirty := FBufferDirty;
  AfterPaintBuffer(aBuffer);

  //if FFastDraw or (Parent is TGRCustomControl) then
  //PaintChildsTo(aBuffer, srcRect, 0, aPaintAll);

  aBuffer.ResetClipRect;
end;

procedure TGRGraphicControl.PaintTo(DC: HDC);
var
  srcRect: TRect;
  I: Integer;
begin
  i := GetClipBox(DC, @srcRect);
 // if (i = RGN_ERROR) or (i = RGN_ERROR) then exit; // SOS 9999
  PaintTo(DC, srcRect);
end;

procedure TGRGraphicControl.PaintTo(DC: HDC; srcRect: TRect);
begin
  if IsBufferDirty then
    PaintBuffer(False);
    //PaintBuffer;

  if Transparent or FAlphaBlend then
    StretchToDCTransparentFunc(DC, SrcRect.Left, SrcRect.Top, FBuffer.Width, FBuffer.Height,
    FBuffer, 0,0, FBuffer.Width, FBuffer.Height)
  else //}
    FBuffer.DrawTo(DC, SrcRect.Left, SrcRect.Top);
end;

procedure TGRGraphicControl.PaintTo(Dst: TBitmap32);
begin
  Dst.SetSize(Width, Height);
  PaintTo(Dst, ClientRect);
end;

procedure TGRGraphicControl.PaintTo(Dst: TBitmap32; srcRect: TRect);
begin
  if IsBufferDirty then PaintBuffer({True});
  FBuffer.DrawTo(Dst, 0,0, srcRect);
end;

procedure TGRGraphicControl.Resize;
begin
  inherited Resize;
  InvalidateSelfBuffer;
end;

procedure TGRGraphicControl.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    if Value or Transparent then
    begin
      FBuffer.DrawMode := dmBlend;
      ControlStyle := ControlStyle - [csOpaque]
    end
    else begin
      FBuffer.DrawMode := dmOpaque;
      ControlStyle := ControlStyle + [csOpaque];
    end;
    if not (csLoading in ComponentState) then
    begin
      InvalidateBuffer;
    end;
  end;
end;

procedure TGRGraphicControl.SetAlphaBlendValue(const Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    FBuffer.MasterAlpha := Value;
    if not (csLoading in ComponentState) and FAlphaBlend then
    begin
      InvalidateBuffer;
    end;
  end;
end;

procedure TGRGraphicControl.SetCaptionFont(const Value: TFont32);
begin
  if FCaptionFont <> Value then
    FCaptionFont.Assign(Value);
end;

procedure TGRGraphicControl.SetFrame(const Value: TGRFrame);
begin
  if FFrame <> Value then
    FFrame.Assign(Value);
end;

procedure TGRGraphicControl.SetFrameHot(const Value: TGRFrame);
begin
  if FFrameHot <> Value then
    FFrameHot.Assign(Value);
end;

procedure TGRGraphicControl.SetStyleController(const Value: TGRStyleController);
begin
  if FStyleController <> Value then
  begin
    if Assigned(FStyleController) then
      FStyleController.RemoveFreeNotification(Self);
  FStyleController := Value;
    if Assigned(FStyleController) then
      FStyleController.FreeNotification(Self);
  end;
end;

procedure TGRGraphicControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if Value or AlphaBlend then
    begin
      FBuffer.DrawMode := dmBlend;
      ControlStyle := ControlStyle - [csOpaque];
      //that should be false!
      //FDoubleBuffered := False;
    end
    else begin
      FBuffer.DrawMode := dmOpaque;
      ControlStyle := ControlStyle + [csOpaque];
    end;
    if not (csLoading in ComponentState) then
    begin
      InvalidateBuffer;
    end;
  end;
end;

procedure TGRGraphicControl.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;//}
end;

procedure TGRGraphicControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);

  (*
  {$ifdef debug}
  var
    ClipRect: TRect;
  {$endif}
  //*)

begin
  Message.Result := 1;
end;

procedure TGRGraphicControl.WMSetText(var Message: TMessage);
begin
  inherited;
  {$ifdef debug}
  SendDebug(ClassName+' Caption Set Text:'+ Pchar(Message.LParam));
  {$endif}
  //FBufferDirty := True;
  //FSelfBufferDirty := True;
  InvalidateSelfBuffer;
end;

constructor TGRBGCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := GetBackgroundClass.Create(Self);
  FBackground.OnChanged := DoBackgroundChanged;
end;

destructor TGRBGCustomControl.Destroy;
begin
  FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TGRBGCustomControl.BeforePaintBuffer(aBitmap32: TBitmap32);
var
  LBackground: TGRBackground;
begin
  //Paint the Background.
  aBitmap32.ResetClipRect;
  //if FTransparent then
    //aBitmap32.Clear(0)
  //else
  LBackground := GetBackground;
  if not FTransparent then
  begin
    if LBackground.Enabled then
    LBackground.PaintTo(aBitmap32, aBitmap32.ClipRect)
    else if Color <> clNone then
      aBitmap32.Clear(Color32(Color))
  end
  else if LBackground.Gradient.Enabled then
    //transparent draw the glass mask
    LBackground.Gradient.PaintTo(aBitmap32, aBitmap32.ClipRect);
  inherited BeforePaintBuffer(aBitmap32);
end;

procedure TGRBGCustomControl.DoBackgroundChanged(Sender: TObject);
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateSelfBuffer;
  end;
end;

function TGRBGCustomControl.GetBackground: TGRBackground;
var
  LStyle: TGRStyle;
begin
  LStyle := GetCurrentStyle;
  if Assigned(LStyle) and LStyle.Background.Enabled then
    Result := LStyle.Background
  else
   Result := FBackground;
end;

class function TGRBGCustomControl.GetBackgroundClass: TGRBackgroundClass;
begin
  Result := TGRBackground;
end;

procedure TGRBGCustomControl.SetBackground(const Value: TGRBackground);
begin
  if FBackground <> Value then
    FBackground.Assign(Value);
end;

constructor TGRBGGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := GetBackgroundClass.Create(Self);
  FBackground.OnChanged := DoBackgroundChanged;
end;

destructor TGRBGGraphicControl.Destroy;
begin
  FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TGRBGGraphicControl.BeforePaintBuffer(aBitmap32: TBitmap32);
var
  LBackground: TGRBackground;
begin
  inherited BeforePaintBuffer(aBitmap32);
  //Paint the Background.
  LBackground := GetBackground;
  if not FTransparent then
  begin
    if LBackground.Enabled then
    LBackground.PaintTo(aBitmap32, aBitmap32.ClipRect)
    else if Color <> clNone then
      aBitmap32.Clear(Color32(Color))
  end
  else if LBackground.Gradient.Enabled then
    //transparent draw the glass mask
    LBackground.Gradient.PaintTo(aBitmap32, aBitmap32.ClipRect);
end;

procedure TGRBGGraphicControl.DoBackgroundChanged(Sender: TObject);
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateSelfBuffer;
  end;
end;

function TGRBGGraphicControl.GetBackground: TGRBackground;
var
  LStyle: TGRStyle;
begin
  LStyle := GetCurrentStyle;
  if Assigned(LStyle) and LStyle.Background.Enabled then
    Result := LStyle.Background
  else
   Result := FBackground;
end;

class function TGRBGGraphicControl.GetBackgroundClass: TGRBackgroundClass;
begin
  Result := TGRBackground;
end;

procedure TGRBGGraphicControl.SetBackground(const Value: TGRBackground);
begin
  if FBackground <> Value then
    FBackground.Assign(Value);
end;

constructor TGRStyleController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FNormalStyle := TGRStyle.Create(Self);
  FNormalStyle.OnChanged := DoNormalStyleChanged;

  FHotStyle := TGRStyle.Create(Self);
  FDownStyle := TGRStyle.Create(Self);
end;

destructor TGRStyleController.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FNormalStyle);
  FreeAndNil(FHotStyle);
  FreeAndNil(FDownStyle);
  inherited Destroy;
end;

procedure TGRStyleController.DoNormalStyleChanged(Sender: TObject);
var
  I: Integer;
  LControl: TComponent;
begin
  if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    //Notify all controls here
    if Assigned(FList) then
      for i := 0 to FList.Count - 1 do
      begin
        LControl := TComponent(FList[i]);
        if csDestroying in LControl.ComponentState then continue;
        if LControl is TGRCustomControl then
          TGRCustomControl(LControl).InvalidateSelfBuffer
        else if LControl is TGRGraphicControl then
          TGRGraphicControl(LControl).InvalidateSelfBuffer;
      end;
  end;
end;

procedure TGRStyleController.FreeNotification(AComponent: TComponent);
begin
  if (Owner = nil) or (AComponent.Owner <> Owner) then
  begin
    // Never acquire a reference to a component that is being deleted.
    assert(not (csDestroying in (ComponentState + AComponent.ComponentState)));

    if (AComponent is TGRCustomControl) or (AComponent is TGRGraphicControl) then
    begin
      if FList.IndexOf(AComponent) < 0 then
        FList.Add(aComponent);
    end;
  end;
  inherited FreeNotification(AComponent);
end;

procedure TGRStyleController.RemoveFreeNotification(AComponent: TComponent);
begin
  if FList <> nil then
  begin
    FList.Remove(AComponent);
    if FList.Count = 0 then
    begin
      FList.Free;
      FList := nil;
    end;
  end;
  inherited RemoveFreeNotification(AComponent);
end;

procedure TGRStyleController.SetDownStyle(const Value: TGRStyle);
begin
  if FDownStyle <> Value then
    FDownStyle.Assign(Value);
end;

procedure TGRStyleController.SetHotStyle(const Value: TGRStyle);
begin
  if FHotStyle <> Value then
    FHotStyle.Assign(Value);
end;

procedure TGRStyleController.SetNormalStyle(const Value: TGRStyle);
begin
  if FNormalStyle <> Value then
    FNormalStyle.Assign(Value);
end;


initialization
  GDefaultPopupMenu := TPopupMenu.Create(nil);
finalization
  FreeAndNil(GDefaultPopupMenu);
end.
