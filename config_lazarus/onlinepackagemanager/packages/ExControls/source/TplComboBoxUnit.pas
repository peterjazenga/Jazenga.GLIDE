{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplComboBoxUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Messages, LMessages,
  Classes, Forms, Controls, Graphics, StdCtrls, plUtils,
  SysUtils, comctrls ,dialogs;

type

TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;

TplCustomComboBox = class(TCustomComboBox)
  private
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetAdvColors(Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    function  GetButtonRect: TRect;
    procedure PaintButton;
    procedure PaintBorder;
    procedure DrawAll;
    procedure InvalidateSelection;
    function  GetSolidBorder: Boolean;
    procedure SetSolidBorder;
    procedure ListWndProc (var Message: TMessage);
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var TheMessage: TLMCommand); message CN_Command;
    procedure CMFontChanged (var Message: TLMessage); message CM_FONTCHANGED;
    procedure CMSysFontChanged(var Message: TLMessage); message CM_SYSFONTCHANGED;
    procedure CMParentFontChanged(var Message: TLMessage); message CM_PARENTFONTCHANGED;
  protected
    FArrowColor: TColor;
    FArrowBackgroundColor: TColor;
    FBorderColor: TColor;
    FHighlightColor: TColor;
    FUseAdvColors: Boolean;
    FAdvColorArrowBackground: TAdvColors;
    FAdvColorBorder: TAdvColors;
    FAdvColorHighlight: TAdvColors;
    FButtonWidth: Integer;
    FChildHandle: HWND;
    FDefListProc: Pointer;
    FListHandle: HWND;
    FSysBtnWidth: Integer;
    FSolidBorder: Boolean;
    procedure CalcAdvColors;
    procedure WndProc (var Message: TMessage); override;
   // procedure ComboWndProc (var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    property  SolidBorder: Boolean read FSolidBorder;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorArrow: TColor index 0 read FArrowColor write SetColors ;
    property ColorArrowBackground: TColor index 1 read FArrowBackgroundColor write SetColors ;
    property ColorBorder: TColor index 2 read FBorderColor write SetColors ;
    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors ;
    property AdvColorArrowBackground: TAdvColors index 1 read FAdvColorArrowBackground write SetAdvColors ;
    property AdvColorHighlight: TAdvColors index 2 read FAdvColorHighlight write SetAdvColors;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors ;
  published


    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;// Note: windows has a fixed height in some styles
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color default $00E1EAEB;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property OnEndDock;
    property OnStartDock;


  end;

TplComboBox = class(TplCustomComboBox)
  private
   function  GetSelection:String;
   procedure SetSelection(s: String);
  published
   property Selection: String read GetSelection write SetSelection;
end;

implementation   

constructor TplCustomComboBox.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  //ControlStyle := ControlStyle - [csFixedHeight] + [csOpaque];
  TControlCanvas(Canvas).Control := self;
  FButtonWidth := 11;
  FSysBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDefListProc := nil;
  ItemHeight := 13;

  FArrowColor := DefiColorArrow;
  FArrowBackgroundColor := DefiColorArrowBackground;
  FBorderColor := DefiColorBorder;
  FHighlightColor := DefiColorHighlight;

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorBorder := defiAdvColorBorder;
  FAdvColorArrowBackground := defiAdvColorArrowBackground;
  FAdvColorHighlight := DefiAdvColorHighlight;
  Style := csOwnerDrawFixed;

end;

destructor TplCustomComboBox.Destroy;
begin
  inherited;
end;

procedure TplCustomComboBox.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FArrowColor := Value;
    1: FArrowBackgroundColor := Value;
    2: FBorderColor := Value;
    3: FHighlightColor := Value;
  end;
  Invalidate;
end;

procedure TplCustomComboBox.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(TForm(Parent).Color, FBorderColor, FAdvColorBorder, darken);
    FArrowBackgroundColor := CalcAdvancedColor(TForm(Parent).Color, FArrowBackgroundColor, FAdvColorArrowBackground, darken);
  end;
end;

procedure TplCustomComboBox.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
    1: FAdvColorArrowBackground := Value;
    2: FAdvColorHighlight := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplCustomComboBox.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplCustomComboBox.CMSysFontChanged(var Message: TLMessage);
begin
  if FUseAdvColors then
    CalcAdvColors;
  Invalidate;
end;

procedure TplCustomComboBox.CMParentFontChanged(var Message: TLMessage);
begin
  if FUseAdvColors then
    CalcAdvColors;
  Invalidate;
end;

procedure TplCustomComboBox.WndProc (var Message: TMessage);
begin
  if (Message.Msg = LM_PARENTNOTIFY) then
    case LoWord(Message.wParam) of
      WM_CREATE:
        if FDefListProc <> nil then
        begin
          SetWindowLong(FListHandle, GWL_WNDPROC, PtrInt(FDefListProc));
          FDefListProc := nil;
          FChildHandle := Message.lParam;
        end
        else
          if FChildHandle = 0 then
            FChildHandle := Message.lParam else
            FListHandle := Message.lParam;
      end
  else

    if (Message.Msg = LM_WINDOWPOSCHANGING) then
      if Style in [csDropDown, csSimple] then
        SetWindowPos( Handle, 0,
          0, 0, ClientWidth - FButtonWidth - 2 * 2 - 4, Height - 2 * 2 - 2,
          SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_NOREDRAW);

  inherited;
  if Message.Msg = LM_CTLCOLORLISTBOX then
  begin
    SetBkColor(Message.wParam, ColorToRGB(Color));
    Message.Result := CreateSolidBrush(ColorToRGB(Color));
  end;
end;

procedure TplCustomComboBox.ListWndProc (var Message: TMessage);
begin
  case Message.Msg of
    LM_WINDOWPOSCHANGING:
      with TLMWindowPosMsg(Message).WindowPos^ do
      begin
        // size of the drop down list
        if Style in [csDropDown, csDropDownList] then
          cy := (GetFontHeight(Font)-2) * Min(DropDownCount, Items.Count) + 4
        else
          cy := (ItemHeight) * Min(DropDownCount, Items.Count) + 4;

        if cy <= 4  then cy := 10;
      end;
    else
      with Message do
        Result := CallWindowProc(FDefListProc, FListHandle, Msg, WParam, LParam);
  end;
end;
         {
procedure TplCustomComboBox.ComboWndProc (var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  inherited;
  if (ComboWnd = EditHandle) then
    case Message.Msg of
      WM_SETFOCUS, WM_KILLFOCUS:
        SetSolidBorder;
    end;
end;    }

procedure TplCustomComboBox.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    SetSolidBorder;
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
end;

procedure TplCustomComboBox.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    SetSolidBorder;
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
end;

procedure TplCustomComboBox.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplCustomComboBox.WMKeyDown(var Message: TLMKeyDown);
var
  S: String;
begin
  S := Text;
  inherited;
  if not (Style in [csSimple, csDropDown]) and (Text <> S) then
    InvalidateSelection;
end;

procedure TplCustomComboBox.CNCommand(var TheMessage: TLMCommand);
var
  R: TRect;
begin
  inherited;
  if TheMessage.NotifyCode in [9, CBN_DROPDOWN, CBN_SELCHANGE] then
  begin
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection;
  end;
  if (TheMessage.NotifyCode in [CBN_CLOSEUP]) then
  begin
    R := GetButtonRect;
    Dec(R.Left, 2);
    InvalidateRect(Handle, @R, FALSE);
  end;
end;

procedure TplCustomComboBox.WMPaint(var Msg: TLMPaint);
var
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
begin

  DC := BeginPaint(Handle, PS);
  try
    R := PS.rcPaint;
    if R.Right > Width - FButtonWidth - 4 then
      R.Right := Width - FButtonWidth - 4; 

    FillRect(DC, R, Brush.Handle);
    if RectInRect(GetButtonRect, PS.rcPaint) then PaintButton;

    ExcludeClipRect(DC, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);

    PaintWindow(DC);
    if (Style = csDropDown) and DroppedDown then
    begin
      R := ClientRect;
      InflateRect(R, -2, -2);
      R.Right := Width - FButtonWidth - 3;
      Canvas.Brush.Color := clWindow;
      Canvas.FrameRect(R);
    end else
    begin
      if Style <> csDropDown then  InvalidateSelection;
    end;


  finally
    EndPaint(Handle, PS);
  end;
  DrawAll;
  Msg.Result := 0;
end;

procedure TplCustomComboBox.WMNCPaint (var Message: TMessage);
begin
  inherited;
  DrawAll;
end;

procedure TplCustomComboBox.CMFontChanged (var Message: TMessage);
begin
  inherited;
  ItemHeight := 13;
  RecreateWnd(Self); { *Converted from RecreateWnd* } { *Converted from RecreateWnd* }
end;

procedure TplCustomComboBox.InvalidateSelection;
var
  R: TRect;
begin
  R := ClientRect;
  InflateRect(R, -2, -3);
  R.Left := R.Right - FButtonWidth - 8;
  Dec(R.Right, FButtonWidth + 3);
  if (GetFocus = Handle) and not DroppedDown then
    Canvas.Brush.Color := FHighLightcolor
  else
    Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);
  if (GetFocus = Handle) and not DroppedDown then
  begin
    R := ClientRect;
    InflateRect(R, -3, -3);
    Dec(R.Right, FButtonWidth + 2);
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWindow;
  end;
  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);
end;

function TplCustomComboBox.GetButtonRect: TRect;
begin
  GetWindowRect(Handle, Result);
  OffsetRect(Result, -Result.Left, -Result.Top);
  Inc(Result.Left, ClientWidth - FButtonWidth);
  OffsetRect(Result, -1, 0);
end;


procedure TplCustomComboBox.SetSolidBorder;
var
  sb: Boolean;
begin
  sb := GetSolidBorder;
  if sb <> FSolidBorder then
  begin
    FSolidBorder := sb;
    DrawAll;
  end;
end;


function TplCustomComboBox.GetSolidBorder: Boolean;
begin
  Result := ( (csDesigning in ComponentState) and Enabled) or
    (not(csDesigning in ComponentState) and
    (DroppedDown or (GetFocus = Handle) or (GetFocus = Handle)) );
end;

//==================================================================

procedure TplCustomComboBox.PaintButton;
var
  R: TRect;
  x, y: Integer;
begin
  R := GetButtonRect;
  r:=Rect(r.Left,r.Top,r.Right+1,r.Bottom);
  Canvas.Brush.Color := FBorderColor;
  Canvas.FrameRect(R);

  GradientFillRect(Canvas,Rect(r.Left+1,r.Top+1,r.Right-1,r.Bottom-2),
                   DefiBtnFromColor,DefiBtnToColor,fdTopToBottom,32);
  
  x := (R.Right - R.Left) div 2 - 6 + R.Left;
  if DroppedDown then
    y := (R.Bottom - R.Top) div 2 - 1 + R.Top  else
    y := (R.Bottom - R.Top) div 2 - 1 + R.Top;

  if Enabled then
  begin
    canvas.Brush.Color := FArrowColor;
    canvas.Pen.Color := FArrowColor;

    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]) else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);

  end
  else
  begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])  else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
    Dec(x); Dec(y);
    canvas.Brush.Color := clGray;
    canvas.Pen.Color := clGray;
    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]) else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end;

   ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth, 0, ClientWidth, ClientHeight);
end;

procedure TplCustomComboBox.PaintBorder;
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
  aPen,OldPen:HPEN;
begin
  DC := GetDC(Handle);

  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  Dec(R.Right, FButtonWidth + 1);
  try
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
    WindowBrush := CreateSolidBrush(ColorToRGB(Color));

    FrameRect(DC, R, BtnFaceBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);

    ///----------- Draw 3D ------------------
    if DefiDrawFlat=false then
     begin
      aPen:=CreatePen(PS_SOLID,1,clGray);
      OldPen:=SelectObject(dc, aPen);
      MoveToEx(dc, 0, 0, nil);
      LineTo(dc, R.Right, 0);
      MoveToEx(dc, 0, 0, nil);
      LineTo(dc, 0, r.Bottom);
      SelectObject(dc, OldPen);
      DeleteObject(aPen);

      aPen:=CreatePen(PS_SOLID,1,clWhite);
      OldPen:=SelectObject(dc, aPen);
      MoveToEx(dc, 0, r.Bottom+1, nil);
      LineTo(dc, R.Right+2, r.Bottom+1);
      MoveToEx(dc, R.Right+1, 0, nil);
      LineTo(dc, r.Right+1, r.Bottom+1);
      SelectObject(dc, OldPen);
      DeleteObject(aPen);
     end;

  finally
    ReleaseDC(Handle, DC);
  end;
  DeleteObject(WindowBrush);
  DeleteObject(BtnFaceBrush);
end;

procedure TplCustomComboBox.DrawAll;
begin
  PaintBorder;
  if Style <> csSimple then PaintButton;
end;

//================================================================


function TplComboBox.GetSelection:TFontName;
begin
 result:='';
 if (csDesigning in ComponentState) then Exit;  // ct9999

 result:=Items.Strings[ItemIndex];
end;

procedure TplComboBox.SetSelection(s: TFontName);
var
 i: integer;
begin
 if s = '' then
  ItemIndex := -1
 else
  for i := 0 to Items.Count - 1 do
   if SameText(s, Items.Strings[i]) then
    begin
     ItemIndex := i;
     Exit;
    end;
end;

end.

