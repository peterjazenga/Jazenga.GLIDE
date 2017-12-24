
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplMemoUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Controls, Forms, Graphics, StdCtrls, plUtils;

type
  TplMemo = class(TCustomMemo)
  private
    FUseAdvColors: Boolean;
    FAdvColorFocused: TAdvColors;
    FAdvColorBorder: TAdvColors;
    FParentColor: Boolean;
    FFocusedColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    MouseInControl: Boolean;
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetParentColor (Value: Boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
   // procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  protected
    procedure CalcAdvColors;
    procedure RedrawBorder (const Clip: HRGN);
  public
    constructor Create (AOwner: TComponent); override;

    property ColorFocused: TColor index 0 read FFocusedColor write SetColors ;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors ;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors ;

    property AdvColorFocused: TAdvColors index 0 read FAdvColorFocused write SetAdvColors ;
    property AdvColorBorder: TAdvColors index 1 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors ;
  published

    property ParentColor: Boolean read FParentColor write SetParentColor default false;      
    property Align;
    property Alignment;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ScrollBars;
    property TabOrder;
    property TabStop;
    property Visible;
    property Lines;
    property WantReturns;
    property WantTabs;
    property WordWrap;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

constructor TplMemo.Create (AOwner: TComponent);
begin
  inherited;
  ParentFont := True;


  FFocusedColor := DefiColorFocused;
  FBorderColor := DefiColorBorder;
  FFlatColor := DefiColorFlat;
  FParentColor := false;

  AutoSize := False;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csFramed];
  SetBounds(0, 0, 185, 89);
  
  FUseAdvColors := DefiUseAdvColors;
  FAdvColorBorder := DefiAdvColorBorder;
  FAdvColorFocused := DefiAdvColorFocused;
end;

procedure TplMemo.SetParentColor (Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
      RedrawBorder(0);
    end;
  end;
end;

procedure TplMemo.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    if Parent <> nil then
      FFlatColor := TForm(Parent).Color;
    CalcAdvColors;
  end
  else
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
    end;
  RedrawBorder(0);
end;

procedure TplMemo.CMParentColorChanged(var Message: TLMessage);
begin
  if FUseAdvColors then
  begin
    if Parent <> nil then
      FFlatColor := TForm(Parent).Color;
    CalcAdvColors;
  end
  else
    if FParentColor then
    begin
      if Parent <> nil then
        FFlatColor := TForm(Parent).Color;
    end;
  RedrawBorder(0);
end;

procedure TplMemo.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FBorderColor := Value;
    2: FFlatColor := Value;
  end;
  if Index = 2 then
    FParentColor := False;
  RedrawBorder(0);
end;

procedure TplMemo.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FFocusedColor := CalcAdvancedColor(FFlatColor, FFocusedColor, FAdvColorFocused, lighten);
    FBorderColor := CalcAdvancedColor(FFlatColor, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplMemo.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorFocused := Value;
    1: FAdvColorBorder := Value;
  end;
  if FUseAdvColors then
  begin
    CalcAdvColors;
    RedrawBorder(0);
  end;
end;

procedure TplMemo.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    RedrawBorder(0);
  end;
end;

procedure TplMemo.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    MouseInControl := True;
    RedrawBorder(0);
  end;
end;

procedure TplMemo.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  MouseInControl := False;
  RedrawBorder(0);
end;

procedure TplMemo.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  RedrawBorder(0);
end;

procedure TplMemo.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TplMemo.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;
   {
procedure TplMemo.WMNCCalcSize (var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end; }

procedure TplMemo.WMNCPaint (var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TplMemo.RedrawBorder (const Clip: HRGN);
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, WindowBrush, FocusBrush: HBRUSH;
  aPen,OldPen:HPEN;
begin
  DC := GetDC(Handle);
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
    WindowBrush := CreateSolidBrush(ColorToRGB(FFlatColor));
    FocusBrush := CreateSolidBrush(ColorToRGB(FFocusedColor));

    if (not(csDesigning in ComponentState) and
      (Focused or (MouseInControl and not(Screen.ActiveControl is TplMemo)))) then
    begin
      { Focus }
      Color := FFocusedColor;
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, FocusBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, FocusBrush);
      if ScrollBars = ssBoth then
        FillRect(DC, Rect(R.Right - 17, R.Bottom - 17, R.Right - 1, R.Bottom - 1), FocusBrush);
    end
    else
    begin
      { non Focus }
      Color := FFlatColor;
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, WindowBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, WindowBrush);
      if ScrollBars = ssBoth then
        FillRect(DC, Rect(R.Right - 17, R.Bottom - 17, R.Right - 1, R.Bottom - 1), WindowBrush);
    end;

    //----------- Draw 3D ------------------
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
  DeleteObject(FocusBrush);
end;

end.
