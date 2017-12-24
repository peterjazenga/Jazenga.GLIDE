
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplEditUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Controls, Forms, Graphics, StdCtrls, SysUtils,
  plUtils;

type
  TCustomFlatEdit = class(TCustomEdit)
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
    procedure RedrawBorder (const Clip: HRGN);
    procedure NewAdjustHeight;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
   // procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged (var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
  protected
    procedure CalcAdvColors;
    procedure Loaded; override;

    property ColorFocused: TColor index 0 read FFocusedColor write SetColors ;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors ;
    property AdvColorFocused: TAdvColors index 0 read FAdvColorFocused write SetAdvColors;
    property AdvColorBorder: TAdvColors index 1 read FAdvColorBorder write SetAdvColors;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors default false;


    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property CharCase;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
   // property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

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

  public
    constructor Create (AOwner: TComponent); override;
  end;

  TplEdit = class(TCustomFlatEdit)
  published

    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property CharCase;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
  //  property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

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

constructor TCustomFlatEdit.Create (AOwner: TComponent);
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
  SetBounds(0, 0, 121, 16);

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorBorder := DefiAdvColorBorder;
  FAdvColorFocused := DefiAdvColorFocused;
  Height:=16;  
end;

procedure TCustomFlatEdit.SetParentColor (Value: Boolean);
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

procedure TCustomFlatEdit.CMSysColorChange (var Message: TMessage);
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

procedure TCustomFlatEdit.CMParentColorChanged (var Message: TWMNoParams);
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

procedure TCustomFlatEdit.SetColors (Index: Integer; Value: TColor);
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

procedure TCustomFlatEdit.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FFocusedColor := CalcAdvancedColor(FFlatColor, FFocusedColor, FAdvColorFocused, lighten);
    FBorderColor := CalcAdvancedColor(FFlatColor, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TCustomFlatEdit.SetAdvColors (Index: Integer; Value: TAdvColors);
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

procedure TCustomFlatEdit.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    RedrawBorder(0);
  end;
end;

procedure TCustomFlatEdit.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    MouseInControl := True;
    RedrawBorder(0);
  end;
end;

procedure TCustomFlatEdit.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  MouseInControl := False;
  RedrawBorder(0);
end;

procedure TCustomFlatEdit.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Height := Metrics.tmHeight + 4;
end;

procedure TCustomFlatEdit.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TCustomFlatEdit.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor= (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  RedrawBorder(0);
end;

procedure TCustomFlatEdit.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TCustomFlatEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TCustomFlatEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TCustomFlatEdit.WMNCPaint (var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TCustomFlatEdit.RedrawBorder (const Clip: HRGN);
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, FillBrush: HBRUSH;
  aPen,OldPen:HPEN;
  iH:Integer;
begin
  DC := GetDC(Handle);
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
    if DefiDrawFlat then iH:=-1 else iH:=0;

    if (not(csDesigning in ComponentState) and
       (Focused or (MouseInControl and not(Screen.ActiveControl is TplEdit)))) then
    begin
      Color := FFocusedColor;
      FillBrush := CreateSolidBrush(ColorToRGB(FFocusedColor)); // Focus
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, iH);
      FrameRect(DC, R, FillBrush);
      InflateRect(R, -1, 0);
      FrameRect(DC, R, FillBrush);
    end
    else
    begin
      Color := FFlatColor;
      FillBrush := CreateSolidBrush(ColorToRGB(FFlatColor));   // non Focus
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, iH);
      FrameRect(DC, R, FillBrush);
      InflateRect(R, -1, 0);
      FrameRect(DC, R, FillBrush);
    end;
          
    //----------- Draw 3D ------------------

    if DefiDrawFlat=false then
     begin
       GetWindowRect(Handle, R);
       OffsetRect(R, -R.Left-2, -R.Top-2);

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
  DeleteObject(BtnFaceBrush);
  DeleteObject(FillBrush);
end;


end.
