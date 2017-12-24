
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplMaskEditUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Controls, Forms, Graphics, StdCtrls, SysUtils,
  plUtils, MaskEdit;

type
  TValidateEvent = Procedure ( Sender : TObject) of Object;

type
  TplMaskEdit = class(TCustomMaskEdit)
  private
    FUseAdvColors: Boolean;
    FAdvColorFocused: TAdvColors;
    FAdvColorBorder: TAdvColors;
    FParentColor: Boolean;
    FFocusedColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FOnValidate : TValidateEvent;
    MouseInControl: Boolean;
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetParentColor (Value: Boolean);
    procedure RedrawBorder (const Clip: HRGN);
    procedure NewAdjustHeight;
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
   // procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure CalcAdvColors;
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
   {.$IFDEF DFS_DELPHI_4_UP}
    procedure ValidateEdit; override;
   {.$ENDIF}


    property ColorFocused: TColor index 0 read FFocusedColor write SetColors;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors;
    property ColorFlat: TColor index 2 read FFlatColor write SetColors ;
    property AdvColorFocused: TAdvColors index 0 read FAdvColorFocused write SetAdvColors ;
    property AdvColorBorder: TAdvColors index 1 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors default false;

  published
   
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    
    property Align;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
  //  property Color;
   // property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property HideSelection;
    property MaxLength;
  //  property OEMConvert;
  //  property ParentCtl3D;
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
    property OnValidate : TValidateEvent read FOnValidate write FOnValidate;

    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

  end;

implementation

constructor TplMaskEdit.Create (AOwner: TComponent);
begin
  inherited;
  ParentFont := True;
  FParentColor := false;   

  FFocusedColor := DefiColorFocused;
  FBorderColor := DefiColorBorder;
  FFlatColor := DefiColorFlat;

  
  AutoSize := False;
  //Ctl3D := False;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csFramed];
  SetBounds(0, 0, 121, 16);

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorBorder := DefiAdvColorBorder;
  FAdvColorFocused := DefiAdvColorFocused;
  Height:=16;
end;

procedure TplMaskEdit.SetParentColor (Value: Boolean);
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

procedure TplMaskEdit.CMSysColorChange (var Message: TMessage);
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

procedure TplMaskEdit.CMParentColorChanged(var Message: TLMessage);
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

procedure TplMaskEdit.SetColors (Index: Integer; Value: TColor);
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

procedure TplMaskEdit.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FFocusedColor := CalcAdvancedColor(FFlatColor, FFocusedColor, FAdvColorFocused, lighten);
    FBorderColor := CalcAdvancedColor(FFlatColor, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplMaskEdit.SetAdvColors (Index: Integer; Value: TAdvColors);
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

procedure TplMaskEdit.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    RedrawBorder(0);
  end;
end;

procedure TplMaskEdit.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    MouseInControl := True;
    RedrawBorder(0);
  end;
end;

procedure TplMaskEdit.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  MouseInControl := False;
  RedrawBorder(0);
end;

procedure TplMaskEdit.NewAdjustHeight;
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

  Height := Metrics.tmHeight + 6;
end;

procedure TplMaskEdit.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TplMaskEdit.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor= (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  RedrawBorder(0);
end;

procedure TplMaskEdit.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TplMaskEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;

procedure TplMaskEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder(0);
end;
          {
procedure TplMaskEdit.WMNCCalcSize (var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;    }

procedure TplMaskEdit.WMNCPaint (var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TplMaskEdit.RedrawBorder (const Clip: HRGN);
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
      (Focused or (MouseInControl and not(Screen.ActiveControl is TplMaskEdit)))) then
    begin
      { Focus }
      Color := FFocusedColor;
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, FocusBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, FocusBrush);
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

procedure TplMaskEdit.ValidateEdit;
begin
  If Assigned(FOnValidate) Then
    FOnValidate(Self)
  else
    inherited;
end;

end.
