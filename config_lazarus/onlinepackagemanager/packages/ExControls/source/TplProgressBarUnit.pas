{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplProgressBarUnit;

interface

{$I AllExControlsRegister.inc}

uses
  SysUtils, Messages, LMessages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Comctrls,
  plUtils;

type

  TplProgressBar = class(TGraphicControl)
  private
    FTransparent: Boolean;
    FSmooth: Boolean;
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FOrientation: TProgressBarOrientation;
    FElementWidth: Integer;
    FElementColor: TColor;
    FBorderColor: TColor;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FStep: Integer;
    procedure SetMin (Value: Integer);
    procedure SetMax (Value: Integer);
    procedure SetPosition (Value: Integer);
    procedure SetStep (Value: Integer);
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetOrientation (Value: TProgressBarOrientation);
    procedure SetSmooth (Value: Boolean);
    procedure CheckBounds;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged (var Message: TLMNoParams); message CM_PARENTCOLORCHANGED;
    procedure SetTransparent (const Value: Boolean);
  protected
    procedure CalcAdvColors;
    procedure DrawElements;
    procedure Paint; override;

  public
    constructor Create (AOwner: TComponent); override;
    procedure StepIt;
    procedure StepBy (Delta: Integer);
  published
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Align;
    property Cursor;
    property Color default $00E1EAEB;
    property ColorElement: TColor index 0 read FElementColor write SetColors default $00996633;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default $008396A0;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Enabled;
    property ParentColor;
    property Visible;
    property Hint;
    property ShowHint;
    property PopupMenu;
    property ParentShowHint;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition default 0;
    property Step: Integer read FStep write SetStep default 10;
    property Smooth: Boolean read FSmooth write SetSmooth default false;

    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

  end;

implementation

constructor TplProgressBar.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 16;
  Width := 147;
  FElementWidth := 8;
  FElementColor := $00996633;
  FBorderColor := $008396A0;
  ParentColor := True;
  Orientation := pbHorizontal;
  FStep := 10;
  FMin := 0;
  FMax := 100;

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorBorder := DefiAdvColorBorder;

  Transparent := false;
  color:=DefiColorFlat;
end;

procedure TplProgressBar.SetOrientation (Value: TProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if (csLoading in ComponentState) then
    begin
      Repaint;
      Exit;
    end;
    SetBounds(Left, Top, Height, Width);
    Invalidate;
  end;
end;

procedure TplProgressBar.SetMin (Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TplProgressBar.SetMax (Value: Integer);
begin
  if FMax <> Value then
  begin
    if Value < FPosition then FPosition := Value;
    FMax := Value;
    Invalidate;
  end;
end;

procedure TplProgressBar.SetPosition (Value: Integer);
begin
  if Value > FMax then Value := FMax;
  if Value < FMin then Value := FMin;
  
  if Value > FPosition then
  begin
    FPosition := Value;
    DrawElements;
  end;
  if Value < FPosition then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TplProgressBar.SetStep (Value: Integer);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    Invalidate;
  end;
end;

procedure TplProgressBar.StepIt;
begin
  if (FPosition + FStep) > FMax then
    FPosition := FMax
  else
    FPosition := FPosition + FStep;
  DrawElements;
end;

procedure TplProgressBar.StepBy (Delta: Integer);
begin
  if (FPosition + Delta) > FMax then
    FPosition := FMax
  else
    FPosition := FPosition + Delta;
  DrawElements;
end;

procedure TplProgressBar.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FElementColor := Value;
    1: FBorderColor := Value;
  end;
  Invalidate;
end;

procedure TplProgressBar.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplProgressBar.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplProgressBar.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplProgressBar.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplProgressBar.CMParentColorChanged (var Message: TLMNoParams);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplProgressBar.SetSmooth(Value: Boolean);
begin
  if Value <> FSmooth then
  begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TplProgressBar.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;


procedure TplProgressBar.CheckBounds;
var
  maxboxes: Word;
begin
  if FOrientation = pbHorizontal then
  begin
    maxboxes := (Width - 3) div (FElementWidth + 1);
    if Width < 12 then
      Width := 12
    else
      Width := maxboxes * (FElementWidth + 1) + 3;
  end
  else
  begin
    maxboxes := (Height - 3) div (FElementWidth + 1);
    if Height < 12 then
      Height := 12
    else
      Height := maxboxes * (FElementWidth + 1) + 3;
  end;
end;

procedure TplProgressBar.Paint;
var PaintRect,r: TRect;
begin
  if not Smooth then
    CheckBounds;
  PaintRect := ClientRect;
  
  // Background
  if not FTransparent then
  begin
    canvas.Brush.Color := Self.Color;
    canvas.Brush.Style := bsSolid;
    canvas.FillRect(PaintRect);
  end;

  if DefiDrawFlat then
   begin
    canvas.Brush.Color := FBorderColor;
    Canvas.FrameRect(PaintRect);
   end else
   begin       
    //----------- Draw 3D ------------------
    r:=PaintRect;
    canvas.pen.Color:=clGray;
    canvas.MoveTo(0, 0);
    canvas.LineTo(R.Right,0);
    canvas.MoveTo(0, 0);
    canvas.LineTo(0, r.Bottom);

    canvas.pen.Color:=clWhite;
    canvas.MoveTo(0, r.Bottom+1);
    canvas.LineTo(R.Right+2, r.Bottom+1);
    canvas.MoveTo(R.Right+1, 0);
    canvas.LineTo(r.Right+1, r.Bottom+1);
   end;

  // Elements
  DrawElements;
end;

procedure TplProgressBar.DrawElements;
var
  NumElements, NumToPaint: LongInt;
  Painted: Byte;
  ElementRect: TRect;
begin
  with canvas do
  begin
    if not Smooth then
    begin
      if FOrientation = pbHorizontal then
      begin
        NumElements := Trunc((ClientWidth - 3) div (FElementWidth + 1));
        NumToPaint := Trunc((FPosition - FMin) / ((FMax - FMin) / NumElements) + 0.00000001);

        if NumToPaint > NumElements then
          NumToPaint := NumElements;


        if BidiMode = bdRightToLeft then
          ElementRect := Rect(ClientRect.Right - 2 - FElementWidth, ClientRect.Top + 2, ClientRect.Right - 2, ClientRect.Bottom - 2)
        else
          ElementRect := Rect(ClientRect.Left + 2, ClientRect.Top + 2, ClientRect.Left + 2 + FElementWidth, ClientRect.Bottom - 2);


        if NumToPaint > 0 then
        begin
          Brush.Color := FElementColor;
          Brush.Style := bsSolid;
          for Painted := 1 to NumToPaint do
          begin
            Canvas.FillRect(ElementRect);

            if BidiMode = bdRightToLeft then
            begin
              ElementRect.Left := ElementRect.Left - FElementWidth - 1;
              ElementRect.Right := ElementRect.Right - FElementWidth - 1;
            end
            else
            begin
             ElementRect.Left := ElementRect.Left + FElementWidth + 1;
             ElementRect.Right := ElementRect.Right + FElementWidth + 1;
            end;

          end;
        end;
      end
      else
      begin
        NumElements := Trunc((ClientHeight - 3) div (FElementWidth + 1));
        NumToPaint := Trunc((FPosition - FMin) / ((FMax - FMin) / NumElements) + 0.00000001);

        if NumToPaint > NumElements then
          NumToPaint := NumElements;
        ElementRect := Rect(ClientRect.Left + 2, ClientRect.Bottom - FElementWidth - 2, ClientRect.Right - 2, ClientRect.Bottom - 2);


        if NumToPaint > 0 then
        begin
          Brush.Color := FElementColor;
          Brush.Style := bsSolid;
          for Painted := 1 to NumToPaint do
          begin
            Canvas.FillRect(ElementRect);
            ElementRect.Top := ElementRect.Top - (FElementWidth + 1);
            ElementRect.Bottom := ElementRect.Bottom - (FElementWidth + 1);
          end;
        end;
      end;
    end
    else
    begin
      if (FOrientation = pbHorizontal) and (FPosition > 0) then
      begin
        Brush.Color := FElementColor;
        Canvas.FillRect(Rect(2, 2, ClientRect.Left + 2 + ((FPosition * (ClientWidth - 4)) div (FMax - FMin)), ClientRect.Bottom - 2));
      end
      else
      begin
        Brush.Color := FElementColor;
        Canvas.FillRect(Rect(2, ClientRect.Bottom - 2 - ((FPosition * (ClientHeight - 4)) div (FMax - FMin)), ClientRect.Right - 2, ClientRect.Bottom - 2));
      end;
    end;
  end;
end;

end.
