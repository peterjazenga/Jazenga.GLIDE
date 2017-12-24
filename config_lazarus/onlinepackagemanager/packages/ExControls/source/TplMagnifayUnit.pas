
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplMagnifayUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TplMagnifay = class(TGraphicControl)
  private
    FActive   : boolean;
    FAtDesign : boolean;
    FZoom     : integer;
    FTimer            : TTimer;
    FZoomedX,FZoomedY : integer;
    FVirtualBmp       : TBitmap;
    FGetDC,FDesktopDC : HDC;
    FDesktopBmp       : HDC;
    procedure SetActive(Value : boolean);
    procedure SetAtDesign(Value :boolean);
    procedure SetZoom(Value : integer);
  protected
    procedure Paint; override;
    procedure BuildVirtualBmp;
    procedure PaintOnVirtualDC(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(const Filename : string);
    procedure SetBounds(ALeft,ATop,AWidth,AHeight : integer); override;
  published
    property Active : boolean read FActive write SetActive default false;
    property AtDesign : boolean read FAtDesign write SetAtDesign default false;
    property Zoom : integer read FZoom write SetZoom default 1;
  end;


implementation



constructor TplMagnifay.Create(AOwner : TComponent);
begin
  // These lines are before the inherited Create because the inherited Create
  // calls SetBounds that uses these variables, Don't move them !
  FZoom:=1;
  FVirtualBmp:=TBitmap.Create;
  //FGetDC:=CreateDC('DISPLAY',nil,nil,nil);
  //FDesktopDC:=CreateCompatibleDC(FGetDC);
  inherited Create(AOwner);
  ControlStyle:=[csClickEvents,csCaptureMouse,csOpaque,csDoubleClicks];
  Width:=100;
  Height:=100;
  FActive:=false;
  FAtDesign:=false;
  FTimer:=TTimer.Create(Self);
  with FTimer do begin
    Enabled:=false;
    Interval:=100;
    OnTimer:=PaintOnVirtualDC;
  end;
  BuildVirtualBmp;
end;


destructor TplMagnifay.Destroy;
begin
  DeleteDC(FDesktopBmp);
  DeleteDC(FDesktopDC);
  DeleteDC(FGetDC);
  FVirtualBmp.Free;
  inherited Destroy;
end;


procedure TplMagnifay.SetActive(Value : boolean);
begin
  FActive:=Value;
  FTimer.Enabled:=FActive;
end;


procedure TplMagnifay.SetAtDesign(Value :boolean);
begin
  FAtDesign:=Value;
  Repaint;
end;


procedure TplMagnifay.SetZoom(Value : integer);
begin
  if Value<>FZoom then begin
    if Value>0 then FZoom:=Value else FZoom:=1;
    BuildVirtualBmp;
  end;
end;


procedure TplMagnifay.SetBounds(ALeft,ATop,AWidth,AHeight : integer);
begin
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
  BuildVirtualBmp;
end;


procedure TplMagnifay.Paint;
begin
  if (csDesigning in ComponentState) and not FAtDesign then with Canvas do begin
    Brush.Color:=clBtnFace;
    FillRect(ClipRect);
    Brush.Color:=clBlack;
    FrameRect(ClipRect);
  end else with Canvas,Canvas.ClipRect do
    BitBlt(Handle,Left,Top,Right,Bottom,FVirtualBmp.Canvas.Handle,
      Left,Top,SRCCOPY);
end;


procedure TplMagnifay.BuildVirtualBmp;
begin
  FVirtualBmp.Width:=Width;
  FVirtualBmp.Height:=Height;
  FZoomedX:=round(Width/FZoom);
  FZoomedY:=round(Height/FZoom);
  if FDesktopBmp<>0 then DeleteDC(FDesktopBmp);
  FDesktopBmp:=CreateCompatibleBitmap(FGetDC,FZoomedX,FZoomedY);
  SelectObject(FDesktopDC,FDesktopBmp);
end;


procedure TplMagnifay.PaintOnVirtualDC(Sender : TObject);
var
  CurPos : TPoint;
begin
  if not (csDesigning in ComponentState) or (FAtDesign) then begin
    GetCursorPos(CurPos);
    // Get rect so that cursor point will be in the middle
    with CurPos do BitBlt(FDesktopDC,0,0,Width,Height,FGetDC,X-(FZoomedX div 2),
      Y-(FZoomedY div 2),SRCCOPY);
    StretchBlt(FVirtualBmp.Canvas.Handle,0,0,Width,Height,FDesktopDC,0,0,
      FZoomedX,FZoomedY,SRCCOPY);
     Repaint;
  end;
end;


procedure TplMagnifay.SaveToFile(const Filename : string);
begin
  FTimer.Enabled:=false;
  FVirtualBmp.SaveToFile(Filename);
  FTimer.Enabled:=FActive;
end;

end.
