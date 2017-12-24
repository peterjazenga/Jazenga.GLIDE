unit bgPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, LResources, LMessages,
  BGRABitmap;

type

  TBGRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap) of object;

  { TBGPanel }

  { TCustomBGPanel }

  TCustomBGPanel = class(TCustomPanel)
  private
    FBGRA: TBGRABitmap;
    FOnRedraw: TBGRedrawEvent;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer);
    procedure RedrawBitmapContent; virtual;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure RedrawBitmap;
    procedure DiscardBitmap;
  public
    property OnRedraw: TBGRedrawEvent read FOnRedraw write FOnRedraw;
    property Bitmap: TBGRABitmap read FBGRA;
  end;

  TBGPanel = class(TCustomBGPanel)
  published
    property OnRedraw;
    property Bitmap;
    property Align;
    property Anchors;
    property Constraints;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  //{$I icons\bgpanel_icon.lrs}
  RegisterComponents('BGRA Games', [TBGPanel]);
end;

{ TBGPanel }

procedure TCustomBGPanel.Paint;
begin
  inherited Paint;
  BGRASetSize(Width, Height);
  FBGRA.Draw(Canvas, 0, 0);
end;

procedure TCustomBGPanel.Resize;
begin
  inherited Resize;
  DiscardBitmap;
end;

procedure TCustomBGPanel.BGRASetSize(AWidth, AHeight: integer);
begin
  if (FBGRA <> nil) and (AWidth <> FBGRA.Width) and (AHeight <> FBGRA.Height) then
  begin
    FBGRA.SetSize(AWidth, AHeight);
    RedrawBitmapContent;
  end;
end;

procedure TCustomBGPanel.RedrawBitmapContent;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    if Assigned(FOnRedraw) then
      FOnRedraw(Self, FBGRA);
  end;
end;

{$hints off}
procedure TCustomBGPanel.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin

end;

{$hints on}

constructor TCustomBGPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
  {$IFDEF WINDOWS}
  DoubleBuffered := True;
  {$ENDIF}
end;

destructor TCustomBGPanel.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

procedure TCustomBGPanel.RedrawBitmap;
begin
  RedrawBitmapContent;
  Repaint;
end;

procedure TCustomBGPanel.DiscardBitmap;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.SetSize(0, 0);
    Invalidate;
  end;
end;

end.
