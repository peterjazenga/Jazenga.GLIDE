
{**********************************************************************
 Package pl_AGGPasVS.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com)
***********************************************************************}
unit agx_lclpaintbox;

interface

{$I agg_vsmode.inc }

uses
  LCLIntf, LMessages,

  {$IFDEF Windows}
  Windows,
  {$ELSE}
  LCLType, LCLProc, Types,
  {$ENDIF}

  Classes,
  Controls, Messages, Graphics, SysUtils,
  agg_basics, agg_color,
  agg_pixelmap, agg_platform_support,
  agg_pixfmt, agg_pixfmt_rgb,
  agg_render_scanlines, agg_renderer_base, agg_rendering_buffer,
  agg_rasterizer_scanline_aa, agg_renderer_scanline, agg_scanline_p,
  agg_ctrl, agg_slider_ctrl, agg_gsv_text, agg_conv_stroke,
  agx_canvas;

type

TAgxPaintBoxBuffer = class(TInterfacedPersistent, IStreamPersist)
  private
    FPixelMap: TBitMap;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
  end;

TAgxCustomPaintBox = class(TCustomControl)
  private
    FAgxEngine: TAgxCanvas;
    FAggColor: aggclr;
    FBuffer: TAgxPaintBoxBuffer;
    FPixelMap: TBitMap;
    FBufferValid: Boolean;
    FMouseInControl: Boolean;

    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
    procedure Resize; override;

    procedure Invalidate; override;
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    property  AggEngine: TAgxCanvas read FAgxEngine;
    property  Buffer: TAgxPaintBoxBuffer read FBuffer;
  end;

TAgxPaintBox = class(TAgxCustomPaintBox)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
  published
    property Align;
    property Anchors;
    property Color;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

resourcestring
  RCStrStreamIsNotAValid = 'Stream is not a valid Bitmap';


function ColorToAggColor(WinColor: TColor): aggclr; overload;
begin
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);

  Result.R := WinColor and $FF;
  Result.G := (WinColor and $FF00) shr 8;
  Result.B := (WinColor and $FF0000) shr 16;
  Result.A := $FF;
end;
//============== TAgxPaintBoxBuffer =============================

constructor TAgxPaintBoxBuffer.Create;
begin
  inherited;
  FPixelMap:=Tbitmap.Create;
  FPixelMap.PixelFormat:=pf32bit;
end;

destructor TAgxPaintBoxBuffer.Destroy;
begin
  FPixelMap.free;
  inherited;
end;

procedure TAgxPaintBoxBuffer.LoadFromFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TAgxPaintBoxBuffer.LoadFromStream(Stream: TStream);
begin
  FPixelMap.LoadFromStream(Stream,Stream.Size);
end;

procedure TAgxPaintBoxBuffer.SaveToFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TAgxPaintBoxBuffer.SaveToStream(Stream: TStream);
begin
 FPixelMap.SaveToStream(Stream);
end;

//============== TAgxCustomPaintBox =============================

constructor TAgxCustomPaintBox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;

  Width := 128;
  Height := 128;

  FBuffer:=TAgxPaintBoxBuffer.Create;

  FPixelMap := FBuffer.FPixelMap;
  FPixelMap.SetSize(Width,Height);


  FAgxEngine:=TAgxCanvas.Create;
  FAgxEngine.Attach(FPixelMap);

  FAggColor := ColorToAggColor(Color);
  FAgxEngine.ClearAll(FAggColor.r ,FAggColor.g ,FAggColor.b, FAggColor.a);
end;

destructor TAgxCustomPaintBox.Destroy;
begin
  FBuffer.Free;
  FAgxEngine.Free;
  inherited;
end;

procedure TAgxCustomPaintBox.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TAgxCustomPaintBox then
  begin
    FPixelMap.Assign(TAgxCustomPaintBox(Dest).FPixelMap);
    TAgxCustomPaintBox(Dest).FBufferValid := FBufferValid;
    TAgxCustomPaintBox(Dest).FOnMouseEnter := FOnMouseEnter;
    TAgxCustomPaintBox(Dest).FOnMouseLeave := FOnMouseLeave;
  end;
end;

procedure TAgxCustomPaintBox.CMColorChanged(var Message: TMessage);
begin
  FAggColor := ColorToAggColor(Color);
  FAgxEngine.ClearAll(FAggColor.r ,FAggColor.g ,FAggColor.b, FAggColor.a);
end;

procedure TAgxCustomPaintBox.Invalidate;
begin
  FAgxEngine.Invalidate;
  FBufferValid := False;
  inherited;
end;

procedure TAgxCustomPaintBox.Loaded;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAgxCustomPaintBox.Paint;
begin
  if not Assigned(Parent) then
    Exit;

  Canvas.Draw(0,0,FPixelMap);
end;

procedure TAgxCustomPaintBox.Resize;
begin
   if FPixelMap<>nil then
    if (Self.Width <> FPixelMap.Width) or (Self.Height <> FPixelMap.Height) then
    begin
      FPixelMap.SetSize(Self.Width, Self.Height);
      FAgxEngine.Attach(FPixelMap);
      FAgxEngine.Viewport(0, 0, Width, Height, 0, 0, Width, Height, AGX_XMidYMid);
      FAgxEngine.ClearAll(FAggColor.r ,FAggColor.g ,FAggColor.b, FAggColor.a);
    end;

  inherited;
end;


//============== TAgxPaintBox ==========================================

procedure TAgxPaintBox.Paint;
begin
  if Assigned(FOnPaint)
    then FOnPaint(Self);

  inherited;
end;

end.
