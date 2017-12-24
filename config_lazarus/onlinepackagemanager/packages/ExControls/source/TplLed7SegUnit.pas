
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLed7SegUnit;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TSegmentShape = (ssRectangle, ssEdge, ssDoubleEdge);

type
  TplLed7Seg = class(TGraphicControl)
  private
    { Private declarations }
    FBrightColor: TColor;
    FDimColor: TColor;
    FBackColor: TColor;
    FValue: byte;
    FSpacing: byte;
    FGap: byte;
    FSegWidth: byte;
    FSegShape: TSegmentShape;
    FSegPoints: array[0..7, 0..5] of TPoint;
    FHorzSegLength: integer;
    FVertSegLength: integer;
    procedure SetBackColor(const Value: TColor);
    procedure SetBrightColor(const Value: TColor);
    procedure SetDimColor(const Value: TColor);
    procedure SetValue(const Value: byte);
    procedure SetSpacing(const Value: byte);
    procedure SetSegWidth(const Value: byte);
    procedure SetSegShape(const Value: TSegmentShape);
    procedure Geometry;
    procedure GeometryRectangle;
    procedure GeometryEdge;
    procedure GeometryDoubleEdge;
    procedure SetGap(const Value: byte);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BrightColor: TColor read FBrightColor write SetBrightColor;
    property DimColor: TColor read FDimColor write SetDimColor;
    property BackColor: TColor read FBackColor write SetBackColor;
    property Value: byte read FValue write SetValue;
    property Spacing: byte read FSpacing write SetSpacing;
    property Gap: byte read FGap write SetGap;
    property SegWidth: byte read FSegWidth write SetSegWidth;
    property SegShape: TSegmentShape read FSegShape write SetSegShape;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Cursor;
    property Enabled;
    property Visible;

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

const
  KValue2Segment: array[0..9, 0..6] of boolean = (
    (True, True, True, True, True, True, False),
    (False, True, True, False, False, False, False),
    (True, True, False, True, True, False, True),
    (True, True, True, True, False, False, True),
    (False, True, True, False, False, True, True),
    (True, False, True, True, False, True, True),
    (True, False, True, True, True, True, True),
    (True, True, True, False, False, False, False),
    (True, True, True, True, True, True, True),
    (True, True, True, True, False, True, True)
    );


implementation

{ TplLed7Seg }

constructor TplLed7Seg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  // Default Value for all properties
  ControlStyle := ControlStyle + [csOpaque];
  FValue := 0;
  FSegWidth := 10;
  FSegShape := ssDoubleEdge;
  FSpacing := 4;
  FGap := 2;
  Width := 100;
  Height := 140;
  FBrightColor := cllime;
  FDimColor := clGreen;
  FBackColor := clBlack;
end;

destructor TplLed7Seg.Destroy;
begin
  inherited Destroy;
end;

procedure TplLed7Seg.Geometry;
begin
  case FSegShape of
    ssRectangle: GeometryRectangle;
    ssEdge: GeometryEdge;
    ssDoubleEdge: GeometryDoubleEdge;
  end;
end;

procedure TplLed7Seg.GeometryDoubleEdge;
var
  swb2, segLength, orgX, orgY: integer;
begin
  if (Width = 0) or (Height = 0) then
    Exit;
  // Horizontal Segment Length
  // Maintain sufficient segment length
  // increase width if necessary
  // room for space on left and right and
  // room for vertical segments on the left and right
  segLength := Width - FSpacing - FSpacing - FGap - FGap - FSegWidth;
  if segLength < 5 + FSegWidth + FSegWidth then
  begin
    Width := Width + (5 + FSegWidth + FSegWidth - segLength);
    Exit;
  end;
  FHorzSegLength := segLength;

  // Vertical Segment Length
  // Maintain sufficient segment length
  // increase height if necessary
  // room for space on top and bottom and
  // room for horizontal segments on the top, middle and bottom
  segLength := Height - FSpacing - FSpacing - FGap - FGap - FGap - FGap - FSegWidth;
  segLength := segLength div 2;
  if segLength < 5 + FSegWidth + FSegWidth then
  begin
    Height := Height + (2 * (5 + FSegWidth + FSegWidth - segLength));
    Exit;
  end;
  FVertSegLength := segLength;

  swb2 := FSegWidth div 2;
  // Points for segment a
  orgX := FSpacing + FGap + swb2;
  orgY := FSpacing + swb2;
  FSegPoints[0, 0] := Point(orgX, orgY);
  FSegPoints[0, 1] := Point(orgX + swb2, orgY + swb2);
  FSegPoints[0, 2] := Point(orgX + FHorzSegLength - swb2, orgY + swb2);
  FSegPoints[0, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[0, 4] := Point(orgX + FHorzSegLength - swb2, orgY - swb2);
  FSegPoints[0, 5] := Point(orgX + swb2, orgY - swb2);

  // Points for segment b
  orgX := FSpacing + FGap + swb2 + FHorzSegLength + FGap;
  orgY := FSpacing + FGap + swb2;
  FSegPoints[1, 0] := Point(orgX, orgY);
  FSegPoints[1, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[1, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[1, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[1, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[1, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment c
  orgX := FSpacing + FGap + swb2 + FHorzSegLength + FGap;
  orgY := FSpacing + FGap + swb2 + FGap + FGap + FVertSegLength + FGap;
  FSegPoints[2, 0] := Point(orgX, orgY);
  FSegPoints[2, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[2, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[2, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[2, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[2, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment d
  orgX := FSpacing + FGap + swb2;
  orgY := FSpacing + swb2 + FGap + FVertSegLength + FGap + FGap + FVertSegLength + FGap;
  FSegPoints[3, 0] := Point(orgX, orgY);
  FSegPoints[3, 1] := Point(orgX + swb2, orgY + swb2);
  FSegPoints[3, 2] := Point(orgX + FHorzSegLength - swb2, orgY + swb2);
  FSegPoints[3, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[3, 4] := Point(orgX + FHorzSegLength - swb2, orgY - swb2);
  FSegPoints[3, 5] := Point(orgX + swb2, orgY - swb2);

  // Points for segment e
  orgX := FSpacing + swb2;
  orgY := FSpacing + FGap + swb2 + FGap + FGap + FVertSegLength + FGap;
  FSegPoints[4, 0] := Point(orgX, orgY);
  FSegPoints[4, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[4, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[4, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[4, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[4, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment f
  orgX := FSpacing + swb2;
  orgY := FSpacing + FGap + swb2;
  FSegPoints[5, 0] := Point(orgX, orgY);
  FSegPoints[5, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[5, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[5, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[5, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[5, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment g
  orgX := FSpacing + FGap + swb2;
  orgY := FSpacing + swb2 + FGap + FVertSegLength + FGap;
  FSegPoints[6, 0] := Point(orgX, orgY);
  FSegPoints[6, 1] := Point(orgX + swb2, orgY + swb2);
  FSegPoints[6, 2] := Point(orgX + FHorzSegLength - swb2, orgY + swb2);
  FSegPoints[6, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[6, 4] := Point(orgX + FHorzSegLength - swb2, orgY - swb2);
  FSegPoints[6, 5] := Point(orgX + swb2, orgY - swb2);
end;

procedure TplLed7Seg.GeometryEdge;
var
  segLength, orgX, orgY: integer;
begin
  if (Width = 0) or (Height = 0) then
    Exit;
  // Horizontal Segment Length
  // Maintain sufficient segment length
  // increase width if necessary
  // room for space on left and right and
  // room for vertical segments on the left and right
  segLength := Width - FSpacing - FSpacing - FGap - FGap;
  if segLength < 5 + FSegWidth + FSegWidth then
  begin
    Width := Width + (5 + FSegWidth + FSegWidth - segLength);
    Exit;
  end;
  FHorzSegLength := segLength;

  // Vertical Segment Length
  // Maintain sufficient segment length
  // increase height if necessary
  // room for space on top and bottom and
  // room for horizontal segments on the top, middle and bottom
  segLength := Height - FSpacing - FSpacing - FGap - FGap - FGap - FGap;
  segLength := segLength div 2;
  if segLength < 5 then
  begin
    Height := Height + (2 * (5 - segLength));
    Exit;
  end;
  FVertSegLength := segLength;

  // Points for segment a
  orgX := FSpacing + FGap;
  orgY := FSpacing;
  FSegPoints[0, 0] := Point(orgX, orgY);
  FSegPoints[0, 1] := Point(orgX + FSegWidth, orgY + FSegWidth);
  FSegPoints[0, 2] := Point(orgX + FHorzSegLength - FSegWidth, orgY + FSegWidth);
  FSegPoints[0, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[0, 4] := FSegPoints[0, 3];
  FSegPoints[0, 5] := FSegPoints[0, 3];

  // Points for segment b
  orgX := FSpacing + FGap + FHorzSegLength + FGap - FSegWidth;
  orgY := FSpacing + FGap + FSegWidth;
  FSegPoints[1, 0] := Point(orgX, orgY);
  FSegPoints[1, 1] := Point(orgX, orgY + FVertSegLength - FSegWidth - FSegWidth);
  FSegPoints[1, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[1, 3] := Point(orgX + FSegWidth, orgY - FSegWidth);
  FSegPoints[1, 4] := FSegPoints[1, 3];
  FSegPoints[1, 5] := FSegPoints[1, 3];

  // Points for segment c
  orgX := FSpacing + FGap + FHorzSegLength + FGap - FSegWidth;
  orgY := FSpacing + FGap + FSegWidth + FVertSegLength + FGap + FGap;
  FSegPoints[2, 0] := Point(orgX, orgY);
  FSegPoints[2, 1] := Point(orgX, orgY + FVertSegLength - FSegWidth - FSegWidth);
  FSegPoints[2, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[2, 3] := Point(orgX + FSegWidth, orgY - FSegWidth);
  FSegPoints[2, 4] := FSegPoints[2, 3];
  FSegPoints[2, 5] := FSegPoints[2, 3];

  // Points for segment d
  orgX := FSpacing + FGap + FSegWidth;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap + FVertSegLength + FGap - FSegWidth;
  FSegPoints[3, 0] := Point(orgX, orgY);
  FSegPoints[3, 1] := Point(orgX - FSegWidth, orgY + FSegWidth);
  FSegPoints[3, 2] := Point(orgX + FHorzSegLength - FSegWidth, orgY + FSegWidth);
  FSegPoints[3, 3] := Point(orgX + FHorzSegLength - FSegWidth - FSegWidth, orgY);
  FSegPoints[3, 4] := FSegPoints[3, 3];
  FSegPoints[3, 5] := FSegPoints[3, 3];

  // Points for segment e
  orgX := FSpacing;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap;
  FSegPoints[4, 0] := Point(orgX, orgY);
  FSegPoints[4, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[4, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[4, 3] := Point(orgX + FSegWidth, orgY + FSegWidth);
  FSegPoints[4, 4] := FSegPoints[4, 3];
  FSegPoints[4, 5] := FSegPoints[4, 3];

  // Points for segment f
  orgX := FSpacing;
  orgY := FSpacing + FGap;
  FSegPoints[5, 0] := Point(orgX, orgY);
  FSegPoints[5, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[5, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[5, 3] := Point(orgX + FSegWidth, orgY + FSegWidth);
  FSegPoints[5, 4] := FSegPoints[5, 3];
  FSegPoints[5, 5] := FSegPoints[5, 3];

  // Points for segment g
  orgX := FSpacing + FGap;
  orgY := FSpacing + FGap + FVertSegLength + FGap;
  FSegPoints[6, 0] := Point(orgX, orgY);
  FSegPoints[6, 1] := Point(orgX + FSegWidth, orgY + (FSegWidth div 2));
  FSegPoints[6, 2] := Point(orgX + FHorzSegLength - FSegWidth, orgY + (FSegWidth div 2));
  FSegPoints[6, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[6, 4] := Point(orgX + FHorzSegLength - FSegWidth, orgY - (FSegWidth div 2));
  FSegPoints[6, 5] := Point(orgX + FSegWidth, orgY - (FSegWidth div 2));
end;

procedure TplLed7Seg.GeometryRectangle;
var
  segLength, orgX, orgY: integer;
begin
  if (Width = 0) or (Height = 0) then
    Exit;
  // Horizontal Segment Length
  // Maintain sufficient segment length
  // increase width if necessary
  // room for space on left and right and
  // room for vertical segments on the left and right
  segLength := Width - FSpacing - FSpacing - FSegWidth - FSegWidth - FGap - FGap;
  if segLength < 5 then
  begin
    Width := Width + (5 - segLength);
    Exit;
  end;
  FHorzSegLength := segLength;

  // Vertical Segment Length
  // Maintain sufficient segment length
  // increase height if necessary
  // room for space on top and bottom and
  // room for horizontal segments on the top, middle and bottom
  segLength := Height - FSpacing - FSpacing - FGap - FGap - FGap - FGap;
  segLength := segLength div 2;
  if segLength < 5 then
  begin
    Height := Height + (2 * (5 - segLength));
    Exit;
  end;
  FVertSegLength := segLength;

  // Points for segment a
  orgX := FSpacing + FSegWidth + FGap;
  orgY := FSpacing;
  FSegPoints[0, 0] := Point(orgX, orgY);
  FSegPoints[0, 1] := Point(orgX, orgY + FSegWidth);
  FSegPoints[0, 2] := Point(orgX + FHorzSegLength, orgY + FSegWidth);
  FSegPoints[0, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[0, 4] := FSegPoints[0, 3];
  FSegPoints[0, 5] := FSegPoints[0, 3];

  // Points for segment b
  orgX := FSpacing + FSegWidth + FGap + FGap + FHorzSegLength;
  orgY := FSpacing + FGap;
  FSegPoints[1, 0] := Point(orgX, orgY);
  FSegPoints[1, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[1, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[1, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[1, 4] := FSegPoints[1, 3];
  FSegPoints[1, 5] := FSegPoints[1, 3];

  // Points for segment c
  orgX := FSpacing + FSegWidth + FGap + FGap + FHorzSegLength;
  orgY := FSpacing + FGap + FGap + FGap + FVertSegLength;
  FSegPoints[2, 0] := Point(orgX, orgY);
  FSegPoints[2, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[2, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[2, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[2, 4] := FSegPoints[2, 3];
  FSegPoints[2, 5] := FSegPoints[2, 3];

  // Points for segment d
  orgX := FSpacing + FSegWidth + FGap;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap + FVertSegLength + FGap - FSegWidth;
  FSegPoints[3, 0] := Point(orgX, orgY);
  FSegPoints[3, 1] := Point(orgX, orgY + FSegWidth);
  FSegPoints[3, 2] := Point(orgX + FHorzSegLength, orgY + FSegWidth);
  FSegPoints[3, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[3, 4] := FSegPoints[3, 3];
  FSegPoints[3, 5] := FSegPoints[3, 3];

  // Points for segment e
  orgX := FSpacing;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap;
  FSegPoints[4, 0] := Point(orgX, orgY);
  FSegPoints[4, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[4, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[4, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[4, 4] := FSegPoints[4, 3];
  FSegPoints[4, 5] := FSegPoints[4, 3];

  // Points for segment f
  orgX := FSpacing;
  orgY := FSpacing + FGap;
  FSegPoints[5, 0] := Point(orgX, orgY);
  FSegPoints[5, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[5, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[5, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[5, 4] := FSegPoints[5, 3];
  FSegPoints[5, 5] := FSegPoints[5, 3];

  // Points for segment g
  orgX := FSpacing + FSegWidth + FGap;
  orgY := FSpacing + FGap + FVertSegLength + FGap - (FSegWidth div 2);
  FSegPoints[6, 0] := Point(orgX, orgY);
  FSegPoints[6, 1] := Point(orgX, orgY + FSegWidth);
  FSegPoints[6, 2] := Point(orgX + FHorzSegLength, orgY + FSegWidth);
  FSegPoints[6, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[6, 4] := FSegPoints[6, 3];
  FSegPoints[6, 5] := FSegPoints[6, 3];
end;

procedure TplLed7Seg.Paint;
var
  i: integer;
begin
  inherited;
  with Canvas do
  begin
    // Fill the background
    Brush.Color := FBackColor;
    Pen.Color := FBackColor;
    Rectangle(0, 0, Width, Height);

    // Paint the segments
    // Lookup the table and paint bright if segment is
    // supposed to be on
    for i := 0 to 6 do
    begin
      if Enabled then
      begin
        if KValue2Segment[FValue, i] then
          Brush.Color := FBrightColor
        else
          Brush.Color := FDimColor;
      end
      else
        Brush.Color := FDimColor;


      Polygon(FSegPoints[i]);
    end;
  end;
end;

procedure TplLed7Seg.Resize;
begin
  inherited;
  Geometry;
  Invalidate;
end;

procedure TplLed7Seg.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetBrightColor(const Value: TColor);
begin
  if FBrightColor <> Value then
  begin
    FBrightColor := Value;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetDimColor(const Value: TColor);
begin
  if FDimColor <> Value then
  begin
    FDimColor := Value;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetGap(const Value: byte);
begin
  if FGap <> Value then
  begin
    FGap := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetSegShape(const Value: TSegmentShape);
begin
  if FSegShape <> Value then
  begin
    FSegShape := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetSegWidth(const Value: byte);
begin
  if FSegWidth <> Value then
  begin
    FSegWidth := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetSpacing(const Value: byte);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TplLed7Seg.SetValue(const Value: byte);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    Invalidate;
  end;
end;

end.
