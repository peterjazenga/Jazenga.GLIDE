
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplColorPanelUnit;

interface

uses
  Messages, LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  LCLType, LCLIntf,
  ExtCtrls, plUtils;

type

  TplColorPanel = class(TCustomPanel)
  private
    FTransparent: boolean;
    FDrawFrame: boolean;
    fFrameColorHighLight: TColor;
    fFrameColorShadow: TColor;
    fFrameWidth: integer;
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TLMNoParams); message CM_TEXTCHANGED;
    procedure SetTransparent(const Value: boolean);
    procedure SetDrawFrame(const Value: boolean);
    procedure SetFrameColorHighLight(const Value: TColor);
    procedure SetFrameColorShadow(const Value: TColor);
    procedure SetFrameWidth(const Value: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Transparent: boolean read FTransparent write SetTransparent default False;
    property DrawFrame: boolean read FDrawFrame write SetDrawFrame default True;
    property FrameColorHighLight: TColor read FFrameColorHighLight write SetFrameColorHighLight;
    property FrameColorShadow: TColor read FFrameColorShadow write SetFrameColorShadow;
    property FrameWidth: integer read FFrameWidth write SetFrameWidth;
    property Color;
    property Caption;
    property Font;
    property ParentColor;
    property Enabled;
    property Visible;
    property Align;
    property Alignment;
    property Cursor;
    property Hint;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property AutoSize;
    property UseDockManager;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property ParentBiDiMode;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

//========================== TplColorPanel =========================================
constructor TplColorPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := True;
  FDrawFrame := True;
  FTransparent := False;
  Color := clBtnFace;
  ParentColor := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];
  fFrameColorHighLight := $008396A0;
  fFrameColorShadow := $008396A0;
  fFrameWidth := 1;
  SetBounds(0, 0, 185, 41);
end;

procedure TplColorPanel.Paint;
var
  textBounds: TRect;
  Format: UINT;
begin
  textBounds := ClientRect;
  Format := DT_SINGLELINE or DT_VCENTER;
  case Alignment of
    taLeftJustify:
      Format := Format or DT_LEFT;
    taCenter:
      Format := Format or DT_CENTER;
    taRightJustify:
      Format := Format or DT_RIGHT;
  end;

    // Draw Background
    if FTransparent then
      DrawParentImage(Self, Canvas)
    else
    begin
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(ClientRect);
    end;

    // Draw Border
    if FDrawFrame then
      Frame3DBorder(Canvas, ClientRect, fFrameColorHighLight, fFrameColorShadow, fFrameWidth);

    // Draw Text
    Canvas.Font := Self.Font;
    Canvas.Brush.Style := bsClear;
    if not Enabled then
    begin
      OffsetRect(textBounds, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, Format);
      OffsetRect(textBounds, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, Format);
    end
    else
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, Format);

end;

procedure TplColorPanel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplColorPanel.CMTextChanged(var Message: TLmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TplColorPanel.SetTransparent(const Value: boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplColorPanel.SetDrawFrame(const Value: boolean);
begin
  FDrawFrame := Value;
  Invalidate;
end;


procedure TplColorPanel.SetFrameColorHighLight(const Value: TColor);
begin
  if fFrameColorHighLight = Value then
    exit;

  fFrameColorHighLight := Value;
  Invalidate;
end;

procedure TplColorPanel.SetFrameColorShadow(const Value: TColor);
begin
  if fFrameColorShadow = Value then
    exit;
  fFrameColorShadow := Value;
  Invalidate;
end;

procedure TplColorPanel.SetFrameWidth(const Value: integer);
begin
  if fFrameWidth = Value then
    exit;
  if Value < 0 then
    exit;

  fFrameWidth := Value;
  Invalidate;
end;


end.
