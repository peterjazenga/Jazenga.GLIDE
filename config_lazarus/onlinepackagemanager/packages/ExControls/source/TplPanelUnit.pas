
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplPanelUnit;

interface


uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, plUtils;

type

TplPanel = class(TCustomPanel)
  private
    FTransparent: Boolean;
    FDrawFrame:boolean;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure SetTransparent (const Value: Boolean);
    procedure SetDrawFrame (const Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property DrawFrame:boolean read FDrawFrame write SetDrawFrame default true;
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
   // property OnCanResize;
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

constructor TplPanel.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := True;
  FDrawFrame:= True;
  FTransparent:= False;
  Color:=DefiControlsBkColor;
  ParentColor := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];
  SetBounds(0, 0, 185, 41);
end;

procedure TplPanel.Paint;
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
      Canvas.Brush.Color := DefiControlsBkColor;
      Canvas.FillRect(ClientRect);
    end;

    // Draw Border
    if FDrawFrame then
      Frame3DBorder(Canvas, ClientRect, DefiPanelColorHighLight, DefiPanelColorShadow, 1);

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

procedure TplPanel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplPanel.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplPanel.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplPanel.SetDrawFrame(const Value: Boolean);
begin
  FDrawFrame := Value;
  Invalidate;
end;



end.
