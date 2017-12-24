{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgxImp;

{$mode objfpc}{$H+}

interface

uses

  LCLProc, LCLType, LResources, LCLIntf, LMessages, SysUtils, Classes, Controls, Graphics,
  Forms, ExtCtrls, Contnrs, ELDsgxUtils,
  ELDsgxSurface;

const
  cJvDesignDefaultHandleWidth = 8;

type
  TplDesignHandle = class(TCustomControl)
  private
    FResizeable: Boolean;
  protected
    function HandleRect(AIndex: Integer): TRect;
    function HitRect(APoint: TPoint): Integer;
    procedure Paint; override;
    procedure PaintEdge(const ARect: TRect);
    procedure PaintHandle(const ARect: TRect);
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
    property Resizeable: Boolean read FResizeable write FResizeable;
  end;

  TplDesignHandles = class(TComponent)
  private
    FContainer: TWinControl;
    FSelected: TControl;
    FResizeable: Boolean;
  protected
    function GetHandleWidth: Integer;
    function GetSelectionRect: TRect;
    function SelectedToScreenRect(const ARect: TRect): TRect;
    procedure CreateHandles;
    procedure SetContainer(const Value: TWinControl);
    procedure SetHandleRects(const ARect: TRect);
    procedure SetResizeable(const Value: Boolean);
    procedure SetSelected(const Value: TControl);
    procedure ShowHideHandles(AShow: Boolean);
  public
    Handles: array [0..3] of TplDesignHandle;
    constructor Create(AOwner: TComponent); override;
    function HitRect(X, Y: Integer): TplDesignHandleId;
    function SelectedToContainer(const APt: TPoint): TPoint;
    procedure RepaintHandles;
    procedure UpdateHandles;
    property Container: TWinControl read FContainer write SetContainer;
    property HandleWidth: Integer read GetHandleWidth;
    property Resizeable: Boolean read FResizeable write SetResizeable;
    property Selected: TControl read FSelected write SetSelected;
  end;

  TplDesignSelector = class(TplDesignCustomSelector)
  private
    FHandles: TObjectList;
    FHandleWidth: Integer;
  protected
    function FindHandles(AValue: TControl): TplDesignHandles;
    function GetCount: Integer; override;
    function GetHandles(AIndex: Integer): TplDesignHandles;
    function GetSelection(AIndex: Integer): TControl; override;
    procedure SetHandles(AIndex: Integer; AValue: TplDesignHandles);
    procedure SetHandleWidth(AValue: Integer);
    procedure SetSelection(AIndex: Integer; AValue: TControl); override;
    procedure ShowHideResizeHandles;
    property Handles[AIndex: Integer]: TplDesignHandles read GetHandles write SetHandles;
  public
    constructor Create(ASurface: TplDesignSurface); override;
    destructor Destroy; override;
    function GetClientControl(AControl: TControl): TControl; override;
    function GetCursor(AX, AY: Integer): TCursor; override;
    function GetHitHandle(AX, AY: Integer): TplDesignHandleId; override;
    function IsSelected(AValue: TControl): Boolean; override;
    procedure AddToSelection(AValue: TControl); override;
    procedure ClearSelection; override;
    procedure RemoveFromSelection(AValue: TControl); override;
    procedure Update; override;
  published
    property HandleWidth: Integer read FHandleWidth write SetHandleWidth default cJvDesignDefaultHandleWidth;
  end;

  TplDesignCustomMouseTool = class(TObject)
  protected
    FDragRect: TRect;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);  virtual; abstract;
    property DragRect: TRect read FDragRect write FDragRect;
  end;

  TplDesignDragMode = (dmNone, dmMove, dmResize, dmSelect, dmCreate);

  TplDesignAction = (daSelectParent, daDelete, daCopy, daCut, daPaste,
    daNudgeLeft, daNudgeRight, daNudgeUp, daNudgeDown, daGrowWidth,
    daShrinkWidth, daGrowHeight, daShrinkHeight, daLastAction = MaxInt);

  TplDesignController = class(TplDesignCustomController)
  private
    FClicked: TControl;
    FDragMode: TplDesignDragMode;
    FDragRect: TRect;
    FKeyDownShift: TShiftState;
    FMouseIsDown: Boolean;
    FMouseTool: TplDesignCustomMouseTool;
  protected
    function GetDragRect: TRect; override;
    function KeyDown(AKeyCode: Cardinal): Boolean; override;
    function KeyUp(AKeyCode: Cardinal): Boolean; override;
    function MouseDown(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    procedure Action(AAction: TplDesignAction);
  end;

  TplDesignMouseTool = class(TplDesignCustomMouseTool)
  private
    FSurface: TplDesignSurface;
    FMouseLast: TPoint;
    FMouseStart: TPoint;
  protected
    function GetMouseDelta: TPoint; virtual;
  public
    constructor Create(AOwner: TplDesignSurface); virtual;
    property Surface: TplDesignSurface read FSurface write FSurface;
  end;

  TplDesignMover = class(TplDesignMouseTool)
  private
    FDragRects: array of TRect;
  protected
    procedure ApplyDragRects;
    procedure CalcDragRects;
    procedure CalcPaintRects;
    procedure PaintDragRects;
  public
    constructor Create(AOwner: TplDesignSurface); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TplDesignBander = class(TplDesignMouseTool)
  protected
    function GetClient: TControl; virtual;
    function GetPaintRect: TRect;
    procedure CalcDragRect; virtual;
    procedure PaintDragRect; virtual;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TplDesignSizer = class(TplDesignBander)
  private
    FHandleId: TplDesignHandleId;
  protected
    function GetClient: TControl; override;
    procedure ApplyDragRect;
    procedure ApplyMouseDelta(X, Y: Integer);
    procedure CalcDragRect; override;
  public
    constructor CreateSizer(AOwner: TplDesignSurface; AHandle: TplDesignHandleId);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

    TplDesignDesigner = class( TIDesigner)// TComponent, IDesignerHook)
  private
    FMessenger: TplDesignCustomMessenger;
  public
    DDC: TDesignerDeviceContext;
    constructor Create(AMessenger: TplDesignCustomMessenger); reintroduce;
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation); reintroduce;
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Msg: TLMessage): Boolean; override;
    procedure PaintGrid; override;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string); reintroduce;
    function UniqueName(const BaseName: string): string; override;
    function GetRoot: TComponent;
    property Messenger: TplDesignCustomMessenger read FMessenger write FMessenger;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
  end;



  TplDesignDesignerMessenger = class(TplDesignCustomMessenger)
  private
    FDesignedForm: TCustomForm;
    FDesigner: TplDesignDesigner;
  protected
    procedure SetComponentDesigning(AComponent: TComponent; ADesigning: Boolean);
    procedure SetContainer(AValue: TWinControl); override;
    procedure UndesignComponent(AComponent: TComponent);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); override;
  end;

  TplDesignMessageHookList = class(TComponent)
  private
    FHooks: TObjectList;
    FUser: TplDesignCustomMessenger;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AUser: TplDesignCustomMessenger); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Hook(AClient: TWinControl);
    procedure Unhook(AComponent: TComponent);
  end;

  TplDesignWinControlHookMessenger = class(TplDesignCustomMessenger)
  private
    FHooks: TplDesignMessageHookList;
  protected
    procedure HookWinControl(AWinControl: TWinControl);
    procedure UnhookWinControl(AWinControl: TWinControl);
    procedure SetContainer(AValue: TWinControl); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); override;
  end;

implementation

uses
  ELDsgxResources, ELDsgxTypes;

var
  ShadedBits: TBitmap;

function NeedShadedBits: TBitmap;
begin
  if ShadedBits = nil then
  begin
    ShadedBits := TBitmap.Create;
    with ShadedBits do
    begin
      Width := 4;
      Height := 2;
      Canvas.Pixels[0, 0] := clGray;
      Canvas.Pixels[1, 0] := clBtnFace;
      Canvas.Pixels[2, 0] := clBtnFace;
      Canvas.Pixels[3, 0] := clBtnFace;
      Canvas.Pixels[0, 1] := clBtnFace;
      Canvas.Pixels[1, 1] := clBtnFace;
      Canvas.Pixels[2, 1] := clGray;
      Canvas.Pixels[3, 1] := clBtnFace;
    end;
  end;
  Result := ShadedBits;
end;

procedure FreeShadedBits;
begin
  FreeAndNil(ShadedBits);
end;

//=== { TplDesignHandle } ====================================================

function TplDesignHandle.HandleRect(AIndex: Integer): TRect;
var
  W: Integer;
begin
  W := TplDesignHandles(Owner).HandleWidth;
  case AIndex of
    0:
      Result := Rect(0, 0, W, W); // left-top
    1:
      Result := Rect((Width - W) div 2, 0, (Width + W) div 2, W); // middle-top
    2:
      Result := Rect(Width - W, 0, Width, W); // right-top
    3:
      Result := Rect(0, (Height - W) div 2, W, (Height + W) div 2); // left-center
  end;
end;

procedure TplDesignHandle.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TplDesignHandle.PaintHandle(const ARect: TRect);
begin
  Canvas.Rectangle(ARect);
end;

procedure TplDesignHandle.PaintEdge(const ARect: TRect);
begin
  Canvas.FillRect(ClientRect);
end;

procedure TplDesignHandle.Paint;
begin
  //CV with Canvas. do
  begin
    Canvas.Brush.Bitmap := NeedShadedBits;
    PaintEdge(ClientRect);
    Canvas.Brush.Bitmap := nil;
    Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
    if Resizeable then
      if Width > Height then
      begin
        PaintHandle(HandleRect(0));
        PaintHandle(HandleRect(1));
        PaintHandle(HandleRect(2));
      end
      else
      begin
        PaintHandle(HandleRect(3));
      end
  end;
end;

function TplDesignHandle.HitRect(APoint: TPoint): Integer;
begin
  Result := -1;
  if Width > Height then
    if PtInRect(HandleRect(0), APoint) then
      Result := 0
    else
    if PtInRect(HandleRect(1), APoint) then
      Result := 1
    else
    if PtInRect(HandleRect(2), APoint) then
      Result := 2;
  if Result < 0 then
    if PtInRect(HandleRect(3), APoint) then
      Result := 3;
end;

//=== { TplDesignHandles } ===================================================

constructor TplDesignHandles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateHandles;
  Resizeable := True;
end;

procedure TplDesignHandles.CreateHandles;
var
  I: Integer;
begin
  for I := Low(Handles) to High(Handles) do
    Handles[I] := TplDesignHandle.Create(Self);
end;

function TplDesignHandles.GetHandleWidth: Integer;
begin
  Result := TplDesignSelector(Owner).HandleWidth;
end;

procedure TplDesignHandles.SetContainer(const Value: TWinControl);
var
  I: Integer;
begin
  FContainer := Value;
  for I := Low(Handles) to High(Handles) do
    with Handles[I] do
    begin
      Visible := False;
      Parent := Container;
    end;
end;

procedure TplDesignHandles.SetSelected(const Value: TControl);
begin
  if Selected <> Value then
  begin
    if Value is TplDesignHandle then
      FSelected := nil
    else
      FSelected := Value;
    UpdateHandles;
  end;
end;

procedure TplDesignHandles.SetResizeable(const Value: Boolean);
var
  I: Integer;
begin
  FResizeable := Value;
  for I := Low(Handles) to High(Handles) do
    Handles[I].Resizeable := Value;
end;

procedure TplDesignHandles.ShowHideHandles(AShow: Boolean);
var
  I: Integer;
begin
  for I := Low(Handles) to High(Handles) do
    with Handles[I] do
    begin
      Visible := AShow;
      if AShow then
        BringToFront;
      Update;
    end;
end;

procedure TplDesignHandles.UpdateHandles;
begin
  if (Selected <> nil) and (Container <> nil) and (Selected <> Container) then
  begin
    SetHandleRects(GetSelectionRect);
    ShowHideHandles(True);
  end
  else
    ShowHideHandles(False)
end;

procedure TplDesignHandles.RepaintHandles;
var
  I: Integer;
begin
  for I := Low(Handles) to High(Handles) do
    Handles[I].Repaint;
end;

function TplDesignHandles.HitRect(X, Y: Integer): TplDesignHandleId;
const
  cRectIds: array [0..3, 0..3] of TplDesignHandleId =
   (
    (dhLeftTop, dhMiddleTop, dhRightTop, dhNone),
    (dhNone, dhNone, dhNone, dhLeftMiddle),
    (dhNone, dhNone, dhNone, dhRightMiddle),
    (dhLeftBottom, dhMiddleBottom, dhRightBottom, dhNone)
   );
var
  I, R: Integer;
begin
  for I := 0 to 3 do
  begin
    with Handles[I] do
      R := HitRect(Point(X - Left, Y - Top));
    if R >= 0 then
    begin
      Result := cRectIds[I][R];
      Exit;
    end;
  end;
  Result := dhNone;
end;

function TplDesignHandles.SelectedToContainer(const APt: TPoint): TPoint;
var
  C: TControl;
begin
  Result := APt;
  C := Selected.Parent;
  while (C <> Container) and (C <> nil) do
  begin
    Inc(Result.X, C.Left);
    Inc(Result.Y, C.Top);
    C := C.Parent;
  end;
end;

function TplDesignHandles.SelectedToScreenRect(const ARect: TRect): TRect;
var
  P: TWinControl;
begin
  if Selected = Container then
    P := Container
  else
    P := Selected.Parent;
  Result.TopLeft := P.ClientToScreen(ARect.TopLeft);
  Result.BottomRight := P.ClientToScreen(ARect.BottomRight);
end;

function TplDesignHandles.GetSelectionRect: TRect;
var
  P: TPoint;
begin
  if Selected = Container then
    P := Point(0, 0)
  else
    P := SelectedToContainer(Selected.BoundsRect.TopLeft);
  Result := Rect(P.X, P.Y, P.X + Selected.Width, P.Y + Selected.Height);
  InflateRect(Result, -HandleWidth div 2, -HandleWidth div 2);
end;

procedure TplDesignHandles.SetHandleRects(const ARect: TRect);
var
  W: Integer;
begin
  W := HandleWidth;
  with ARect do
  begin
    Handles[0].BoundsRect := Rect(Left - W, Top - W, Right + W, Top);
    Handles[1].BoundsRect := Rect(Left - W, Top, Left, Bottom);
    Handles[2].BoundsRect := Rect(Right, Top, Right + W, Bottom);
    Handles[3].BoundsRect := Rect(Left - W, Bottom, Right + W, Bottom + W);
  end;
end;

//=== { TplDesignSelector } ==================================================

constructor TplDesignSelector.Create(ASurface: TplDesignSurface);
begin
  inherited Create(ASurface);
  //ControllerClass := TplDesignController;
  FHandleWidth := cJvDesignDefaultHandleWidth;
  FHandles := TObjectList.Create;
end;

destructor TplDesignSelector.Destroy;
begin
  FHandles.Free;
  inherited Destroy;
end;

procedure TplDesignSelector.SetHandleWidth(AValue: Integer);
begin
  FHandleWidth := AValue;
  Update;
end;

function TplDesignSelector.GetCount: Integer;
begin
  Result := FHandles.Count;
end;

function TplDesignSelector.GetHandles(AIndex: Integer): TplDesignHandles;
begin
  Result := TplDesignHandles(FHandles[AIndex]);
end;

procedure TplDesignSelector.SetHandles(AIndex: Integer; AValue: TplDesignHandles);
begin
  FHandles[AIndex] := AValue;
end;

function TplDesignSelector.GetSelection(AIndex: Integer): TControl;
begin
  Result := Handles[AIndex].Selected;
end;

procedure TplDesignSelector.SetSelection(AIndex: Integer; AValue: TControl);
begin
  Handles[AIndex].Selected := AValue;
end;

function TplDesignSelector.FindHandles(AValue: TControl): TplDesignHandles;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Handles[I];
    if Result.Selected = AValue then
      Break
    else
      Result := nil;
  end;
end;

function TplDesignSelector.IsSelected(AValue: TControl): Boolean;
begin
  Result := FindHandles(AValue) <> nil;
end;

procedure TplDesignSelector.ClearSelection;
begin
  //if not (csDestroying in ComponentState) then
  FHandles.Clear;
end;

procedure TplDesignSelector.ShowHideResizeHandles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Handles[I] do
    begin
      Resizeable := (Count = 1);
      RepaintHandles;
    end;
end;

procedure TplDesignSelector.AddToSelection(AValue: TControl);
var
  H: TplDesignHandles;
begin
  if AValue = nil then
    raise EJVCLException.CreateRes(@RsEDesignCannotSelect);
  if not IsSelected(AValue) then
  begin
    H := TplDesignHandles.Create(Self);
    H.Container := Surface.Container;
    H.Resizeable := Count = 0;
    FHandles.Add(H);
    H.Selected := AValue;
    if Count = 2 then
      ShowHideResizeHandles
    else
      H.UpdateHandles;
    Surface.Messenger.DesignComponent(H.Handles[0], True);
    Surface.Messenger.DesignComponent(H.Handles[1], True);
    Surface.Messenger.DesignComponent(H.Handles[2], True);
    Surface.Messenger.DesignComponent(H.Handles[3], True);
  end;
end;

procedure TplDesignSelector.RemoveFromSelection(AValue: TControl);
begin
  if IsSelected(AValue) then
  begin
    FHandles.Remove(FindHandles(AValue));
    Surface.SelectionChange;
  end;
end;

function TplDesignSelector.GetClientControl(AControl: TControl): TControl;
begin
  if AControl is TplDesignHandle then
    Result := TplDesignHandles(AControl.Owner).Selected
  else
    Result := AControl;
end;

procedure TplDesignSelector.Update;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Handles[I].UpdateHandles;
end;

function TplDesignSelector.GetHitHandle(AX, AY: Integer): TplDesignHandleId;
begin
  if Count > 0 then
    Result := Handles[0].HitRect(AX, AY)
  else
    Result := dhNone;
end;

function TplDesignSelector.GetCursor(AX, AY: Integer): TCursor;
const
  cCurs: array[TplDesignHandleId] of TCursor =
   (crHandPoint, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeWE,
    crSizeNESW, crSizeNS, crSizeNWSE);
begin
  Result := cCurs[GetHitHandle(AX, AY)];
end;

//=== { TplDesignController } ================================================

procedure TplDesignController.Action(AAction: TplDesignAction);
begin
  with Surface do
    case AAction of
      daSelectParent:
        SelectParent;
      daDelete:
        DeleteComponents;
      daCopy:
        CopyComponents;
      daCut:
        CutComponents;
      daPaste:
        PasteComponents;
      daNudgeLeft:
        NudgeComponents(-1, 0);
      daNudgeRight:
        NudgeComponents(1, 0);
      daNudgeUp:
        NudgeComponents(0, -1);
      daNudgeDown:
        NudgeComponents(0, 1);
      daGrowWidth:
        GrowComponents(1, 0);
      daShrinkWidth:
        GrowComponents(-1, 0);
      daGrowHeight:
        GrowComponents(0, 1);
      daShrinkHeight:
        GrowComponents(0, -1);
    end;
  Surface.UpdateDesigner;
end;

function TplDesignController.GetDragRect: TRect;
begin
  Result := FDragRect;
end;

function TplDesignController.KeyDown(AKeyCode: Cardinal): Boolean;

  function CtrlKeys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      VK_LEFT:
        Action(daNudgeLeft);
      VK_RIGHT:
        Action(daNudgeRight);
      VK_UP:
        Action(daNudgeUp);
      VK_DOWN:
        Action(daNudgeDown);
      else
        Result := False;
    end;
  end;

  function ShiftKeys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      VK_LEFT:
        Action(daShrinkWidth);
      VK_RIGHT:
        Action(daGrowWidth);
      VK_UP:
        Action(daShrinkHeight);
      VK_DOWN:
        Action(daGrowHeight);
      else
        Result := False;
    end;
  end;

begin
  FKeyDownShift := Shift666;
  if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else
  if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := False;
end;

function TplDesignController.KeyUp(AKeyCode: Cardinal): Boolean;

  function Keys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      VK_ESCAPE:
        Action(daSelectParent);
      VK_DELETE:
        Action(daDelete);
      else
        Result := False;
    end;
  end;

  function CtrlKeys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      Ord('C'):
        Action(daCopy);
      Ord('X'):
        Action(daCut);
      Ord('V'):
        Action(daPaste);
      else
        Result := False;
    end;
  end;

  function ShiftKeys: Boolean;
  begin
    Result := False;
  end;

begin
  FKeyDownShift := FKeyDownShift + Shift666;
  if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else
  if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := Keys;
  FKeyDownShift := [];
end;

function TplDesignController.MouseDown(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean;
var
  HandleId: TplDesignHandleId;

  procedure CaptureMouse;
  begin
    FMouseIsDown := True;
    Mouse.Capture := Surface.Container.Handle;
  end;

  procedure FocusSurface;
  var
    WasActive: Boolean;
  begin
    if not Surface.Container.Focused and Surface.Container.CanFocus then
    begin
      // Mantis 4732: deactivate the container otherwise SetFocus does not work
      // This bug apparently only happens under certain rare conditions
      // under windows but its fix does not seem to have any negative impact
      // on systems where it does not happen.
      WasActive := TplDesignPanel(Surface.Container).Active;
      if WasActive then
        TplDesignPanel(Surface.Container).Active := False;

      Surface.Container.SetFocus;

      if WasActive then
        TplDesignPanel(Surface.Container).Active := True;
    end;
  end;

  procedure SelectDragMode;
  begin
    HandleId := dhNone;
    if ssCtrl in Shift666 then
      // Ctrl-drag selection has highest priority
      FDragMode := dmSelect
    else
    begin
      HandleId := Surface.GetHitHandle(X, Y);
      if HandleId <> dhNone then
      begin
        FClicked := Surface.Selection[0];
        FDragMode := dmResize;
      end
      else
      begin
        FClicked := Surface.FindControl(X, Y);
        if (FClicked = Surface.Container) or (FClicked is TplDesignHandle) then
          FClicked := nil;
        Surface.GetAddClass;
        if Surface.AddClass <> '' then
          // then object creation
          FDragMode := dmCreate
        else
        if FClicked <> nil then
          // moving is last
          FDragMode := dmMove
        else
          // select by default
          FDragMode := dmSelect;
      end;
    end;
    if FClicked = nil then
      FClicked := Surface.Container;
    FClicked.Parent.DisableAlign;
  end;

  procedure CreateMouseTool;
  begin
    case FDragMode of
      dmSelect, dmCreate:
        begin
          Surface.ClearSelection;
          FMouseTool := TplDesignBander.Create(Surface);
        end;
      dmMove:
        begin
          if ssShift in Shift666 then
            Surface.Selector.AddToSelection(FClicked)
          else
          if not Surface.Selector.IsSelected(FClicked) then
            Surface.Select(FClicked);
          FMouseTool := TplDesignMover.Create(Surface);
        end;
      dmResize:
        begin
          if not Surface.Selector.IsSelected(FClicked) then
            Surface.Select(FClicked);
          FMouseTool := TplDesignSizer.CreateSizer(Surface, HandleId);
        end;
    end;
    if FMouseTool <> nil then
      FMouseTool.MouseDown(Button, Shift666, X, Y);
  end;

begin
  Shift666 := [];
  if (TheMessage.Keys and MK_Shift) = MK_Shift then
    Shift666 := Shift666 + [ssShift];
  if (TheMessage.Keys and MK_Control) = MK_Control then
    Shift666 := Shift666 + [ssCtrl];
  FocusSurface;
  CaptureMouse;
  SelectDragMode;
  CreateMouseTool;
  Result := True;
end;

function TplDesignController.MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean;
begin
  Shift666 := [];
  if (TheMessage.Keys and MK_Shift) = MK_Shift then
    Shift666 := Shift666 + [ssShift];
  if (TheMessage.Keys and MK_Control) = MK_Control then
    Shift666 := Shift666 + [ssCtrl];

  if not FMouseIsDown then
    SetCursor(Screen.Cursors[Surface.GetCursor(X, Y)])
  else
  begin
    if FMouseTool <> nil then
      FMouseTool.MouseMove(Shift666, X, Y);
  end;
  Result := True;
end;

function TplDesignController.MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean;

  procedure ReleaseMouse;
  begin
    FMouseIsDown := False;
    Mouse.Capture := 0;
  end;

  procedure EnableAlign;
  begin
    // If the debugger breaks in during a mouse operation,
    // AlignDisabled can become stuck.
    // This routine is to aid debugging only.
    if FClicked <> nil then
      //cv while FClicked.Parent.AlignDisabled do
        FClicked.Parent.EnableAlign;
  end;

  procedure FinishMouseTool;
  begin
    if FMouseTool <> nil then
    try
      FMouseTool.MouseUp(Button, Shift666, X, Y);
      FDragRect := DesignValidateRect(FMouseTool.DragRect);
      case FDragMode of
        dmCreate:
          begin
            if FClicked <> nil then
              Surface.Select(FClicked);
            Surface.AddComponent;
          end;
        else
          Surface.SelectionChange;
      end;
    finally
      FreeAndNil(FMouseTool);
    end;
  end;

begin
  Shift666 := [];
  if (TheMessage.Keys and MK_Shift) = MK_Shift then
    Shift666 := Shift666 + [ssShift];
  if (TheMessage.Keys and MK_Control) = MK_Control then
    Shift666 := Shift666 + [ssCtrl];

  if FMouseIsDown then
  begin
    ReleaseMouse;
    EnableAlign;
    FinishMouseTool;
    // We have to call UpdateDesigner for GraphicControls because they don't get
    // WM_WINDOWPOSCHANGED messages that update the designer handles.
    //CV LINUX if FClicked is TGraphicControl then
      Surface.UpdateDesigner;
    FClicked := nil;
  end;
  Result := True;
end;

//=== { TplDesignMouseTool } =================================================

constructor TplDesignMouseTool.Create(AOwner: TplDesignSurface);
begin
  Surface := AOwner;
end;

function TplDesignMouseTool.GetMouseDelta: TPoint;
const
  GridX = 4;
  GridY = 4;
begin
  with Result do
  begin
    X := FMouseLast.X - FMouseStart.X;
    Dec(X, X mod GridX);
    Y := FMouseLast.Y - FMouseStart.Y;
    Dec(Y, Y mod GridY);
  end;
end;

//=== { TplDesignMover } =====================================================

constructor TplDesignMover.Create(AOwner: TplDesignSurface);
begin
  inherited Create(AOwner);
  SetLength(FDragRects, Surface.Count);
end;

procedure TplDesignMover.CalcDragRects;
var
  Delta: TPoint;
  I: Integer;
begin
  Delta := GetMouseDelta;
  for I := 0 to Surface.Count - 1 do
    with Surface.Selection[I] do
    begin
      FDragRects[I] := BoundsRect;
      OffsetRect(FDragRects[I], Delta.X, Delta.Y);
    end;
end;

procedure TplDesignMover.CalcPaintRects;
var
  I: Integer;
  ScreenPoint: TPoint;
begin
  CalcDragRects;
  for I := 0 to Surface.Count - 1 do
  begin
    with Surface.Selection[I] do
      ScreenPoint := Parent.ClientToScreen(Point(0, 0));
    OffsetRect(FDragRects[I], ScreenPoint.X, ScreenPoint.Y);
  end;
end;

procedure TplDesignMover.PaintDragRects;
var
  I: Integer;
begin
  for I := 0 to Surface.Count - 1 do
    DesignPaintRubberbandRect(Surface.Container, FDragRects[I], psDot);
end;

procedure TplDesignMover.ApplyDragRects;
var
  I: Integer;
begin
  if (GetMouseDelta.X <> 0) or (GetMouseDelta.Y <> 0) then
  begin
    CalcDragRects;
    for I := 0 to Surface.Count - 1 do
      Surface.Selection[I].BoundsRect := FDragRects[I];
    Surface.Change;
  end;
end;

procedure TplDesignMover.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseStart := Point(X, Y);
  FMouseLast := FMouseStart;
  CalcPaintRects;
  PaintDragRects;
end;

procedure TplDesignMover.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  PaintDragRects;
  FMouseLast := Point(X, Y);
  CalcPaintRects;
  PaintDragRects;
end;

procedure TplDesignMover.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  PaintDragRects;
  FMouseLast := Point(X, Y);
  ApplyDragRects;
end;

//=== { TplDesignBander } ====================================================

procedure TplDesignBander.CalcDragRect;
begin
  with GetMouseDelta do
  begin
    DragRect := Rect(0, 0, X, Y);
    OffsetRect(FDragRect, FMouseStart.X, FMouseStart.Y);
  end;
end;

function TplDesignBander.GetClient: TControl;
begin
  Result := Surface.Container;
end;

function TplDesignBander.GetPaintRect: TRect;
begin
  Result := FDragRect;
  with GetClient.ClientToScreen(Point(0, 0)) do
    OffsetRect(Result, X, Y);
end;

procedure TplDesignBander.PaintDragRect;
begin
  DesignPaintRubberbandRect(Surface.Container, GetPaintRect, psDot);
end;

procedure TplDesignBander.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseStart := Point(X, Y);
  FMouseLast := FMouseStart;
  CalcDragRect;
  PaintDragRect;
end;

procedure TplDesignBander.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  PaintDragRect;
  FMouseLast := Point(X, Y);
  CalcDragRect;
  PaintDragRect;
end;

procedure TplDesignBander.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  PaintDragRect;
  CalcDragRect;
end;

//=== { TplDesignSizer } =====================================================

constructor TplDesignSizer.CreateSizer(AOwner: TplDesignSurface; AHandle: TplDesignHandleId);
begin
  inherited Create(AOwner);
  FHandleId := AHandle;
end;

procedure TplDesignSizer.ApplyMouseDelta(X, Y: Integer);
begin
  case FHandleId of
    dhLeftTop, dhMiddleTop, dhRightTop:
      Inc(FDragRect.Top, Y);
    dhLeftBottom, dhMiddleBottom, dhRightBottom:
      Inc(FDragRect.Bottom, Y);
  end;
  case FHandleId of
    dhLeftTop, dhLeftMiddle, dhLeftBottom:
      Inc(FDragRect.Left, X);
    dhRightTop, dhRightMiddle, dhRightBottom:
      Inc(FDragRect.Right, X);
  end;
end;

procedure TplDesignSizer.CalcDragRect;
begin
  FDragRect := Surface.Selection[0].BoundsRect;
  with GetMouseDelta do
    ApplyMouseDelta(X, Y);
  FDragRect := DesignValidateRect(FDragRect);
end;

function TplDesignSizer.GetClient: TControl;
begin
  Result := Surface.Selection[0].Parent;
end;

procedure TplDesignSizer.ApplyDragRect;
begin
  Surface.Selection[0].BoundsRect := FDragRect;
  Surface.Change;
end;

procedure TplDesignSizer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  ApplyDragRect;
end;

//=== { TplDesignDesigner } ==================================================

constructor TplDesignDesigner.Create(AMessenger: TplDesignCustomMessenger);
begin
  inherited Create;
    DDC:=TDesignerDeviceContext.Create;
    FMessenger := AMessenger;
end;

function TplDesignDesigner.GetCustomForm: TCustomForm;
begin
  Result := nil;
end;

function TplDesignDesigner.GetIsControl: Boolean;
begin
  Result := False;
end;

function TplDesignDesigner.GetRoot: TComponent;
begin
  Result := nil;
end;

function TplDesignDesigner.IsDesignMsg(Sender: TControl; var Msg: TLMessage): Boolean;
begin
  Result := Messenger.IsDesignMessage(Sender, Msg);
end;

procedure TplDesignDesigner.Modified;
begin
  //
end;

procedure TplDesignDesigner.Notification(AnObject: TPersistent;
  Operation: TOperation);
begin
  //
end;

procedure TplDesignDesigner.PaintGrid;
begin
  //
end;

procedure TplDesignDesigner.SetCustomForm(Value: TCustomForm);
begin
  //
end;

procedure TplDesignDesigner.SetIsControl(Value: Boolean);
begin
  //
end;

function TplDesignDesigner.UniqueName(const BaseName: string): string;
begin
  //
end;

procedure TplDesignDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  //
end;

{$IFDEF COMPILER9_UP}
procedure TplDesignDesigner.PaintMenu;
begin
  //
end;
{$ENDIF COMPILER9_UP}

//=== { TplDesignDesignerMessenger } =========================================

constructor TplDesignDesignerMessenger.Create;
begin
  FDesigner := TplDesignDesigner.Create(Self);
end;

destructor TplDesignDesignerMessenger.Destroy;
begin
  if Container <> nil then
    DesignChildren(Container, False);
  if FDesignedForm <> nil then
    FDesignedForm.Designer := nil;
  FDesigner.Free;
  inherited Destroy;
end;

type
  TAccessComponent = class(TComponent);

procedure TplDesignDesignerMessenger.SetComponentDesigning(AComponent: TComponent; ADesigning: Boolean);
begin
  TAccessComponent(AComponent).SetDesigning(ADesigning);
end;

procedure TplDesignDesignerMessenger.UndesignComponent(AComponent: TComponent);
begin
  SetComponentDesigning(AComponent, False);
end;

procedure TplDesignDesignerMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  SetComponentDesigning(AComponent, ADesigning);
end;

procedure TplDesignDesignerMessenger.SetContainer(AValue: TWinControl);

  function FindParentForm: TCustomForm;
  var
    P: TWinControl;
  begin
    P := Container;
    while P.Parent <> nil do
      P := P.Parent;
    if not (P is TCustomForm) then
      raise EJVCLException.CreateResFmt(@RsEOldestFmt , [ClassName]);
    Result := TCustomForm(P);
  end;

begin
  inherited SetContainer(AValue);
  if Container <> nil then
  begin
    FDesignedForm := FindParentForm;
    FDesignedForm.Designer := FDesigner;
    DesignChildren(Container, True);
  end;
end;

//=== { TplDesignMessageHookList } ===========================================

constructor TplDesignMessageHookList.Create(AUser: TplDesignCustomMessenger);
begin
  inherited Create(nil);
  FUser := AUser;
  FHooks := TObjectList.Create;
  FHooks.OwnsObjects := True;
end;

destructor TplDesignMessageHookList.Destroy;
begin
  FHooks.Free;
  inherited Destroy;
end;

procedure TplDesignMessageHookList.Clear;
begin
  FHooks.Clear;
end;

procedure TplDesignMessageHookList.Hook(AClient: TWinControl);
begin
  AClient.FreeNotification(Self);
  FHooks.Add(TplDesignMessageHook.Create(FUser, AClient));
end;

procedure TplDesignMessageHookList.Unhook(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to FHooks.Count - 1 do
    if TplDesignMessageHook(FHooks[I]).Client = AComponent then
    begin
      FHooks.Delete(I);
      Break;
    end;
end;

procedure TplDesignMessageHookList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    Unhook(AComponent);
end;

//=== { TplDesignWinControlHookMessenger } ===================================

constructor TplDesignWinControlHookMessenger.Create;
begin
  inherited Create;
  FHooks := TplDesignMessageHookList.Create(Self);
end;

destructor TplDesignWinControlHookMessenger.Destroy;
begin
  FHooks.Free;
  inherited Destroy;
end;

procedure TplDesignWinControlHookMessenger.Clear;
begin
  FHooks.Clear;
end;

procedure TplDesignWinControlHookMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  if (AComponent is TWinControl) then
    if ADesigning then
      HookWinControl(TWinControl(AComponent))
    else
      UnhookWinControl(TWinControl(AComponent))
end;

procedure TplDesignWinControlHookMessenger.HookWinControl(AWinControl: TWinControl);
begin
  FHooks.Hook(AWinControl);
  DesignChildren(AWinControl, True);
end;

procedure TplDesignWinControlHookMessenger.UnhookWinControl(AWinControl: TWinControl);
begin
  FHooks.Unhook(AWinControl);
  DesignChildren(AWinControl, False);
end;

procedure TplDesignWinControlHookMessenger.SetContainer(AValue: TWinControl);
begin
  inherited SetContainer(AValue);
  if Container <> nil then
    DesignChildren(Container, True);
end;

initialization

finalization
  FreeShadedBits;

end.


