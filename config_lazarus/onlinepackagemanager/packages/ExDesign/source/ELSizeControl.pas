{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELSizeControl;

{$MODE Delphi}

interface

{$R ELSizeControl.res}

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Controls, ExtCtrls,
  Graphics, Forms, TypInfo, Menus;

type
  TplSizeControl = class;
  TTargetObj = class;

  TBtnPos = (bpLeft, bpTopLeft, bpTop, bpTopRight,
    bpRight, bpBottomRight, bpBottom, bpBottomLeft);
  TBtnPosSet = set of TBtnPos;

  TSCState = (scsReady, scsMoving, scsSizing);

  TStartEndEvent = procedure(Sender: TObject; State: TSCState) of object;
  TDuringEvent = procedure(Sender: TObject; dx, dy: integer; State: TSCState) of object;
  TMouseDownEvent = procedure(Sender: TObject; Target: TControl; TargetPt: TPoint; var handled: boolean) of object;
  TSetCursorEvent = procedure(Sender: TObject; Target: TControl; TargetPt: TPoint; var handled: boolean) of object;

  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint; var Handled: boolean) of object;

  //TSizeBtn is used internally by TplSizeControl.
  //There are 8 TSizeBtns for each target which are the target's resize handles.
  TSizeBtn = class(TCustomControl)
  private
    fTargetObj: TTargetObj;
    fPos: TBtnPos;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateBtnCursorAndColor;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(TargetObj: TTargetObj; BtnPos: TBtnPos);
  end;

  //TRegisteredObj is used internally by TplSizeControl. Each TRegisteredObj
  //contains info about a possible target control.
  TRegisteredObj = class
    fSizeCtrl: TplSizeControl; //the owner of TRegisteredObj
    fControl: TControl;
    fHooked: boolean;
    fOldWindowProc: TWndMethod;
    fOldClickMethod: TMethod;
    procedure Hook;
    procedure UnHook;
    procedure NewWindowProc(var Msg: TMessage);
  public
    constructor Create(aSizeCtrl: TplSizeControl; aControl: TControl);
    destructor Destroy; override;
  end;

  //TTargetObj is the container for each current target, and contains the 8
  //TSizeBtn objects. Any number of TTargetObj's can be contained by TplSizeControl.
  TTargetObj = class
  private
    fSizeCtrl: TplSizeControl; //the owner of TTargetObj
    fTarget: TControl;
    fBtns: array [TBtnPos] of TSizeBtn;
    fFocusRect: TRect;
    fStartRec: TRect;
    procedure Hide;
    procedure Show;
    procedure Update;
    procedure StartFocus;
    procedure MoveFocus(dx, dy: integer);
    procedure SizeFocus(dx, dy: integer; BtnPos: TBtnPos);
    procedure EndFocus;
    procedure DrawRect(dc: hDC);
  public
    constructor Create(aSizeCtrl: TplSizeControl; aTarget: TControl);
    destructor Destroy; override;
  end;

TplSizeControl = class(TComponent)
  private
    fTargetList: TList; //list of TTargetObj (current targets)
    fRegList: TList;    //list of TRegisteredObj (possible targets)
    fState: TSCState;
    fMoveOnly: boolean;
    fClipRec: TRect;
    fStartPt: TPoint;
    fEnabledBtnColor: TColor;
    fDisabledBtnColor: TColor;
    fValidBtns: TBtnPosSet;
    fMultiResize: boolean;
    fEnabled: boolean;
    fCapturedCtrl: TControl;
    fCapturedBtnPos: TBtnPos;
    fGridSize: integer;
    fOldWindowProc: TWndMethod;
    fEscCancelled: boolean;
    fParentForm: TCustomForm;
    fParentFormOLDWndMethod: TWndMethod;
    fHandle: THandle;
    fPopupMenu: TPopupMenu;
    fOnContextPopup: TContextPopupEvent;
    fLMouseDownPending: boolean;
    fStartEvent: TStartEndEvent;
    fDuringEvent: TDuringEvent;
    fEndEvent: TStartEndEvent;
    fTargetChangeEvent: TNotifyEvent;
    fOnMouseDown: TMouseDownEvent;
    fOnSetCursor: TSetCursorEvent;
    fOnKeyDown: TKeyEvent;

    function GetTargets(index: integer): TControl;
    function GetTargetCount: integer;
    procedure SetEnabled(Value: boolean);
    procedure WinProc(var TheMessage: TMessage);
    procedure FormWindowProc(var Msg: TMessage);
    procedure DoWindowProc(DefaultProc: TWndMethod; var Msg: TMessage);
    procedure DrawRect;
    procedure SetMoveOnly(Value: boolean);
    function  IsValidSizeBtn(BtnPos: TBtnPos): boolean;
    function  IsValidMove: boolean;
    procedure SetMultiResize(Value: boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure DoPopupMenuStuff;
    procedure SetEnabledBtnColor(aColor: TColor);
    procedure SetDisabledBtnColor(aColor: TColor);
    function  RegisteredCtrlFromPt(screenPt: TPoint): TControl;

    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState);

  protected
    procedure Hide;
    procedure Show;
    procedure UpdateBtnCursors;
    procedure MoveTargets(dx, dy: integer);
    procedure SizeTargets(dx, dy: integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  DoKeyDown(var Message: TLMKey): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Update: it is the responsibility of the component user to call Update
    //if the target(s) are moved or resized independently of this control
    //(eg if the form is resized and targets are aligned with it.)
    procedure Update;

    //RegisterControl: Register potential target controls with TplSizeControl
    function RegisterControl(Control: TControl): integer;
    procedure UnRegisterControl(Control: TControl);
    procedure UnRegisterAll;
    function RegisteredIndex(Control: TControl): integer;

    //AddTarget: Add any number of targets to TplSizeControl so they can be
    //resized or moved together.
    //(nb: The programmer doesn't normally need to call this method directly
    //since TplSizeControl will call it whenever a target is clicked.)
    function AddTarget(Control: TControl): integer;
    procedure DeleteTarget(Control: TControl);
    procedure ClearTargets;
    function TargetIndex(Control: TControl): integer;
    function TargetCtrlFromPt(screenPt: TPoint): TControl;

    //Enabled: This key property should be self-explanatory.
    property Enabled: boolean read fEnabled write SetEnabled;

    //Targets: used to access individual targets (read-only)
    property Targets[index: integer]: TControl read GetTargets;
    property TargetCount: integer read GetTargetCount;
  published
    //MoveOnly: ie prevents resizing
    property MoveOnly: boolean read fMoveOnly write SetMoveOnly;
    //BtnColor: Color of grab-handle buttons
    property BtnColor: TColor read fEnabledBtnColor write SetEnabledBtnColor;
    //BtnColorDisabled: eg grab buttons along aligned edges of target controls
    property BtnColorDisabled: TColor read fDisabledBtnColor write SetDisabledBtnColor;
    //GridSize: aligns mouse moved/resized controls to nearest grid dimensions
    property GridSize: integer read fGridSize write fGridSize;
    //MultiTargetResize: Resizing of multiple targets is allowed by default
    //as long as this isn't impeded by specific Target control alignments
    property MultiTargetResize: boolean read fMultiResize write SetMultiResize;
    property PopupMenu: TPopupMenu read fPopupMenu write SetPopupMenu;

    property OnStartSizeMove: TStartEndEvent read fStartEvent write fStartEvent;
    property OnDuringSizeMove: TDuringEvent read fDuringEvent write fDuringEvent;
    property OnEndSizeMove: TStartEndEvent read fEndEvent write fEndEvent;
    property OnTargetChange: TNotifyEvent read fTargetChangeEvent write fTargetChangeEvent;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnMouseDown: TMouseDownEvent read fOnMouseDown write fOnMouseDown;
    property OnSetCursor: TSetCursorEvent read fOnSetCursor write fOnSetCursor;
    property OnContextPopup: TContextPopupEvent read fOnContextPopup write fOnContextPopup;
  end;

const
  BTNSIZE = 5;
  MINWIDTH = 1;   //minimum target width   (could make this a property later)
  MINHEIGHT = 1;   //minimum target height

  CM_LMOUSEDOWN = LM_USER + $1;
  CM_RMOUSEDOWN = LM_USER + $2;

implementation

type
  THackedControl = class(TControl);
  THackedWinControl = class(TWinControl);
  TAlignSet = set of TAlign;

//turn warnings off concerning unsafe typecasts since we know they're safe...
{$WARNINGS OFF}

// =======================================================================

function max(int1, int2: integer): integer;
begin
  if int1 > int2 then
    Result := int1
  else
    Result := int2;
end;

function IsVisible(Control: TControl): boolean;
begin
  Result := True;
  while assigned(Control) do
    if Control is TCustomForm then
      exit
    else if not Control.Visible then
      break
    else
      Control := Control.Parent;
  Result := False;
end;

function GetBoundsAsScreenRect(Control: TControl): TRect;
begin
  with Control do
  begin
    Result.TopLeft := parent.ClientToScreen(BoundsRect.TopLeft);
    Result.Right := Result.Left + Width;
    Result.Bottom := Result.Top + Height;
  end;
end;

function PointIsInControl(screenPt: TPoint; Control: TControl): boolean;
begin
  Result := PtInRect(GetBoundsAsScreenRect(Control), screenPt);
end;

procedure AlignToGrid(Ctrl: TControl; ProposedBoundsRect: TRect; GridSize: integer);
begin
  if (GridSize > 1) then
  begin
    OffsetRect(ProposedBoundsRect, GridSize div 2, GridSize div 2);
    Dec(ProposedBoundsRect.Left, ProposedBoundsRect.Left mod GridSize);
    Dec(ProposedBoundsRect.Top, ProposedBoundsRect.Top mod GridSize);
    Dec(ProposedBoundsRect.Right, ProposedBoundsRect.Right mod GridSize);
    Dec(ProposedBoundsRect.Bottom, ProposedBoundsRect.Bottom mod GridSize);
  end;

  with ProposedBoundsRect do
    Ctrl.SetBounds(left, top, right, bottom);
end;

function ShiftKeyIsPressed: boolean;
begin
  Result := GetKeyState(VK_SHIFT) < 0;
end;

function CtrlKeyIsPressed: boolean;
begin
  Result := GetKeyState(VK_CONTROL) < 0;
end;

//============================= TRegisteredObj ====================================

constructor TRegisteredObj.Create(aSizeCtrl: TplSizeControl; aControl: TControl);
begin
  inherited Create;
  fSizeCtrl := aSizeCtrl;
  fControl := aControl;

  if fSizeCtrl.Enabled then
    Hook;
end;

destructor TRegisteredObj.Destroy;
begin
  UnHook;
  inherited Destroy;
end;
procedure TRegisteredObj.Hook;
var
  meth: TMethod;
begin
  if fHooked then exit;
  fOldWindowProc := fControl.WindowProc;
  fControl.WindowProc := NewWindowProc;
  fHooked := True;
end;

procedure TRegisteredObj.UnHook;
 var meth: TMethod;
begin
  if not fHooked then exit;

  fControl.WindowProc := fOldWindowProc;
  fHooked := False;
end;

procedure TRegisteredObj.NewWindowProc(var Msg: TMessage);
begin
  fSizeCtrl.DoWindowProc(fOldWindowProc, Msg);
end;

//================ TSizeBtn =======================================================

constructor TSizeBtn.Create(TargetObj: TTargetObj; BtnPos: TBtnPos);
begin
  inherited Create(nil);
  fTargetObj := TargetObj;
  fPos := BtnPos;
  Width := BTNSIZE;
  Height := BTNSIZE;
  Visible := False;
  UpdateBtnCursorAndColor;
end;

procedure TSizeBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TSizeBtn.UpdateBtnCursorAndColor;
begin
  if not (fPos in fTargetObj.fSizeCtrl.fValidBtns) or fTargetObj.fSizeCtrl.fMoveOnly then
  begin
    Cursor := crDefault;
    Color := fTargetObj.fSizeCtrl.fDisabledBtnColor;
  end
  else
  begin
    case fPos of
      bpLeft, bpRight: Cursor := crSizeWE;
      bpTop, bpBottom: Cursor := crSizeNS;
      bpTopLeft, bpBottomRight: Cursor := crSizeNWSE;
      bpTopRight, bpBottomLeft: Cursor := crSizeNESW;
    end;
    Color := fTargetObj.fSizeCtrl.fEnabledBtnColor;
  end;
end;

procedure TSizeBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
    fTargetObj.fSizeCtrl.DoMouseDown(self, Button, Shift);
end;

//=============================  TTargetObj ======================================

constructor TTargetObj.Create(aSizeCtrl: TplSizeControl; aTarget: TControl);
var
  i: TBtnPos;
begin
  inherited Create;
  fSizeCtrl := aSizeCtrl;
  fTarget := aTarget;
  for i := low(TBtnPos) to high(TBtnPos) do
    fBtns[i] := TSizeBtn.Create(self, i);
end;

destructor TTargetObj.Destroy;
var
  i: TBtnPos;
begin
  for i := low(TBtnPos) to high(TBtnPos) do
    fBtns[i].Free;
  inherited Destroy;
end;

procedure TTargetObj.Hide;
var
  i: TBtnPos;
begin
  for i := low(TBtnPos) to high(TBtnPos) do
    fBtns[i].Visible := False;
  //to avoid the buttons messing up the Size-Move Rect ...
  if fTarget is TWinControl then
    fTarget.Repaint
  else
    fTarget.Parent.Repaint;
end;

procedure TTargetObj.Show;
var
  i: TBtnPos;
begin
  for i := low(TBtnPos) to high(TBtnPos) do
    fBtns[i].Visible := True;
end;

procedure TTargetObj.Update;
var
  i: TBtnPos;
  parentForm: TCustomForm;
  tl: TPoint;
  bsDiv2: integer;
begin
  parentForm := fSizeCtrl.fParentForm;
  if not assigned(parentForm) then
    exit;

  //get tl of Target relative to parentForm ...
  tl := GetBoundsAsScreenRect(fTarget).TopLeft;
  tl := parentForm.ScreenToClient(tl);
  bsDiv2 := (BTNSIZE div 2);

  for i := low(TBtnPos) to high(TBtnPos) do
  begin
    fBtns[i].ParentWindow := parentForm.Handle; //ie keep btns separate !!!
    fBtns[i].Left := tl.X - bsDiv2;
    case i of
      bpTop, bpBottom:
        fBtns[i].Left := fBtns[i].Left + (fTarget.Width div 2);
      bpRight, bpTopRight, bpBottomRight:
        fBtns[i].Left := fBtns[i].Left + fTarget.Width - 1;
    end;
    fBtns[i].Top := tl.Y - bsDiv2;
    case i of
      bpLeft, bpRight:
        fBtns[i].Top := fBtns[i].Top + (fTarget.Height div 2);
      bpBottomLeft, bpBottom, bpBottomRight:
        fBtns[i].Top := fBtns[i].Top + fTarget.Height - 1;
    end;
    //force btns to the top ...
    SetWindowPos(fBtns[i].Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TTargetObj.StartFocus;
begin
  fFocusRect := GetBoundsAsScreenRect(fTarget);
  fStartRec := fFocusRect;
end;

procedure TTargetObj.MoveFocus(dx, dy: integer);
begin
  fFocusRect := fStartRec;
  offsetRect(fFocusRect, dx, dy);
end;

procedure TTargetObj.SizeFocus(dx, dy: integer; BtnPos: TBtnPos);
begin
  fFocusRect := fStartRec;
  case BtnPos of
    bpLeft: Inc(fFocusRect.Left, dx);
    bpTopLeft:
    begin
      Inc(fFocusRect.Left, dx);
      Inc(fFocusRect.Top, dy);
    end;
    bpTop: Inc(fFocusRect.Top, dy);
    bpTopRight:
    begin
      Inc(fFocusRect.Right, dx);
      Inc(fFocusRect.Top, dy);
    end;
    bpRight: Inc(fFocusRect.Right, dx);
    bpBottomRight:
    begin
      Inc(fFocusRect.Right, dx);
      Inc(fFocusRect.Bottom, dy);
    end;
    bpBottom: Inc(fFocusRect.Bottom, dy);
    bpBottomLeft:
    begin
      Inc(fFocusRect.Left, dx);
      Inc(fFocusRect.Bottom, dy);
    end;
  end;
end;

procedure TTargetObj.EndFocus;
begin
  //update target position ...
  fFocusRect.TopLeft := fTarget.Parent.ScreenToClient(fFocusRect.TopLeft);
  fFocusRect.BottomRight := fTarget.Parent.ScreenToClient(fFocusRect.BottomRight);
  with fFocusRect do
    AlignToGrid(fTarget, Rect(Left, top, max(MINWIDTH, right - left), max(MINHEIGHT, bottom - top)),
      fSizeCtrl.fGridSize);
  Update;
  fTarget.Invalidate;
end;

procedure TTargetObj.DrawRect(dc: hDC);
begin
  DrawFocusRect(dc, fFocusRect);
end;

//=======================  TplSizeControl =====================================

constructor TplSizeControl.Create(AOwner: TComponent);
begin
  if not (aOwner is TWinControl) then
    raise Exception.Create('TplSizeControl.Create: Owner must be a TWinControl');

  inherited Create(AOwner);
  fTargetList := TList.Create;
  fRegList := TList.Create;
  fEnabledBtnColor := clNavy;
  fDisabledBtnColor := clGray;
  fMultiResize := True;
  fValidBtns := [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight, bpBottomRight, bpBottom, bpBottomLeft];

  fParentFormOLDWndMethod := TWinControl(AOwner).WindowProc;
  TWinControl(AOwner).WindowProc := WinProc;
  fHandle := TWinControl(AOwner).Handle;

  screen.Cursors[crSize] := loadcursor(hInstance, 'NSEW');
end;

destructor TplSizeControl.Destroy;
begin
  if assigned(fTargetList) then
  begin
    if Owner<>nil then TWinControl(Owner).WindowProc:=fParentFormOLDWndMethod;

    UnRegisterAll;
    fTargetList.Free;
    fRegList.Free;
  end;
  inherited Destroy;
end;

procedure TplSizeControl.SetEnabled(Value: boolean);
var
  i: integer;
begin
  if Value = fEnabled then
    exit;

  fParentForm := GetParentForm(TWinControl(owner));
  if fParentForm = nil then
    exit;

  fEnabled := Value;
  ClearTargets;

  if fEnabled then
  begin
    //hook all registered controls and disable their OnClick events ...
    for i := 0 to fRegList.Count - 1 do
      TRegisteredObj(fRegList[i]).Hook;
    //hook the parent form too ...
    fOldWindowProc := fParentForm.WindowProc;
    fParentForm.WindowProc := FormWindowProc;
  end
  else
  begin
    //unhook all registered controls and reenable their OnClick events ...
    for i := 0 to fRegList.Count - 1 do
      TRegisteredObj(fRegList[i]).UnHook;
    //unhook the parent form too ...
    fParentForm.WindowProc := fOldWindowProc;
  end;
end;

procedure TplSizeControl.FormWindowProc(var Msg: TMessage);
begin
  DoWindowProc(fOldWindowProc, Msg);
end;

procedure TplSizeControl.WinProc(var TheMessage: TMessage);
var
  Button: TMouseButton;
  ShiftState: TShiftState;
begin

  if TheMessage.Msg = CM_LMOUSEDOWN then
  begin
    try
      fLMouseDownPending := False;
      if bool(TheMessage.WParam) then
        Button := mbLeft
      else
        Button := mbRight;
      if bool(TheMessage.LParam) then
        ShiftState := [ssShift]
      else
        ShiftState := [];
      DoMouseDown(nil, Button, ShiftState);
    except
      Application.HandleException(Self);
    end;
  end
  else
  begin
    fParentFormOLDWndMethod(TheMessage);
  end;

end;

//WindowProc for the 'hooked' form and all 'hooked' controls
procedure TplSizeControl.DoWindowProc(DefaultProc: TWndMethod; var Msg: TMessage);
var
  i: integer;
  ShiftState: TShiftState;
  controlPt, screenPt: TPoint;
  regCtrl: TControl;
  handled: boolean;

  //this seems the only reasonably simple way of managing both 'owned' and
  //'notified' LM_LBUTTONDOWN messages ...
  procedure PostMouseDownMessage(isLeftBtn, shiftKeyPressed: boolean);
  begin
    if fLMouseDownPending then
      exit;

    if assigned(fOnMouseDown) then
    begin
      getCursorPos(screenPt);
      regCtrl := RegisteredCtrlFromPt(screenPt);
      if assigned(regCtrl) then
      begin
        handled := False;
        controlPt := regCtrl.ScreenToClient(screenPt);
        fOnMouseDown(self, regCtrl, controlPt, handled);
        if handled then
          exit;
      end;
    end;

    fLMouseDownPending := True;
    PostMessage(fHandle, CM_LMOUSEDOWN, Ord(isLeftBtn), Ord(shiftKeyPressed));
  end;

begin
  case Msg.Msg of

    LM_MOUSEFIRST .. LM_MOUSELAST:
    begin
      ShiftState := KeysToShiftState(word(TLMMouse(Msg).Keys));
      case Msg.Msg of
        LM_LBUTTONDOWN: PostMouseDownMessage(True, ssShift in ShiftState);
        LM_RBUTTONDOWN: DoPopupMenuStuff;
        LM_MOUSEMOVE: DoMouseMove(nil, ShiftState);
        LM_LBUTTONUP: DoMouseUp(nil, mbLeft, ShiftState);
        //Could also add event handlers for right click events here.
      end;
      Msg.Result := 0;
    end;

    LM_PARENTNOTIFY:
      if not (TLMNotify(Msg).Msg in [LM_CREATE, LM_DESTROY]) then
      begin
        if ShiftKeyIsPressed then
          ShiftState := [ssShift]
        else
          ShiftState := [];
        case TLMNotify(Msg).Msg of
          LM_LBUTTONDOWN: PostMouseDownMessage(True, ssShift in ShiftState);
        end;
        Msg.Result := 0;
      end;

    LM_SETCURSOR:
      if (HIWORD(Msg.lParam) <> 0) then
      begin
        Msg.Result := 1;
        getCursorPos(screenPt);
        regCtrl := RegisteredCtrlFromPt(screenPt);

        handled := False;
        if assigned(fOnSetCursor) and assigned(regCtrl) then
        begin
          controlPt := regCtrl.ScreenToClient(screenPt);
          fOnSetCursor(self, RegisteredCtrlFromPt(screenPt), controlPt, handled);
        end;

        if handled then //do nothing
        else if TargetIndex(regCtrl) >= 0 then
        begin
          if not IsValidMove then
            DefaultProc(Msg)
          else
            SetCursor(screen.Cursors[crSize]);
        end
        else if assigned(regCtrl) then
          SetCursor(screen.Cursors[crHandPoint])
        else
          DefaultProc(Msg);
      end
      else
        DefaultProc(Msg);

    LM_GETDLGCODE: Msg.Result := DLGC_WANTTAB;

    LM_KEYDOWN:
    begin
      Msg.Result := 0;
      if DoKeyDown(TLMKey(Msg)) then
        exit;
      case Msg.WParam of
        VK_UP:
          if ShiftKeyIsPressed then
          begin
            SizeTargets(0, -1);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(0, -1);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        VK_DOWN:
          if ShiftKeyIsPressed then
          begin
            SizeTargets(0, +1);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(0, +1);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        VK_LEFT:
          if ShiftKeyIsPressed then
          begin
            SizeTargets(-1, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(-1, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        VK_RIGHT:
          if ShiftKeyIsPressed then
          begin
            SizeTargets(+1, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(+1, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        VK_TAB:
        begin
          if fRegList.Count = 0 then
            exit
          else if targetCount = 0 then
            AddTarget(TRegisteredObj(fRegList[0]).fControl)
          else
          begin
            i := RegisteredIndex(Targets[0]);
            if ShiftKeyIsPressed then
              Dec(i)
            else
              Inc(i);
            if i < 0 then
              i := fRegList.Count - 1
            else if i = fRegList.Count then
              i := 0;
            ClearTargets;
            AddTarget(TRegisteredObj(fRegList[i]).fControl);
          end;
        end;
        VK_ESCAPE:
          //ESCAPE is used for both -
          //  1. cancelling a mouse move/resize operation, and
          //  2. selecting the parent of the currenctly selected target
          if fState <> scsReady then
          begin
            fEscCancelled := True;
            DoMouseUp(nil, mbLeft, []);
          end
          else
          begin
            if (targetCount = 0) then
              exit;
            i := RegisteredIndex(Targets[0].Parent);
            ClearTargets;
            if i >= 0 then
              AddTarget(TRegisteredObj(fRegList[i]).fControl);
          end;
      end;
    end;

    LM_KEYUP: Msg.Result := 0;
    LM_CHAR: Msg.Result := 0;

    else
      DefaultProc(Msg);
  end;
end;

function TplSizeControl.DoKeyDown(var Message: TLMKey): boolean;
var
  ShiftState: TShiftState;
begin
  Result := True;
  if fParentForm.KeyPreview and THackedWinControl(fParentForm).DoKeyPress(Message) then
    Exit;

  if Assigned(fOnKeyDown) then
    with Message do
    begin
      ShiftState := KeyDataToShiftState(KeyData);
      fOnKeyDown(Self, CharCode, ShiftState);
      if CharCode = 0 then
        Exit;
    end;
  Result := False;
end;

function TplSizeControl.GetTargets(index: integer): TControl;
begin
  if (index < 0) or (index >= TargetCount) then
    Result := nil
  else
    Result := TTargetObj(fTargetList[index]).fTarget;
end;

function TplSizeControl.TargetIndex(Control: TControl): integer;
var
  i: integer;
begin
  Result := -1;
  if assigned(Control) then
    for i := 0 to fTargetList.Count - 1 do
      if TTargetObj(fTargetList[i]).fTarget = Control then
      begin
        Result := i;
        break;
      end;
end;

function TplSizeControl.AddTarget(Control: TControl): integer;
var
  TargetObj: TTargetObj;
begin
  Result := -1;
  if (csDestroying in ComponentState) or (fState <> scsReady) then
    exit;
  Result := TargetIndex(Control);
  if not assigned(Control) or not Control.Visible or (Control is TCustomForm) or (Result >= 0) then
    exit;

  Result := fTargetList.Count;
  TargetObj := TTargetObj.Create(self, Control);
  fTargetList.Add(TargetObj);
  UpdateBtnCursors;
  TargetObj.Update;
  TargetObj.Show;
  RegisterControl(Control);

  if fParentForm <> nil then fParentForm.ActiveControl := nil;

  if assigned(fTargetChangeEvent) then  fTargetChangeEvent(self);
end;

procedure TplSizeControl.DeleteTarget(Control: TControl);
var
  i: integer;
begin
  i := TargetIndex(Control);
  if i < 0 then
    exit;
  TTargetObj(fTargetList[i]).Free;
  fTargetList.Delete(i);
  UpdateBtnCursors;
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);
end;

procedure TplSizeControl.ClearTargets;
var
  i: integer;
begin
  if fTargetList.Count = 0 then  exit;

  for i := 0 to fTargetList.Count - 1 do
    TTargetObj(fTargetList[i]).Free;

  fTargetList.Clear;
  if (csDestroying in ComponentState) then  exit;

  UpdateBtnCursors;
  if assigned(fTargetChangeEvent) then  fTargetChangeEvent(self);
end;

function TplSizeControl.RegisterControl(Control: TControl): integer;
var
  RegisteredObj: TRegisteredObj;
begin
  if not IsVisible(Control) then
  begin
    Result := -1;
    exit;
  end;

  Result := RegisteredIndex(Control);

  if Result >= 0 then exit;

  Result := fRegList.Count;
  RegisteredObj := TRegisteredObj.Create(self, Control);
  fRegList.Add(RegisteredObj);
end;


procedure TplSizeControl.UnRegisterControl(Control: TControl);
var
  i: integer;
begin
  //first, make sure it's not a current target ...
  DeleteTarget(Control);
  //now unregister it ...
  i := RegisteredIndex(Control);

  if i < 0 then exit;

  TRegisteredObj(fRegList[i]).Free;
  fRegList.Delete(i);
end;

procedure TplSizeControl.UnRegisterAll;
var
  i: integer;
begin
  //first, clear any targets
  ClearTargets;
  //now, clear all registered controls ...
  for i := 0 to fRegList.Count - 1 do
    TRegisteredObj(fRegList[i]).Free;

  fRegList.Clear;
end;

function TplSizeControl.RegisteredIndex(Control: TControl): integer;
 var i: integer;
begin
  Result := -1;
  for i := 0 to fRegList.Count - 1 do
    if TRegisteredObj(fRegList[i]).fControl = Control then
    begin
      Result := i;
      break;
    end;
end;

function TplSizeControl.TargetCtrlFromPt(screenPt: TPoint): TControl;
var
  i: integer;
  tmpCtrl: TWinControl;
begin
  //nb: If controls overlap at screenPt, then the (top-most) child control
  //is selected if there is a parent-child relationship. Otherwise, simply
  //the first control under screenPt is returned.
  Result := nil;
  for i := fTargetList.Count - 1 downto 0 do
    with TTargetObj(fTargetList[i]) do
    begin
      if not PointIsInControl(screenPt, fTarget) then
        continue;
      if not (fTarget is TWinControl) then
      begin
        Result := fTarget;
        exit; //ie assume this is top-most since it can't be a parent.
      end
      else if not assigned(Result) then
        Result := fTarget
      else
      begin
        tmpCtrl := TWinControl(fTarget).Parent;
        while assigned(tmpCtrl) and (tmpCtrl <> Result) do
          tmpCtrl := tmpCtrl.Parent;
        if assigned(tmpCtrl) then
          Result := fTarget;
      end;
    end;
end;

function TplSizeControl.RegisteredCtrlFromPt(screenPt: TPoint): TControl;
var
  i: integer;
  tmpCtrl: TWinControl;
begin
  //nb: If controls overlap at screenPt, then the (top-most) child control
  //is selected if there is a parent-child relationship. Otherwise, simply
  //the first control under screenPt is returned.
  Result := nil;
  for i := fRegList.Count - 1 downto 0 do
    with TRegisteredObj(fRegList[i]) do
    begin
      if not PointIsInControl(screenPt, fControl) then
        continue;
      if not (fControl is TWinControl) then
      begin
        Result := fControl;
        exit; //ie assume this is top-most since it can't be a parent.
      end
      else if not assigned(Result) then
        Result := fControl
      else
      begin
        tmpCtrl := TWinControl(fControl).Parent;
        while assigned(tmpCtrl) and (tmpCtrl <> Result) do
          tmpCtrl := tmpCtrl.Parent;
        if assigned(tmpCtrl) then
          Result := fControl;
      end;
    end;
end;

function TplSizeControl.GetTargetCount: integer;
begin
  Result := fTargetList.Count;
end;

procedure TplSizeControl.MoveTargets(dx, dy: integer);
var
  i: integer;
begin
  if not IsValidMove then
    exit;
  for i := 0 to fTargetList.Count - 1 do
    with TTargetObj(fTargetList[i]) do
    begin
      with fTarget do
        SetBounds(Left + dx, Top + dy, Width, Height);
      Update;
    end;
end;

procedure TplSizeControl.SizeTargets(dx, dy: integer);
var
  i: integer;
begin
  if MoveOnly then
    exit;
  if (dx <> 0) and not (IsValidSizeBtn(bpLeft) or IsValidSizeBtn(bpRight)) then  exit;
  if (dy <> 0) and not (IsValidSizeBtn(bpBottom) or IsValidSizeBtn(bpTop)) then  exit;

  for i := 0 to fTargetList.Count - 1 do
    with TTargetObj(fTargetList[i]) do
    begin
      with fTarget do
        SetBounds(Left, Top, max(MINWIDTH, Width + dx), max(MINHEIGHT, Height + dy));
      Update;
    end;
end;

procedure TplSizeControl.Update;
var
  i: integer;
begin
  for i := 0 to fTargetList.Count - 1 do
    TTargetObj(fTargetList[i]).Update;
end;

procedure TplSizeControl.DrawRect;
var
  i: integer;
  dc: hDC;
begin
  if TargetCount = 0 then exit;
  dc := GetDC(0);
  try
    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).DrawRect(dc);
  finally
    ReleaseDC(0, dc);
  end;
end;

procedure TplSizeControl.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
var
  i, targetIdx: integer;
  p: TWinControl;
  parentClientRec: TRect;
  targetObj: TTargetObj;
begin
  fEscCancelled := False;
  GetCursorPos(fStartPt);
  if (Sender is TSizeBtn) then
  begin
    if fMoveOnly then  exit; //should never happen
    targetObj := TSizeBtn(Sender).fTargetObj;
    fCapturedCtrl := targetObj.fTarget;
    fCapturedBtnPos := TSizeBtn(Sender).fPos;
    //make sure we're allowed to size these targets with this button ...
    if not IsValidSizeBtn(fCapturedBtnPos) then  exit;
    fState := scsSizing;
  end
  else
  begin
    //First find the top-most control that's clicked ...
    //nb: It's so much simpler to do this here than try and work it out from
    //the WindowProc owner (because of disabled controls & non-TWinControls.)
    fCapturedCtrl := RegisteredCtrlFromPt(fStartPt);

    targetIdx := TargetIndex(fCapturedCtrl);
    if not (ssShift in Shift) and (targetIdx < 0) then
      ClearTargets;
    if not assigned(fCapturedCtrl) then
      exit;

    //if the control isn't a target then add it ...
    if targetIdx < 0 then
    begin
      AddTarget(fCapturedCtrl);
      exit;
      //if the control's already a target but the Shift key's pressed then delete it ...
    end
    else if (ssShift in Shift) then
    begin
      DeleteTarget(fCapturedCtrl);
      fCapturedCtrl := nil;
      exit;
    end;
    fParentForm.ActiveControl := nil;
    if not IsValidMove then
      exit;
    targetObj := TTargetObj(fTargetList[targetIdx]);
    fState := scsMoving;
  end;

  for i := 0 to TargetCount - 1 do
    TTargetObj(fTargetList[i]).StartFocus;

  if assigned(fStartEvent) then fStartEvent(self, fState);

  //now calculate and set the clipping region in screen coords ...
  p := targetObj.fTarget.Parent;
  parentClientRec := p.ClientRect;
  parentClientRec.TopLeft := p.ClientToScreen(parentClientRec.TopLeft);
  parentClientRec.BottomRight := p.ClientToScreen(parentClientRec.BottomRight);
  if fState = scsMoving then
  begin
    fClipRec := parentClientRec;
  end
  else
    with targetObj do //ie sizing
    begin
      fClipRec := fFocusRect;
      case TSizeBtn(Sender).fPos of
        bpLeft: fClipRec.Left := parentClientRec.Left;
        bpTopLeft:
        begin
          fClipRec.Left := parentClientRec.Left;
          fClipRec.Top := parentClientRec.Top;
        end;
        bpTop: fClipRec.Top := parentClientRec.Top;
        bpTopRight:
        begin
          fClipRec.Right := parentClientRec.Right;
          fClipRec.Top := parentClientRec.Top;
        end;
        bpRight: fClipRec.Right := parentClientRec.Right;
        bpBottomRight:
        begin
          fClipRec.Right := parentClientRec.Right;
          fClipRec.Bottom := parentClientRec.Bottom;
        end;
        bpBottom: fClipRec.Bottom := parentClientRec.Bottom;
        bpBottomLeft:
        begin
          fClipRec.Left := parentClientRec.Left;
          fClipRec.Bottom := parentClientRec.Bottom;
        end;
      end;
    end;
  //ClipCursor(@fClipRec);

  Hide;
  DrawRect;
  THackedControl(fCapturedCtrl).MouseCapture := True;
end;

procedure TplSizeControl.DoMouseMove(Sender: TObject; Shift: TShiftState);
var
  i, dx, dy: integer;
  newPt: TPoint;
begin

  if (fState = scsReady) or not assigned(fCapturedCtrl) then
    exit;
  DrawRect;

  GetCursorPos(newPt);

  dx := newPt.X - fStartPt.X;
  dy := newPt.Y - fStartPt.Y;

  if (fState = scsSizing) then
  begin
    case fCapturedBtnPos of
      bpLeft, bpRight: dy := 0;
      bpTop, bpBottom: dx := 0;
    end;
    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).SizeFocus(dx, dy, fCapturedBtnPos);
  end
  else
    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).MoveFocus(dx, dy);

  if assigned(fDuringEvent) then
    fDuringEvent(self, dx, dy, fState);
  DrawRect;
end;

procedure TplSizeControl.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
var
  i: integer;
begin
  if fState = scsReady then
    exit;
  DrawRect;
  // ClipCursor(nil);
  THackedControl(fCapturedCtrl).MouseCapture := False;
  fCapturedCtrl := nil;

  if not fEscCancelled then
    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).EndFocus;

  fEscCancelled := False;
  if assigned(fEndEvent) then
    fEndEvent(self, fState);

  Show;
  fState := scsReady;
end;

procedure TplSizeControl.Hide;
var
  i: integer;
begin
  for i := 0 to TargetCount - 1 do
    TTargetObj(fTargetList[i]).Hide;
end;

procedure TplSizeControl.Show;
var
  i: integer;
begin
  for i := 0 to TargetCount - 1 do
    TTargetObj(fTargetList[i]).Show;
end;

procedure TplSizeControl.UpdateBtnCursors;
var
  i: integer;
  j: TBtnPos;
begin
  if fMultiResize or (TargetCount = 1) then
  begin
    fValidBtns := [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight, bpBottomRight, bpBottom, bpBottomLeft];
    for i := 0 to TargetCount - 1 do
      case TTargetObj(fTargetList[i]).fTarget.Align of
        alTop: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight,
            bpBottomRight, bpBottomLeft];
        alBottom: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTopRight, bpRight,
            bpBottomRight, bpBottom, bpBottomLeft];
        alLeft: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTop, bpTopRight, bpBottomRight,
            bpBottom, bpBottomLeft];
        alRight: fValidBtns := fValidBtns - [bpTopLeft, bpTop, bpTopRight, bpRight,
            bpBottomRight, bpBottom, bpBottomLeft];
        alClient: fValidBtns := [];
      end;
  end
  else
    fValidBtns := [];

  for i := 0 to TargetCount - 1 do
    with TTargetObj(fTargetList[i]) do
      for j := low(TBtnPos) to high(TBtnPos) do
        fBtns[j].UpdateBtnCursorAndColor;
end;

procedure TplSizeControl.SetMoveOnly(Value: boolean);
begin
  if fMoveOnly = Value then
    exit;
  fMoveOnly := Value;
  UpdateBtnCursors;
end;

function TplSizeControl.IsValidSizeBtn(BtnPos: TBtnPos): boolean;
begin
  Result := (TargetCount > 0) and (TTargetObj(fTargetList[0]).fBtns[BtnPos].Cursor <> crDefault);
end;

function TplSizeControl.IsValidMove: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to TargetCount - 1 do
    if (TTargetObj(fTargetList[i]).fTarget.Align <> alNone) then
      exit;
  Result := True;
end;

procedure TplSizeControl.SetMultiResize(Value: boolean);
begin
  if Value = fMultiResize then
    exit;
  fMultiResize := Value;
  UpdateBtnCursors;
end;

procedure TplSizeControl.SetEnabledBtnColor(aColor: TColor);
begin
  if fEnabledBtnColor = aColor then
    exit;
  fEnabledBtnColor := aColor;
  UpdateBtnCursors;
end;

procedure TplSizeControl.SetDisabledBtnColor(aColor: TColor);
begin
  if fDisabledBtnColor = aColor then
    exit;
  fDisabledBtnColor := aColor;
  UpdateBtnCursors;
end;

procedure TplSizeControl.SetPopupMenu(Value: TPopupMenu);
begin
  fPopupMenu := Value;
  if Value = nil then
    exit;
  Value.FreeNotification(Self);
end;

procedure TplSizeControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

procedure TplSizeControl.DoPopupMenuStuff;
var
  Handled: boolean;
  pt: TPoint;
  targetCtrl: TControl;
begin
  if not assigned(fPopupMenu) then
    exit;
  GetCursorPos(pt);
  targetCtrl := TargetCtrlFromPt(pt);
  if not assigned(targetCtrl) then
    exit;
  Handled := False;
  if Assigned(FOnContextPopup) then
    fOnContextPopup(Self, pt, Handled);
  if Handled then
    exit;
  THackedControl(owner).ExecuteCancelAction;
  fPopupMenu.PopupComponent := targetCtrl;
  PopupMenu.Popup(Pt.X, Pt.Y);
end;


end.
