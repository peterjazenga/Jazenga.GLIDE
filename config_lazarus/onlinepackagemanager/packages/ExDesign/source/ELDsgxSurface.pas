{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgxSurface;

{$mode objfpc}{$H+}
{$DEFINE NO_DESIGNHOOK}

interface

uses
  Classes, SysUtils,
  LCLProc, LCLType, LResources, LCLIntf, LMessages,
  Forms, Controls, Graphics,
  Dialogs,
  ExtCtrls, Contnrs,
  Clipbrd,
  Types;


type
  TplDesignSurface = class;

  TplDesignMessage = function(ASender: TControl; var AMsg: TLMessage;
    const APt: TPoint): Boolean of object;

  TplDesignCustomMessenger = class(TObject)
  private
    FContainer: TWinControl;
    FOnDesignMessage: TplDesignMessage;
  protected
    procedure SetContainer(AValue: TWinControl); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage): Boolean; virtual;
    procedure Clear; virtual;
    procedure DesignChildren(AContainer: TWinControl; ADesigning: Boolean);
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); virtual;
    property Container: TWinControl read FContainer write SetContainer;
    property OnDesignMessage: TplDesignMessage read FOnDesignMessage write FOnDesignMessage;
  end;

  TplDesignCustomMessengerClass = class of TplDesignCustomMessenger;

  TplDesignMessageHook = class(TObject)
  private
    FClient: TWinControl;
    FOldProc: TWndMethod;
    FUser: TplDesignCustomMessenger;
  protected
    procedure HookProc(var AMessage: TLMessage);
    procedure Unhook;
  public
    constructor Create(AUser: TplDesignCustomMessenger; AClient: TWinControl);
    destructor Destroy; override;
    property Client: TWinControl read FClient;
  end;

  TplDesignCustomController = class(TObject)
  private
    FSurface: TplDesignSurface;
    FShift: TShiftState; //CV
  protected
    function GetDragRect: TRect; virtual; abstract;
    function KeyDown(AKeyCode: Cardinal): Boolean; virtual; abstract;
    function KeyUp(AKeyCode: Cardinal): Boolean; virtual; abstract;
    function MouseDown(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean; virtual; abstract;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; virtual; abstract;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean; virtual; abstract;
  public
    constructor Create(ASurface: TplDesignSurface); virtual;
    property DragRect: TRect read GetDragRect;
    property Shift666: TShiftState read FShift write FShift; //CV
    property Surface: TplDesignSurface read FSurface;
  end;

  TplDesignCustomControllerClass = class of TplDesignCustomController;

  TplDesignHandleId = (dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
    dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom);

  TplDesignCustomSelector = class(TComponent)
  private
    FSurface: TplDesignSurface;
  protected
    function GetCount: Integer; virtual; abstract;
    function GetSelection(AIndex: Integer): TControl;  virtual; abstract;
    procedure SetSelection(AIndex: Integer; AValue: TControl); virtual; abstract;
  public
    constructor Create(ASurface: TplDesignSurface); reintroduce; virtual;
    destructor Destroy; override;
    function IsSelected(AValue: TControl): Boolean; virtual; abstract;
    function GetClientControl(AControl: TControl): TControl; virtual; abstract;
    function GetCursor(AX, AY: Integer): TCursor; virtual; abstract;
    function GetHitHandle(AX, AY: Integer): TplDesignHandleId; virtual; abstract;
    procedure AddToSelection(AValue: TControl); virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure RemoveFromSelection(AValue: TControl); virtual; abstract;
    procedure ToggleSelection(AValue: TControl);
    procedure Update; virtual; abstract;
    property Count: Integer read GetCount;
    property Selection[AIndex: Integer]: TControl read GetSelection write SetSelection;
    property Surface: TplDesignSurface read FSurface;
  end;

  TplDesignCustomSelectorClass = class of TplDesignCustomSelector;

  TplDesignObjectArray = array of TObject;
  TplDesignGetAddClassEvent = procedure(Sender: TObject; var ioClass: string) of object;


  TplDesignSurface = class(TComponent)
  private
    FActive: Boolean;
    FAddClass: string;
    FContainer: TWinControl;
    FContainerHook: TplDesignMessageHook;
    FController: TplDesignCustomController;
    FControllerClass: TplDesignCustomControllerClass;
    FMessenger: TplDesignCustomMessenger;
    FMessengerClass: TplDesignCustomMessengerClass;
    FSelector: TplDesignCustomSelector;
    FSelectorClass: TplDesignCustomSelectorClass;
    FUpdateOwner: TComponent;
  protected
    FOnChange: TNotifyEvent;
    FOnGetAddClass: TplDesignGetAddClassEvent;
    FOnSelectionChange: TNotifyEvent;
    function GetAddBounds: TRect;
    function GetCount: Integer;
    function GetSelected: TplDesignObjectArray;
    function GetSelectedContainer: TWinControl;
    function GetSelection(AIndex: Integer): TControl;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedContainer;
    procedure NeedController;
    procedure NeedMessenger;
    procedure NeedSelector;
    procedure ReaderError(Reader: TReader; const Msg: string; var Handled: Boolean);
    procedure SetActive(AValue: Boolean);
    procedure SetContainer(AValue: TWinControl);
    procedure SetSelection(AIndex: Integer; AValue: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clear: TplDesignSurface;
    function ContainerToSelectedContainer(const APt: TPoint): TPoint;
    function FindControl(AX, AY: Integer): TControl; virtual;
    function GetCursor(AX, AY: Integer): TCursor; virtual;
    function GetHitHandle(AX, AY: Integer): TplDesignHandleId; virtual;
    function IsDesignMessage(ASender: TControl; var AMsg: TLMessage; const APt: TPoint): Boolean;
    function LoadFromFile(const AFileName: string): TplDesignSurface;
    function LoadFromStream(AStream: TStream): TplDesignSurface;
    procedure AddComponent;
    procedure Change;
    procedure ClearSelection;
    procedure CopyComponents;
    procedure CutComponents;
    procedure DeleteComponents;
    procedure GetAddClass;
    procedure GrowComponents(AGrowWidth, AGrowHeight: Integer);
    procedure NudgeComponents(ANudgeLeft, ANudgeTop: Integer);
    procedure PasteComponents;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure Select(AControl: TControl);
    procedure SelectionChange;
    procedure SelectParent;
    procedure SetSelected(const AValue: array of TObject);
    procedure UpdateDesigner; virtual;
    property Active: Boolean read FActive write SetActive;
    property AddClass: string read FAddClass write FAddClass;
    property Controller: TplDesignCustomController read FController;
    property ControllerClass: TplDesignCustomControllerClass read FControllerClass write FControllerClass;
    property Count: Integer read GetCount;
    property Messenger: TplDesignCustomMessenger read FMessenger;
    property MessengerClass: TplDesignCustomMessengerClass read FMessengerClass write FMessengerClass;
    property Selected: TplDesignObjectArray read GetSelected;
    property SelectedContainer: TWinControl read GetSelectedContainer;
    property Selection[AIndex: Integer]: TControl read GetSelection write SetSelection;
    property Selector: TplDesignCustomSelector read FSelector;
    property SelectorClass: TplDesignCustomSelectorClass read FSelectorClass write FSelectorClass;
  published
    property Container: TWinControl read FContainer write SetContainer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAddClass: TplDesignGetAddClassEvent read FOnGetAddClass write FOnGetAddClass;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

  TplDesignScrollBox = class(TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl);
  end;

  TplDesignPanel = class(TPanel)
  private
    FSurface: TplDesignSurface;
    FOnPaint: TNotifyEvent;
    FDrawRules: Boolean;
    function GetActive: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnGetAddClass: TplDesignGetAddClassEvent;
    function GetOnSelectionChange: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnGetAddClass(const Value: TplDesignGetAddClassEvent);
    procedure SetOnSelectionChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure Paint; override;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SetDrawRules(const Value: Boolean);
    property Active: Boolean read GetActive write SetActive;
    property Canvas;
    property Surface: TplDesignSurface read FSurface;
  published
    property DrawRules: Boolean read FDrawRules write SetDrawRules default True;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnGetAddClass: TplDesignGetAddClassEvent read GetOnGetAddClass write SetOnGetAddClass;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
  end;


implementation

uses
  ELDsgxUtils, ELDsgxClip, ELDsgxImp, ELDsgxResources, ELDsgxTypes;

//=== { TplDesignCustomMessenger } ===========================================

constructor TplDesignCustomMessenger.Create;
begin
  //
end;

destructor TplDesignCustomMessenger.Destroy;
begin
  //
end;

procedure TplDesignCustomMessenger.Clear;
begin
  //
end;

procedure TplDesignCustomMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  //
end;

procedure TplDesignCustomMessenger.DesignChildren(AContainer: TWinControl; ADesigning: Boolean);
var
  I: Integer;
begin
  for I := 0 to AContainer.ControlCount - 1 do
    DesignComponent(AContainer.Controls[I], ADesigning);
end;

procedure TplDesignCustomMessenger.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

function TplDesignCustomMessenger.IsDesignMessage(ASender: TControl;
  var AMessage: TLMessage): Boolean;

  function MousePoint: TPoint;
  begin
    with TLMMouse(AMessage) do
      MousePoint := Point(XPos, YPos);
    Result := DesignClientToParent(Result, ASender, Container);
  end;

begin
  if not Assigned(FOnDesignMessage) then
    Result := False
  else
    case AMessage.Msg of
      LM_MOUSEFIRST..LM_MOUSELAST:
        Result := FOnDesignMessage(ASender, AMessage, MousePoint);
      LM_KEYDOWN..LM_KEYUP, LM_PAINT, LM_ERASEBKGND, LM_WINDOWPOSCHANGED, CN_KEYDOWN..CN_KEYUP:
        Result := FOnDesignMessage(ASender, AMessage, Point(0, 0));
      else
        Result := False;
    end;
end;

//=== { TplDesignMessageHook } ===============================================

constructor TplDesignMessageHook.Create(AUser: TplDesignCustomMessenger;
  AClient: TWinControl);
begin
  FUser := AUser;
  FClient := AClient;
  FOldProc := FClient.WindowProc;
  FClient.WindowProc := @HookProc;
end;

destructor TplDesignMessageHook.Destroy;
begin
  Unhook;
  inherited Destroy;
end;

procedure TplDesignMessageHook.Unhook;
begin
  FClient.WindowProc := FOldProc;
end;

procedure TplDesignMessageHook.HookProc(var AMessage: TLMessage);
begin
  if not FUser.IsDesignMessage(FClient, AMessage) then
    FOldProc(AMessage);
end;


constructor TplDesignCustomController.Create(ASurface: TplDesignSurface);
begin
  FSurface := ASurface;
end;


function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then Include(Result, ssMiddle);
end;


constructor TplDesignCustomSelector.Create(ASurface: TplDesignSurface);
begin
  inherited Create(nil);
  FSurface := ASurface;
end;

destructor TplDesignCustomSelector.Destroy;
begin
  inherited Destroy;
end;

procedure TplDesignCustomSelector.ToggleSelection(AValue: TControl);
begin
  if IsSelected(AValue) then
    RemoveFromSelection(AValue)
  else
    AddToSelection(AValue);
end;


constructor TplDesignSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessengerClass := TplDesignDesignerMessenger;
  FControllerClass := TplDesignController;
  FSelectorClass := TplDesignSelector;
  //FDrawGrid := True;
end;

destructor TplDesignSurface.Destroy;
begin
  FContainerHook.Free;
  Messenger.Free;
  Controller.Free;
  Selector.Free;
  inherited Destroy;
end;

procedure TplDesignSurface.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TplDesignSurface.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

procedure TplDesignSurface.NeedContainer;
begin
  if (Container = nil) and (Owner is TWinControl) then
    Container := TWinControl(Owner);
  if Container = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Container']);
end;

procedure TplDesignSurface.NeedController;
begin
  if (Controller = nil) and (ControllerClass <> nil) then
    FController := ControllerClass.Create(Self);
  if Controller = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Controller']);
end;

procedure TplDesignSurface.NeedMessenger;
begin
  if (Messenger = nil) and (MessengerClass <> nil) then
  begin
    FMessenger := MessengerClass.Create;
    Messenger.OnDesignMessage := @IsDesignMessage;
  end;
  if Messenger = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Messenger']);
end;

procedure TplDesignSurface.NeedSelector;
begin
  if (Selector = nil) and (SelectorClass <> nil) then
    FSelector := SelectorClass.Create(Self);
  if Selector = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Selector']);
end;

procedure TplDesignSurface.SetActive(AValue: Boolean);

  procedure Activate;
  begin
    NeedContainer;
    NeedController;
    NeedSelector;
    NeedMessenger;
    Messenger.Container := Container;
    FContainerHook := TplDesignMessageHook.Create(Messenger, Container);
  end;

  procedure Deactivate;
  begin
    FreeAndNil(FContainerHook);
    Selector.ClearSelection;
    FreeAndNil(FMessenger);
  end;

begin
  if FActive <> AValue then
  begin
    if AValue then
      Activate
    else
      Deactivate;
    FActive := AValue;
    SelectionChange;
    if Assigned(Container) then
      Container.Invalidate;
  end;
end;

procedure TplDesignSurface.UpdateDesigner;
begin
  Selector.Update;
end;

function TplDesignSurface.GetCount: Integer;
begin
  Result := Selector.Count;
end;

function TplDesignSurface.GetSelection(AIndex: Integer): TControl;
begin
  Result := Selector.Selection[AIndex];
end;

procedure TplDesignSurface.SetSelection(AIndex: Integer; AValue: TControl);
begin
  Selector.Selection[AIndex] := AValue;
end;

procedure TplDesignSurface.ClearSelection;
begin
  Selector.ClearSelection;
end;

procedure TplDesignSurface.SelectionChange;
begin
  if not (csDestroying in ComponentState) and Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TplDesignSurface.GetSelected: TplDesignObjectArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Selector.Selection[I];
end;

procedure TplDesignSurface.SetSelected(const AValue: array of TObject);
var
  I: Integer;
begin
  ClearSelection;
  for I := 0 to Length(AValue) - 1 do
    if AValue[I] is TControl then
      Selector.AddToSelection(TControl(AValue[I]));
end;

procedure TplDesignSurface.Select(AControl: TControl);
begin
  ClearSelection;
  if AControl <> nil then
    Selector.AddToSelection(AControl);
end;

function TplDesignSurface.FindControl(AX, AY: Integer): TControl;
var
  C, C0: TControl;
  P: TPoint;
begin
  P := Point(AX, AY);
  C := Container.ControlAtPos(P, True, True);
  while (C <> nil) and (C is TWinControl) do
  begin
    Dec(P.X, C.Left);
    Dec(P.Y, C.Top);
    C0 := TWinControl(C).ControlAtPos(P, True, True);
    if (C0 = nil) or (C0.Owner <> C.Owner) then
      Break;
    C := C0;
  end;
  if C = nil then
    C := Container;
  Result := Selector.GetClientControl(C);
end;

function TplDesignSurface.GetSelectedContainer: TWinControl;
begin
  if Count <> 1 then
    Result := Container
  else
  if (Selection[0] is TWinControl) and
    (csAcceptsControls in Selection[0].ControlStyle) then
    Result := TWinControl(Selection[0])
  else
    Result := Selection[0].Parent;
end;

function TplDesignSurface.ContainerToSelectedContainer(const APt: TPoint): TPoint;
var
  C: TControl;
begin
  Result := APt;
  C := SelectedContainer;
  while (C <> Container) and (C <> nil) do
  begin
    Dec(Result.X, C.Left);
    Dec(Result.Y, C.Top);
    C := C.Parent;
  end;
end;

function TplDesignSurface.GetAddBounds: TRect;
begin
  with Result, Controller do
  begin
    TopLeft := ContainerToSelectedContainer(DragRect.TopLeft);
    BottomRight := ContainerToSelectedContainer(DragRect.BottomRight);
  end;
end;

procedure TplDesignSurface.GetAddClass;
begin
  if Assigned(FOnGetAddClass) then
    FOnGetAddClass(Self, FAddClass);
end;

procedure TplDesignSurface.AddComponent;
var
  CC: TComponentClass;
  C: TComponent;
  CO: TControl;

  function GetBounds: TRect;
  begin
    Result := GetAddBounds;
    if DesignRectWidth(Result) = 0 then
      Result.Right := Result.Left + CO.Width;
    if DesignRectHeight(Result) = 0 then
      Result.Bottom := Result.Top + CO.Height;
  end;

begin
  CC := TComponentClass(GetClass(AddClass));
  if (CC <> nil) and (SelectedContainer <> nil) then
  begin
    //C := CC.Create(Owner);
    //C.Name := DesignUniqueName(Owner, AddClass);
    C := CC.Create(Container);
    C.Name := DesignUniqueName(Container, AddClass);
    if C is TControl then
    begin
      CO := TControl(C);
      CO.Parent := SelectedContainer;
      CO.BoundsRect := GetBounds;
      Select(CO);
    end;
    Messenger.DesignComponent(C, Active);
    SelectionChange;
    Change;
    AddClass := '';
  end;
end;

procedure TplDesignSurface.NudgeComponents(ANudgeLeft, ANudgeTop: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Left := Left + ANudgeLeft;
      Top := Top + ANudgeTop;
    end;
  Change;
end;

procedure TplDesignSurface.GrowComponents(AGrowWidth, AGrowHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Width := DesignMax(1, Width + AGrowWidth);
      Height := DesignMax(1, Height + AGrowHeight);
    end;
  Change;
end;

procedure TplDesignSurface.DeleteComponents;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
      Selection[I].Free;
    ClearSelection;
    SelectionChange;
    Change;
  end;
end;

procedure TplDesignSurface.CopyComponents;
var
  I: Integer;
begin
  with TplDesignComponentClipboard.Create(Container) do
  try
    OpenWrite;
    try
      for I := 0 to Count - 1 do
        SetComponent(Selection[I]);
    finally
      CloseWrite;
    end;
  finally
    Free;
  end;
end;

procedure TplDesignSurface.CutComponents;
begin
  CopyComponents;
  DeleteComponents;
end;

procedure TplDesignSurface.PasteComponents;
var
  CO: TControl;
  C: TComponent;
  P: TWinControl;

  procedure KeepInParent;
  begin
    with P do
    begin
      if CO.Left > ClientWidth then
        CO.Left := ClientWidth - CO.Width;
      if CO.Top > ClientHeight then
        CO.Top := ClientHeight - CO.Height;
    end;
  end;

  procedure PasteComponent;
  begin
    C.Name := DesignUniqueName(Owner, C.ClassName);
    Owner.InsertComponent(C);
    if C is TControl then
    begin
      CO := TControl(C);
      KeepInParent;
      CO.Parent := P;
      Selector.AddToSelection(CO);
    end;
  end;

begin
  with TplDesignComponentClipboard.Create(Container) do
  try
    OpenRead;
    try
      C := GetComponent;
      if (C <> nil) then
      begin
        P := SelectedContainer;
        ClearSelection;
        repeat
          PasteComponent;
          C := GetComponent;
        until C = nil;
        SelectionChange;
        Change;
      end;
    finally
      CloseRead;
    end;
  finally
    Free;
  end;
end;

procedure TplDesignSurface.SelectParent;
begin
  if Count > 0 then
    Select(Selection[0].Parent);
end;



type
  TAccessWinControl = class(TWinControl);

function TplDesignSurface.IsDesignMessage(ASender: TControl;
  var AMsg: TLMessage; const APt: TPoint): Boolean;

  function VirtKey: Cardinal;
  begin
    Result := AMsg.WParam;
  end;

var
  PosChangedHandle: HWND;
  I: Integer;
  Control: TAccessWinControl;
begin
  if not Active then
    Result := False
  else
    case AMsg.Msg of
{
      WM_ERASEBKGND:
        Result := HandleEraseBkgnd;
      WM_PAINT:
        Result := HandlePaint;
}
      LM_LBUTTONDOWN:
        Result := Controller.MouseDown(mbLeft, APt.X, APt.Y, TLMMOUSE(AMsg));
      LM_LBUTTONUP:
        Result := Controller.MouseUp(mbLeft, APt.X, APt.Y, TLMMouse( aMsg));
      LM_MOUSEMOVE:
      begin
        Result := Controller.MouseMove(APt.X, APt.Y, TLMMouse( aMsg));
      end;
      LM_KEYDOWN, CN_KEYDOWN:
        Result := Controller.KeyDown(VirtKey);
      LM_KEYUP, CN_KEYUP:
        Result := Controller.KeyUp(VirtKey);
     {LM_WINDOWPOSCHANGED:
        begin
          if AMsg.lParam <> 0 then
          begin
            //CVPosChangedHandle := PWindowPos(AMsg.lParam).hwnd;
            PosChangedHandle := PWindowPos(AMsg.lParam)^.hwnd;

            // If the window that has changed is a control owned by our container
            // then we must update the designer. This allows to programatically
            // change the location of a control while making the designer handles
            // follow it around (Mantis 4693).
            // For this to work properly, we MUST update the bounds of the
            // control before calling UpdateDesigner because the VCL has not yet
            // processed the WM_WINDOWPOSCHANGED message when this code executes.
            // If we did not, the designer would use the previous position of the
            // control to display the handles.
            // Additionnaly, we must not work with controls that don't have their
            // handle allocated. In some instances, creating the handle may trigger
            // a second WM_WINDOWPOSCHANGED message, thus leading to an infinite
            // loop and a crash (Mantis 5225)
            for I := 0 to Container.ComponentCount - 1 do
            begin
              if Container.Components[I] is TWinControl then
              begin
                Control := TAccessWinControl(Container.Components[I]);
                if Control.HandleAllocated and (PosChangedHandle = Control.Handle) then
                begin
                  if not (csDestroyingHandle in Control.ControlState) then
                   //$IFDEF DELPHI10_UP
                    //CV Control.UpdateBounds;
                    //$ELSE
                    Control.Dispatch(AMsg);
                    //$ENDIF DELPHI10_UP

                  UpdateDesigner;
                end;
              end;
            end;//for
          end;

          // Must return False to let the VCL do its own work of placing the window
          Result := False;
        end;   }
      else
        Result := False;
    end;
end;

function TplDesignSurface.GetCursor(AX, AY: Integer): TCursor;
begin
  // Using FindControl is inefficient.
  // All we really want to know is if Selected[0] contains (AX, AY)
  if (Count > 0) and (FindControl(AX, AY) = Selected[0]) then
    Result := Selector.GetCursor(AX, AY)
  else
    Result := crDefault;
end;

function TplDesignSurface.GetHitHandle(AX, AY: Integer): TplDesignHandleId;
begin
  Result := Selector.GetHitHandle(AX, AY);
end;

procedure TplDesignSurface.BeginUpdate;
begin
  Active := False;
  FUpdateOwner := Owner;
  Owner.RemoveComponent(Self);
end;

procedure TplDesignSurface.EndUpdate;
begin
  FUpdateOwner.InsertComponent(Self);
  Active := True;
end;

procedure TplDesignSurface.ReaderError(Reader: TReader; const Msg: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

function TplDesignSurface.Clear: TplDesignSurface;
begin
  BeginUpdate;
  try
    Container.DestroyComponents;
  finally
    EndUpdate;
  end;
  Result := Self;
end;

procedure TplDesignSurface.SaveToStream(AStream: TStream);
begin
  BeginUpdate;
  try
    DesignSaveComponentToStream(Container, AStream);
  finally
    EndUpdate;
  end;
end;

function TplDesignSurface.LoadFromStream(AStream: TStream): TplDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromStream(Container, AStream, @ReaderError);
    Container.Name := SavedName;
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;

procedure TplDesignSurface.SaveToFile(const AFileName: string);
begin
  BeginUpdate;
  try
    DesignSaveComponentToFile(Container, AFileName);
  finally
    EndUpdate;
  end;
end;

function TplDesignSurface.LoadFromFile(const AFileName: string): TplDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromFile(Container, AFileName, @ReaderError);
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;



//=== { TplDesignScrollBox } =================================================

procedure TplDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
  //
end;

//=== { TplDesignPanel } =====================================================

constructor TplDesignPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawRules := True;
  FSurface := TplDesignSurface.Create(Self);
  Surface.Name := 'Surface';
  Surface.Container := Self;
end;

procedure TplDesignPanel.SetDrawRules(const Value: Boolean);
begin
  FDrawRules := Value;
  Invalidate;
end;

procedure TplDesignPanel.Paint;
begin
  inherited Paint;
  if Surface.Active or (csDesigning in ComponentState) then
  begin
    if DrawRules then
      DesignPaintRules(Canvas, ClientRect);
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  end;
end;

procedure TplDesignPanel.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from Clear.
  FSurface := Surface.Clear;
end;

procedure TplDesignPanel.SaveToStream(AStream: TStream);
begin
  Surface.SaveToStream(AStream);
end;

procedure TplDesignPanel.LoadFromStream(AStream: TStream);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromStream.
  FSurface := Surface.LoadFromStream(AStream);
end;

procedure TplDesignPanel.SaveToFile(const AFileName: string);
begin
  Surface.SaveToFile(AFileName);
end;

procedure TplDesignPanel.LoadFromFile(const AFileName: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  FSurface := Surface.LoadFromFile(AFileName);
end;

function TplDesignPanel.GetActive: Boolean;
begin
  Result := Surface.Active;
end;

function TplDesignPanel.GetOnChange: TNotifyEvent;
begin
  Result := Surface.OnChange;
end;

function TplDesignPanel.GetOnGetAddClass: TplDesignGetAddClassEvent;
begin
  Result := Surface.OnGetAddClass;
end;

function TplDesignPanel.GetOnSelectionChange: TNotifyEvent;
begin
  Result := Surface.OnSelectionChange;
end;

procedure TplDesignPanel.SetActive(const Value: Boolean);
begin
  Surface.Active := Value;
end;

procedure TplDesignPanel.SetOnChange(const Value: TNotifyEvent);
begin
  Surface.OnChange := Value;
end;

procedure TplDesignPanel.SetOnGetAddClass(const Value: TplDesignGetAddClassEvent);
begin
  Surface.OnGetAddClass := Value;
end;

procedure TplDesignPanel.SetOnSelectionChange(const Value: TNotifyEvent);
begin
  Surface.OnSelectionChange := Value;
end;


end.

