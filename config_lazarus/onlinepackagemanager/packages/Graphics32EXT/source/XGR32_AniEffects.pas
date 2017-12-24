
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_AniEffects;

interface

uses
     LCLIntf, LCLType, LMessages,
     Messages, Classes, Graphics , SysUtils , Controls,
     GR32 ;

type
  TGRCustomAnimationEffect = class;
  { Summary the abstract Effefts egnine }
  { Description
  you should override this class to use.

  the DoWMPaint is no use!!
  so you have to do the paint by youself.
  and the Timer is your work by yourself.
  }
  TGRCustomAnimationEffects = class(TList)
  private
    procedure SetControl(const Value: TControl);
    procedure SetEnabled(Value: Boolean);
  protected
    FControl: TControl;
    FDrawing: Boolean;
    FEnabled: Boolean;
    FFPS: LongWord;
    FFPSCurrent: LongWord;
    FFPSOldTime: LongWord;
    FFPSTime: LongWord;
    FLastTickCount: LongWord;
    FOldResizeProc: TNotifyEvent;
    FOldWndProc: TWndMethod;
    FTimerId: Integer;
    FUpdating: Boolean;
    FWinStyle: LongInt;
    FOnUpdate: TNotifyEvent;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure DoControlPaint(Sender: TControl; DC: HDC); overload; virtual;
    procedure DoControlPaint(Sender: TBitmap32); overload;virtual;
    procedure DoControlResize(Sender: TObject); virtual;
    procedure DoControlWndProc(var Message: TLMessage); virtual;
    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton);
    procedure DoMouseMove(var Message: TLMMouseMove);
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure DoTimer(Sender: TObject);
    { Summary not used }
    procedure DoWMPaint(var Message: TLMPaint); virtual;
    { Summary true means begin animation }
    procedure HookControl(Value: TControl; Hooked: Boolean); virtual;
    procedure InternalDoTimer; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { Summary Adds a new item. }
    { Description
    If the number of items exceeds the maximum, frees the Item instead.
    }
    procedure Add(var aItem: TGRCustomAnimationEffect);
    property Control: TControl read FControl write SetControl;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  { Summary the Custom Background Effect }
  { Description
  you must override DoPaint method.
  the DoResize is optional.
  }
  TGRCustomAnimationEffect = class(TObject)
  private
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    FEnabled: Boolean;
    FHeight: Integer;
    FLeft: Integer;
    FOwner: TGRCustomAnimationEffects;
    FTop: Integer;
    FWidth: Integer;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoPaint(Sender: TControl; DC: HDC); overload;virtual; abstract;
    procedure DoPaint(Sender: TBitmap32); overload;virtual; abstract;
    { Summary the Owner.FControl size changed. }
    procedure DoResize(Sender: TControl); virtual;
    procedure DoTimer(MoveCount: TFloat); virtual;
    { Summary the width or height changed. }
    procedure SizeChanged(Sender: TControl); virtual;
  public
    constructor Create(aOwner: TGRCustomAnimationEffects); virtual;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Height: Integer read FHeight write SetHeight;
    property Left: Integer read FLeft write SetLeft;
    property Owner: TGRCustomAnimationEffects read FOwner;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
  end;


const
  cMaxWaterSize = 256;
  cMinIntervalValue= 30;
  cMaxAnimationEffects = 500;

implementation

uses
  Forms;

type
  TControlAccess = Class(TControl);

constructor TGRCustomAnimationEffects.Create;
begin
  inherited;
  //Application.OnIdle := AppIdle;
end;

destructor TGRCustomAnimationEffects.Destroy;
begin
  SetEnabled(False);
  inherited Destroy;
end;

procedure TGRCustomAnimationEffects.Add(var aItem: TGRCustomAnimationEffect);
begin
  if Count = cMaxAnimationEffects then begin
    aItem.Free;
    aItem := nil;
    exit;
  end;
  inherited Add(aItem);
  if aItem.Owner <> Self then aItem.FOwner := Self;
end;

procedure TGRCustomAnimationEffects.AppIdle(Sender: TObject; var Done: Boolean);
begin
  if FEnabled then
    DoTimer(Sender);
end;

procedure TGRCustomAnimationEffects.DoControlPaint(Sender: TBitmap32);
var
  LItem: TGRCustomAnimationEffect;
  i: Integer;
  s: string;
begin

  if not FDrawing then
  try
    FDrawing := True;


    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoPaint(Sender);
    end;


  finally
    //aCanvas.UnLock;
    FDrawing := False;
  end;
end;


procedure TGRCustomAnimationEffects.DoControlPaint(Sender: TControl; DC: HDC);
var
  LItem: TGRCustomAnimationEffect;
  i: Integer;
  s: string;
begin

  if not FDrawing then
  try
    FDrawing := True;
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoPaint(Sender, DC);
    end;
  finally
    //aCanvas.UnLock;
    FDrawing := False;
  end;
end;

procedure TGRCustomAnimationEffects.DoControlResize(Sender: TObject);
var
  LItem: TGRCustomAnimationEffect;
  i: Integer;
begin
  if Assigned(FOldResizeProc) then FOldResizeProc(Sender);
  For i := 0 to Count -1 Do
  begin
    LItem := TGRCustomAnimationEffect(Items[i]);
    if LItem.Enabled then
      LItem.DoResize(TControl(Sender));
  end;
end;

procedure TGRCustomAnimationEffects.DoControlWndProc(var Message: TLMessage);
begin
  if Assigned(FOldWndProc) then FOldWndProc(Message);
  case Message.Msg of
    LM_Paint: DoWMPaint(TLMPaint(Message));
    LM_MouseMove: DoMouseMove(TLMMouseMove(Message));
    LM_LButtonDown: DoMouseDown(TLMMouse(Message), mbLeft);
    LM_LBUTTONUP: DoMouseUp(TLMMouse(Message), mbLeft);
    LM_RButtonDown: DoMouseDown(TLMMouse(Message), mbRight);
    LM_RBUTTONUP: DoMouseUp(TLMMouse(Message), mbRight);
    LM_MBUTTONUP: DoMouseUp(TLMMouse(Message), mbMiddle);
    LM_MButtonDown: DoMouseDown(TLMMouse(Message), mbMiddle);
  end;
end;

procedure TGRCustomAnimationEffects.DoMouseDown(var Message: TLMMouse; Button: TMouseButton);
var
  I: Integer;
  LItem: TGRCustomAnimationEffect;
  Shift: TShiftState;
  LPos: TPoint;
begin
  if Assigned(FControl) then
  with Message do
  begin
    Shift := KeysToShiftState(Keys);

    with TControlAccess(FControl) do
    begin
      if (Width > 32768) or (Height > 32768) then
       begin
       // LPos := CalcCursorPos  // ct9999
       end else
       begin
        LPos.X := XPos;
        LPOs.Y := YPos;
      end;
    end;

    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoMouseDown(Button, Shift, LPos.X, LPos.Y);
    end;
  end;
end;

procedure TGRCustomAnimationEffects.DoMouseMove(var Message: TLMMouseMove);
var
  I: Integer;
  LItem: TGRCustomAnimationEffect;
  Shift: TShiftState;
  LPos: TPoint;
begin
  if Assigned(FControl) then
  with Message do
  begin
    Shift := KeysToShiftState(Keys);
    with TControlAccess(FControl) do
    begin
      if (Width > 32768) or (Height > 32768) then
       begin
       // LPos := CalcCursorPos  // ct9999
       end else
       begin
        LPos.X := XPos;
        LPOs.Y := YPos;
      end;
    end;

    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoMouseMove(Shift, LPos.X, LPos.Y);
    end;
  end;
end;

procedure TGRCustomAnimationEffects.DoMouseUp(var Message: TLMMouse; Button:
  TMouseButton);
var
  I: Integer;
  LItem: TGRCustomAnimationEffect;
  Shift: TShiftState;
begin
  //if Assigned(FOldMouseUpProc) then FOldMouseUpProc(Sender, Button, Shift, X, Y);
  with Message do
  begin
    Shift := KeysToShiftState(Keys);

    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoMouseUp(Button, Shift, XPos, YPos);
    end;
  end;
end;

procedure TGRCustomAnimationEffects.DoTimer(Sender: TObject);
begin
  if not FUpdating then
  try
    FUpdating := True;
    InternalDoTimer;
    if Assigned(FOnUpdate) then FOnUpdate(Self);
  finally
    FUpdating := False;
  end;
end;

procedure TGRCustomAnimationEffects.DoWMPaint(var Message: TLMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  (*
  if Assigned(FControl)  then
  begin
    DC := Message.DC;
    {$ifdef Debug}
    SendDebug('WMPaint:'+IntToStr(DC));
    {$endif}
    if (DC <> 0) then
    begin
      //DoControlPaint(FControl, DC);

    end
    else if Control is TWinControl then
    begin

      {DC := BeginPaint(TWinControl(FControl).Handle, PS);
      try
        DoControlPaint(FControl, DC);
      finally
        EndPaint(TWinControl(FControl).Handle, PS);
      end;''}
    end;
  end;

  //*)
end;

procedure TGRCustomAnimationEffects.HookControl(Value: TControl; Hooked:
  Boolean);
begin
  if Hooked then
  begin
    FControl := Value;
    FOldWndProc := Value.WindowProc;
    Value.WindowProc := @DoControlWndProc;
    FOldResizeProc := TControlAccess(Value).OnResize;
    TControlAccess(Value).OnResize := @DoControlResize;
    DoControlResize(Value);
    FLastTickCount := GetTickCount;

  end
  else begin //Unhook
    Value.WindowProc := FOldWndProc;
    FOldWndProc := nil;
    TControlAccess(Value).OnResize := FOldResizeProc;


    FOldResizeProc := nil;
    FControl := nil;
  end;
end;

procedure TGRCustomAnimationEffects.InternalDoTimer;
var
  Tick: TFloat;
  I: Integer;
  LItem: TGRCustomAnimationEffect;
begin
    //FControl.Repaint;

    Tick := (GetTickCount - FLastTickCount) * 0.1;
    FLastTickCount := GetTickCount;

    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoTimer(Tick);
    end;

    //DoPaint;
end;

procedure TGRCustomAnimationEffects.Notify(Ptr: Pointer; Action:
  TListNotification);
begin
  if (Action = lnDeleted) and (TObject(Ptr) is TGRCustomAnimationEffect) then
  begin
    TGRCustomAnimationEffect(Ptr).FOwner := nil;
    TGRCustomAnimationEffect(Ptr).Free;
    //Ptr := nil;
  end;
end;

procedure TGRCustomAnimationEffects.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    Enabled := False;
    FControl := Value;
  end;
end;

procedure TGRCustomAnimationEffects.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FControl) then
      HookControl(FControl, FEnabled);
  end;
end;

constructor TGRCustomAnimationEffect.Create(aOwner: TGRCustomAnimationEffects);
begin
  inherited Create;
  FEnabled := True;
  FOwner := aOwner;
  if Assigned(FOwner) then
  begin
    FOwner.Add(Self);
  end;
end;

destructor TGRCustomAnimationEffect.Destroy;
begin
  if Assigned(FOwner) then FOwner.Remove(Self);
  inherited;
end;

procedure TGRCustomAnimationEffect.DoMouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
end;

procedure TGRCustomAnimationEffect.DoMouseMove(Shift: TShiftState; X, Y:
  Integer);
begin
end;

procedure TGRCustomAnimationEffect.DoMouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
end;

procedure TGRCustomAnimationEffect.DoResize(Sender: TControl);
begin
  if (FHeight <= 0) or (FHeight > Sender.ClientHeight - FTop) then
    FHeight := Sender.ClientHeight - FTop;

  if (FWidth <= 0) or (FWidth > Sender.ClientWidth - FLeft) then
    FWidth := Sender.ClientWidth - FLeft;
  SizeChanged(Sender);
end;

procedure TGRCustomAnimationEffect.DoTimer(MoveCount: TFloat);
begin
end;

procedure TGRCustomAnimationEffect.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
  if Value > Owner.FControl.ClientHeight - FTop then
    Value := Owner.FControl.ClientHeight - FTop;
  FHeight := Value;
  SizeChanged(Owner.FControl);
  end;
end;

procedure TGRCustomAnimationEffect.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
  begin
  FLeft := Value;
  end;
end;

procedure TGRCustomAnimationEffect.SetTop(Value: Integer);
begin
  if FTop <> Value then
  begin
  FTop := Value;
  end;
end;

procedure TGRCustomAnimationEffect.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
  if Value > Owner.FControl.ClientWidth - FLeft then
    Value := Owner.FControl.ClientWidth - FLeft;
  FWidth := Value;
  SizeChanged(Owner.FControl);
  end;
end;

procedure TGRCustomAnimationEffect.SizeChanged(Sender: TControl);
begin
end;

end.

