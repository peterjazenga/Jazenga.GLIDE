{ SdpoDSM v0.1.1

  CopyRight (C) 2006-2009 Paulo Costa

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at paulo.malheiros@fe.up.pt
}
unit SdpoDSM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, Math, Types;
(*{$ifdef WIN32}
  Classes, SysUtils, LResources, Controls, Graphics, Math, Types;
{$endif}
{$ifdef LINUX}
  Classes, SysUtils, LResources, Controls, Graphics, Math, Types;
{$endif}*)


type
  TDSMOrientation = (orUp, orDown);
  TDSMLinkType = (ltStateToTransition, ltTransitionToState);
//  TDSMItemStatusValue = (isOk, isHanging, isOrfan, isUnknown);
  TDSMItemStatusValue = (itHasParent, itHasChild);
  TDSMItemStatus = set of TDSMItemStatusValue;

  type TDSMFireTestEvent = procedure (Sender: TObject; var IsActive: boolean) of object;

  TMachineData=record
    index: integer;
    OutputIndex, OutputCount: integer;
    ConnectionStatus: TDSMItemStatus;
  end;

  TDSMStateMachine = class;
  TDSMConnection = class;

  TDSMState = class (TGraphicControl)
  private
    fStateMachine: TDSMStateMachine;

    fStateName: string;
    fText: string;
    fActiveColor: TColor;
    fInactiveColor: TColor;
//    fStateIsActive: boolean;
    fRectangleHeight: integer;

    fStateActive: TNotifyEvent;
    fStateActivation: TNotifyEvent;
    fStateDeactivation: TNotifyEvent;

    fNameChanged: boolean;

    procedure SetStateName(Value: String);
    procedure SetMyText(Value: String);
    procedure SetActiveColor(Value: TColor);
    procedure SetInactiveColor(Value: TColor);
//    procedure SetStateState(Value: boolean);
    procedure SetRectangleHeight(Value: integer);
    procedure RepositionConnections;


  protected
    MachineData: TMachineData;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure Loaded; override;
//    procedure AfterConstruction; override;

    procedure StateActivationActions;
    procedure StateDeactivationActions;
    procedure StateActiveActions;
  public
    EnterPoint, ExitPoint: TPoint;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure SetBounds (ALeft, ATop, AWidth, AHeight: Integer); override;    ONRESIZE
  published
    property Width default 50;
    property Height default 70;
//    property StateIsActive: boolean
//      read fStateIsActive write SetStateState default false;
    property RectangleHeight: integer
      read fRectangleHeight write SetRectangleHeight default 30;


    property StateMachine: TDSMStateMachine
      read fStateMachine write fStateMachine;
    property StateName: String
      read fStateName write SetStateName;
    property Text: String
      read fText write SetMyText;
    property ActiveColor: TColor
      read fActiveColor write SetActiveColor default clwhite;
    property InactiveColor: TColor
      read fInactiveColor write SetInactiveColor default clBtnFace;


    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;

    property OnStateActive: TNotifyEvent
      read fStateActive write fStateActive;
    property OnStateActivation: TNotifyEvent
      read fStateActivation write fStateActivation;
    property OnStateDeactivation: TNotifyEvent
      read fStateDeactivation write fStateDeactivation;
  end;


  TDSMTransition = class(TGraphicControl)
  private
    fStateMachine: TDSMStateMachine;
    fOrientation: TDSMOrientation;
    fPriority: integer;

    fTransitionName: string;
    fText: string;
    fFireTest: TDSMFireTestEvent;
    fFiring: TNotifyEvent;

    procedure SetOrientation(Value: TDSMOrientation);
    procedure SetTransitionName(Value: String);
    procedure SetMyText(Value: String);
    procedure RepositionConnections;

  protected
    MachineData: TMachineData;
    EnterPoint, ExitPoint: TPoint;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure FireTest(var IsActive: boolean);
    function GetConnectionStatus: TDSMItemStatus;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ConnectionStatus: TDSMItemStatus
      read GetConnectionStatus;
  published
    property Width default 50;
    property Height default 50;
    property Orientation: TDSMOrientation
      read fOrientation write SetOrientation default orDown;

    property StateMachine: TDSMStateMachine
      read fStateMachine write fStateMachine;
    property TransitionName: String
      read fTransitionName write SetTransitionName;
    property Text: String
      read fText write SetMyText;

    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;

    property OnFireTest: TDSMFireTestEvent
      read fFireTest write fFireTest;
    property OnFiring: TNotifyEvent
      read fFiring write fFiring;
    property Priority: integer
      read fPriority write fPriority;
  end;


  TDSMConnection = class(TGraphicControl)
  private
    fStateLink: TDSMState;
    fTransitionLink: TDSMTransition;
    fLinkType: TDSMLinkType;
    Repositioning: boolean;
  protected

    procedure SetStateLink(Value: TDSMState);
    procedure SetTransitionLink(Value: TDSMTransition);
    procedure SetLinkType(Value: TDSMLinkType);

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure Reposition;
  public
    StartPoint, EndPoint: TPoint;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Width default 50;
    property Height default 30;

    property StateLink: TDSMState
      read fStateLink write SetStateLink;
    property TransitionLink: TDSMTransition
      read fTransitionLink write SetTransitionLink;
    property LinkType: TDSMLinkType
      read fLinkType write SetLinkType;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  TDSMStateMachine = class(TComponent)
  private
    fStates: TList;
    fTransitions: Tlist;
    fStatesOutputs, fTransOutputs: Tlist;
//    fHangingTrans, fOrfanTrans: Tlist;

    fActiveState: TDSMState;
    fPreviousState, fNextState: TDSMState;
    fActiveTransition: TDSMTransition;

    fActive: boolean;
    fVisualFeedback: boolean;
    procedure BuildStateOutputsList;
    procedure BuildStatesList;
    procedure BuildtTansitionsList;
    procedure BuildTransitionOutputsList;
  protected
    function GetStates(idx: integer): TDSMState;
    function GetStatesCount: integer;
    function GetTransitions(idx: integer): TDSMTransition;
    function GetTransitionsCount: integer;
    procedure SetActiveState(Value: TDSMState);

    procedure SetActive(Value: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetStateOutput(idx,ith: integer): TDSMConnection;
    function GetStateOutputsCount(idx: integer): integer;
    function GetTransitionOutput(idx,ith: integer): TDSMConnection;
    function GetTransitionOutputsCount(idx: integer): integer;

    function GetConnectdState(idx,ith: integer): TDSMState;
    function GetConnectedTransition(idx, ith: integer): TDSMTransition;

    procedure EvalNextState;
    procedure CommitNextState;
    procedure StepIt;

    property States[idx: integer]: TDSMState
      read GetStates;
    property StatesCount: integer
      read GetStatesCount;
    property Transitions[idx: integer]: TDSMTransition
      read GetTransitions;
    property TransitionsCount: integer
      read GetTransitionsCount;

    property PreviousState: TDSMState  // Last Active State
      read fPreviousState;
    property NextState: TDSMState      // Next Active State if EvalNextState was called
      read fNextState;                 // else equals ActiveState
    property ActiveTransition: TDSMTransition  // if EvalNextState was called gives the next Firing Transition
      read fActiveTransition;                  // else it is equal to last fired transition
  published
    property Active: boolean
      read fActive write SetActive;
    property ActiveState: TDSMState
      read fActiveState write SetActiveState;
    property VisualFeedback: boolean
      read fVisualFeedback write fVisualFeedback;
  end;


procedure Register;

implementation

constructor TDSMState.Create(AOwner: TComponent);
begin
  // call the parent constructor
  inherited Create(AOwner);

  // set the default values
  Width := 50;
  Height := 70;
//  fStateIsActive:= false;
  fActiveColor := clwhite;
  fInactiveColor := clBtnFace;
  fRectangleHeight:=30;

  StateName:='State';
end;

destructor TDSMState.Destroy;
begin
  // call the parent destructor
  inherited Destroy;
end;


procedure TDSMState.SetStateName(Value: String);
begin
  if fStateName <> Value then begin
    fStateName := Value;
    fNameChanged:=true;
    Invalidate;
  end;
end;

procedure TDSMState.SetMyText(Value: String);
begin
  if fText <> Value then begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TDSMState.SetActiveColor(Value: TColor);
begin
  if fActiveColor <> Value then begin
    fActiveColor := Value;
    Invalidate;
  end;
end;

procedure TDSMState.SetInactiveColor(Value: TColor);
begin
  if fInactiveColor <> Value then begin
    fInactiveColor := Value;
    Invalidate;
  end;
end;

//procedure TDSMState.SetStateState(Value: boolean);
//begin
//  if fStateIsActive <> Value then begin
//    fStateIsActive := Value;
//    Invalidate;
//  end;
//end;

procedure TDSMState.SetRectangleHeight(Value: integer);
begin
  if fRectangleHeight <> Value then begin
    fRectangleHeight := Value;
    Invalidate;
  end;
end;


//procedure TDSMState.RepaintRequest (Sender: TObject);
//begin
//  Invalidate;
//end;

procedure TDSMState.Paint;
var
  XCenter, RectHeight, YRecTop, YRecBottom: Integer;
  arrowsize: integer;
  ts: TSize;
begin
  RectHeight:=min(fRectangleHeight,height-10);
  XCenter := (Width - 1) div 2;

  YRecTop:=(height-RectHeight) div 2;
  YRecBottom:=YRecTop + RectHeight;

  arrowsize:=4;
  // Draw the box
//  if fStateIsActive then
//  if (fStateMachine<>nil) and (fStateMachine.ActiveState=self) then
  if (fStateMachine<>nil) and (fStateMachine.fActiveState=self) then
    Canvas.Brush.Color := fActiveColor
  else
    Canvas.Brush.Color := fInactiveColor;
  with canvas do begin
    Rectangle(0,YRecTop,self.Width,YRecBottom);
    MoveTo(XCenter,0);
    Lineto(XCenter,YRecTop);
    MoveTo(XCenter,YRecBottom);
    Lineto(XCenter,height);
    TextOut(2,2+YRecTop,fStateName);
    if fText<>'' then begin
      ts:=TextExtent(fText);
      TextOut(width-(ts.cx+2),YRecBottom-(ts.cy+2),fText);
    end;

    Canvas.Brush.Color := clblack;
    Polygon([Point(XCenter,YRecTop), Point(XCenter-arrowsize,YRecTop-arrowsize),
             Point(XCenter+arrowsize,YRecTop-arrowsize)]);
  end;
end;


procedure TDSMState.StateActiveActions;
begin
  // call the handler, if available
  if Assigned(fStateActive) then
    fStateActive(Self);
end;


procedure TDSMState.Notification(AComponent: TComponent;  Operation: TOperation);
begin
  inherited;
  if (AComponent=fStateMachine) and (Operation=opRemove) then fStateMachine:=nil;
end;


procedure TDSMState.Resize;
begin
  inherited;
  RepositionConnections;
end;

procedure TDSMState.Loaded;
begin
  inherited;
  RepositionConnections;
end;

//procedure TDSMState.AfterConstruction;
//begin
//  inherited;
//  if not fNameChanged then StateName:='After';
//  if not fNameChanged then StateName:=copy(name,4,length(name));
//end;


procedure TDSMState.RepositionConnections;
var i,XCenter: integer;
    Con: TDSMConnection;
begin
  tag:=tag+1;
  XCenter := (Width - 1) div 2;
  EnterPoint.x:=XCenter+Left;
  EnterPoint.y:=top;
  ExitPoint.x:=XCenter+Left;
  ExitPoint.y:=top+height;

  if parent=nil then exit;
  for i:=0 to parent.ComponentCount-1 do begin
    if parent.Components[i]=nil then continue;
    if parent.Components[i] is TDSMConnection then begin
       Con:=(parent.Components[i] as TDSMConnection);
       if Con.fStateLink=self then begin
         Con.Reposition;
       end;
    end;
  end;
end;



procedure TDSMState.StateActivationActions;
begin
  if Assigned(fStateActivation) then
    fStateActivation(Self);
end;


procedure TDSMState.StateDeactivationActions;
begin
  if Assigned (fStateDeactivation) then
    fStateDeactivation(Self);
end;


//-------------------------------------------------------------------------------------------------------------
//  TDSMTransition = class (TGraphicControl)



constructor TDSMTransition.Create(AOwner: TComponent);
begin
  // call the parent constructor
  inherited Create(AOwner);

  // set the default values
  fOrientation := orDown;
  Width := 50;
  Height := 50;

end;

destructor TDSMTransition.Destroy;
begin
  // call the parent destructor
  inherited Destroy;
end;

procedure TDSMTransition.SetOrientation (Value: TDSMOrientation);
begin
  if fOrientation <> Value then begin
    fOrientation := Value;
    RepositionConnections;
    Invalidate;
  end;
end;

procedure TDSMTransition.SetTransitionName(Value: String);
begin
  if fTransitionName <> Value then begin
    fTransitionName := Value;
    Invalidate;
  end;
end;

procedure TDSMTransition.SetMyText(Value: String);
begin
  if fText <> Value then begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TDSMTransition.Notification(AComponent: TComponent;  Operation: TOperation);
begin
  inherited;
  if (AComponent=fStateMachine) and (Operation=opRemove) then fStateMachine:=nil;
end;


procedure TDSMTransition.Paint;
var
  XCenter, RectHeight, RectWidth, YRecTop, YRecBottom: Integer;
  ts: TSize;
  arrowsize: integer;
  txt1,txt2: string;
//  tmpColor: Tcolor;
begin
  RectHeight:=min(5,height-10);
  RectWidth:=min(17,width);
  // compute the center
//  YCenter := (Height - 1) div 2;
  XCenter := (RectWidth - 1) div 2;

  YRecTop:=(height-RectHeight) div 2;
  YRecBottom:=YRecTop + RectHeight;

  arrowsize:=4;

  case fOrientation of
    orUp: begin
      txt1:=fText;
      txt2:=fTransitionName;
    end;
    orDown: begin
      txt2:=fText;
      txt1:=fTransitionName;
    end;
  end;

  with canvas do begin
    Canvas.Brush:=Parent.Brush;

    //ts:=TextExtent(fText);
    ts:=TextExtent(fText+'W');
    TextOut(XCenter+2+arrowsize,YRecTop-(ts.cy+RectHeight div 2),txt1);
    TextOut(XCenter+2+arrowsize,YRecTop+(RectHeight div 2)+2,txt2);

    Rectangle(0,YRecTop,RectWidth,YRecBottom);

    Canvas.Brush.Color:=ClBlack;

    MoveTo(XCenter,0);
    Lineto(XCenter,YRecTop);
//    Polygon([Point(XCenter,YRecTop), Point(XCenter-arrowsize,YRecTop-arrowsize),
//             Point(XCenter+arrowsize,YRecTop-arrowsize)]);
    MoveTo(XCenter,YRecBottom);
    Lineto(XCenter,height);

//    TextOut(width-(ts.cx+2),YRecBottom-(ts.cy+2),fText);

//    tmpColor:=Canvas.Brush.Color;
//    Canvas.Brush.Color := clBlack;
//    FillRect(rect(0,YRecTop,RectWidth,YRecBottom));
//    Canvas.Brush.Color := tmpColor;

  // draw the arrow line
    case fOrientation of
      orUp: begin
        Polygon([Point(XCenter,YRecBottom), Point(XCenter-arrowsize,YRecBottom+arrowsize),
                 Point(XCenter+arrowsize,YRecBottom+arrowsize)]);
      end;
      orDown: begin
        Polygon([Point(XCenter,YRecTop), Point(XCenter-arrowsize,YRecTop-arrowsize),
                 Point(XCenter+arrowsize,YRecTop-arrowsize)]);
      end;
    end;
  end;

end;


procedure TDSMTransition.Resize;
begin
  inherited;
  RepositionConnections;
end;

procedure TDSMTransition.Loaded;
begin
  inherited;
  RepositionConnections;
end;


procedure TDSMTransition.RepositionConnections;
var i,XCenter: integer;
    Con: TDSMConnection;
begin
  //RectWidth:=min(17,width);  // Magic number
  XCenter := (min(17,width) - 1) div 2;

  EnterPoint.x:=XCenter+Left;
  EnterPoint.y:=top;
  ExitPoint.x:=XCenter+Left;
  ExitPoint.y:=top+height;

  if parent=nil then exit;
  for i:=0 to parent.ComponentCount-1 do begin
    if parent.Components[i]=nil then continue;
    if parent.Components[i] is TDSMConnection then begin
       Con:=(parent.Components[i] as TDSMConnection);
       if Con.fTransitionLink=self then begin
         Con.Reposition;
       end;
    end;
  end;
end;


procedure TDSMTransition.FireTest(var IsActive: boolean);
begin
  // call the handler, if available
  if Assigned(fFireTest) then
    fFireTest(Self, IsActive);
end;

function TDSMTransition.GetConnectionStatus: TDSMItemStatus;
begin
  result:=MachineData.ConnectionStatus;
end;



//-------------------------------------------------------------------------------------------------------------
//  TDSMConnection = class (TGraphicControl)



constructor TDSMConnection.Create(AOwner: TComponent);
begin
  // call the parent constructor
  inherited Create(AOwner);

  // set the default values
  Width := 50;
  Height := 30;

  Repositioning:=false;
end;

destructor TDSMConnection.Destroy;
begin
  // call the parent destructor
  inherited Destroy;
end;

procedure TDSMConnection.Notification(AComponent: TComponent;  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then begin
//    if AComponent=fStateMachine then fStateMachine:=nil;
    if AComponent=fStateLink then fStateLink:=nil;
    if AComponent=fTransitionLink then fTransitionLink:=nil;
  end;
end;


procedure TDSMConnection.SetStateLink(Value: TDSMState);
begin
  if fStateLink <> Value then begin
    fStateLink := Value;
    Reposition;
  end;
end;

procedure TDSMConnection.SetTransitionLink(Value: TDSMTransition);
begin
  if fTransitionLink <> Value then begin
    fTransitionLink := Value;
    Reposition;
  end;
end;


procedure TDSMConnection.SetLinkType(Value: TDSMLinkType);
begin
  if fLinkType <> Value then begin
    fLinkType := Value;
    Reposition;
  end;
end;


procedure TDSMConnection.Paint;
begin
  with canvas do begin
    Canvas.Brush:=Parent.Brush;

    if (fStateLink<>nil) and (fTransitionLink<>nil) and
       (fStateLink.fStateMachine=fTransitionLink.fStateMachine) then pen.color:=clblack
    else pen.color:=clred;

    MoveTo(StartPoint.x-left,StartPoint.y-top);
    Lineto(EndPoint.x-left,EndPoint.y-1-top);
//    MoveTo(0,2);
//    Lineto(width-1,height-3);
  end;
end;


// Recalculates the anchors: StatrPoint and Endpoint so that the line starts and ends on the State
// and Transitions' conection points. It also readjusts the bounds

procedure TDSMConnection.Reposition;
begin
  Repositioning:=true;
  if fStateLink<>nil then begin
    if fLinkType=ltStateToTransition then begin
      StartPoint:=fStateLink.ExitPoint;
    end else begin
      StartPoint:=fStateLink.EnterPoint;
    end;
  end else begin
    StartPoint.x:=left;
    StartPoint.y:=top+2;
  end;

  if fTransitionLink<>nil then begin
    if (fLinkType=ltStateToTransition) xor (fTransitionLink.Orientation=orUp) then begin
      EndPoint:=fTransitionLink.EnterPoint;
    end else begin
      EndPoint:=fTransitionLink.ExitPoint;
    end;
  end else begin
    EndPoint.x:=left+width;
    EndPoint.y:=top++2+height;
  end;

  setbounds(min(StartPoint.x,EndPoint.x),min(StartPoint.y,EndPoint.y)-2,
            abs(StartPoint.x-EndPoint.x)+1,abs(StartPoint.y-EndPoint.y)+5);
  SendToBack;
  Invalidate;
  Repositioning:=false;
end;


procedure TDSMConnection.Resize;
begin
  inherited;
  if not Repositioning then Reposition;
end;


procedure TDSMConnection.Loaded;
begin
  inherited;
  if not Repositioning then Reposition;
end;



//---------------------------------------------------------------------------------------------------------------
// TDSMStateMachine

constructor TDSMStateMachine.Create(AOwner: TComponent);
begin
  inherited;
  fStates:=TList.Create;
  fTransitions:=TList.Create;
//      fStatesOutputs, fTransitionsOutputs: Tlist;
  fStatesOutputs:=TList.Create;
  fTransOutputs:=TList.Create;
end;

destructor TDSMStateMachine.Destroy;
begin
  fTransOutputs.Free;
  fStatesOutputs.Free;
  fTransitions.Free;
  fStates.Free;
  inherited;
end;

{
function TDSMStateMachine.GetConnections(idx: integer): TDSMConnection;
begin
  result:=nil;
  if fConnections=nil then exit;
  result:=TDSMConnection(fConnections.items[idx]); //tlist
end;

function TDSMStateMachine.GetConnectionsCount: integer;
begin
  result:=0;
  if fConnections=nil then exit;
  result:=fConnections.Count;
end;
}

function TDSMStateMachine.GetStateOutput(idx,ith: integer): TDSMConnection;
begin
  result:=nil;
  if (fStates=nil) or (fStatesOutputs=nil) then exit;
  result:=TDSMConnection(fStatesOutputs.items[TDSMState(fStates.Items[idx]).MachineData.OutputIndex+ith]);
end;

function TDSMStateMachine.GetConnectedTransition(idx,ith: integer): TDSMTransition;
begin
  result:=nil;
  if (fStates=nil) or (fStatesOutputs=nil) then exit;
  result:=TDSMConnection(fStatesOutputs.items[TDSMState(fStates.Items[idx]).MachineData.OutputIndex+ith]).fTransitionLink;
end;


function TDSMStateMachine.GetStateOutputsCount(idx: integer): integer;
begin
  result:=0;
  if (fStates=nil) or (fStatesOutputs=nil) then exit;
  result:=TDSMState(fStates.Items[idx]).MachineData.OutputCount;
end;


function TDSMStateMachine.GetTransitionOutput(idx,ith: integer): TDSMConnection;
begin
  result:=nil;
  if (fTransitions=nil) or (fTransOutputs=nil) then exit;
  result:=TDSMConnection(fTransOutputs.items[TDSMTransition(fTransitions.Items[idx]).MachineData.OutputIndex+ith]);
end;

function TDSMStateMachine.GetTransitionOutputsCount(idx: integer): integer;
begin
  result:=0;
  if (fTransitions=nil) or (fTransOutputs=nil) then exit;
  result:=TDSMTransition(fTransitions.Items[idx]).MachineData.OutputCount;
end;

function TDSMStateMachine.GetConnectdState(idx,ith: integer): TDSMState;
begin
  result:=nil;
  if (fTransitions=nil) or (fTransOutputs=nil) then exit;
  result:=TDSMConnection(fTransOutputs.items[TDSMTransition(fTransitions.Items[idx]).MachineData.OutputIndex+ith]).fStateLink;
end;


function TDSMStateMachine.GetStates(idx: integer): TDSMState;
begin
  result:=nil;
  if fStates=nil then exit;
  result:=TDSMState(fStates.items[idx]); //tlist
end;

function TDSMStateMachine.GetStatesCount: integer;
begin
  result:=0;
  if fStates=nil then exit;
  result:=fStates.Count;
end;


function TDSMStateMachine.GetTransitions(idx: integer): TDSMTransition;
begin
  result:=nil;
  if fTransitions=nil then exit;
  result:=TDSMTransition(fTransitions.items[idx]); //tlist
end;

function TDSMStateMachine.GetTransitionsCount: integer;
begin
  result:=0;
  if fTransitions=nil then exit;
  result:=fTransitions.Count;
end;

{
function SOStateIndexCompare(Item1, Item2: Pointer): Integer;
begin
  result:=0;
  if TDSMState(Item1).MachineData.index > TDSMState(Item2).MachineData.index then result:=1
  else if TDSMState(Item1).MachineData.index < TDSMState(Item2).MachineData.index then result:=-1;
end;

function TOTransIndexCompare(Item1, Item2: Pointer): Integer;
begin
  result:=0;
  if TDSMTransition(Item1).MachineData.index > TDSMTransition(Item2).MachineData.index then result:=1
  else if TDSMTransition(Item1).MachineData.index < TDSMTransition(Item2).MachineData.index then result:=-1;
  if result=0 then begin
    if TDSMTransition(Item1).Priority > TDSMTransition(Item2).Priority then result:=1
    else if TDSMTransition(Item1).Priority < TDSMTransition(Item2).Priority then result:=-1;
  end;
end;
}

function SOStateIndexCompare(Item1, Item2: Pointer): Integer;
var State1, State2: TDSMState;
    Trans1, Trans2: TDSMTransition;
begin
  result:=0;
  State1:=TDSMConnection(Item1).StateLink;
  State2:=TDSMConnection(Item2).StateLink;
  if State1.MachineData.index > State2.MachineData.index then result:=1
  else if State1.MachineData.index < State2.MachineData.index then result:=-1;
  if result=0 then begin
    Trans1:=TDSMConnection(Item1).TransitionLink;
    Trans2:=TDSMConnection(Item2).TransitionLink;
    if (Trans1<>nil) and (Trans2<>nil) then begin
      if Trans1.Priority > Trans2.Priority then result:=-1
      else if Trans1.Priority < Trans2.Priority then result:=1;
    end;
  end;
end;


function TOTransIndexCompare(Item1, Item2: Pointer): Integer;
var Trans1, Trans2: TDSMTransition;
begin
  result:=0;
  Trans1:=TDSMConnection(Item1).TransitionLink;
  Trans2:=TDSMConnection(Item2).TransitionLink;
  if Trans1.MachineData.index > Trans2.MachineData.index then result:=1
  else if Trans1.MachineData.index < Trans2.MachineData.index then result:=-1;
end;


procedure TDSMStateMachine.SetActive(Value: boolean);
begin
  if fActive=Value then exit;

  if fStates<>nil then fStates.Clear;
  if fTransitions<>nil then fTransitions.Clear;
  if fStatesOutputs<>nil then fStatesOutputs.Clear;
  if fTransOutputs<>nil then fTransOutputs.Clear;
  if value then begin
    BuildStatesList;
    BuildtTansitionsList;
    BuildStateOutputsList;
    BuildTransitionOutputsList;
  end;
end;


procedure TDSMStateMachine.BuildStatesList;
var i: integer;
  Sta: TDSMState;
begin
  // Add states to list
  if (owner=nil) or (fStates=nil) then exit;
  for i:=0 to owner.ComponentCount-1 do begin
    if owner.Components[i]=nil then continue;
    if owner.Components[i] is TDSMState then begin
       Sta:=(owner.Components[i] as TDSMState);
       if Sta.StateMachine=self then begin
         fStates.Add(Sta);
         Sta.MachineData.index:=fStates.Count-1;
         Sta.MachineData.OutputCount:=0;
         Sta.MachineData.ConnectionStatus:=[];
       end;
    end;
  end;
end;


procedure TDSMStateMachine.BuildtTansitionsList;
var i: integer;
  Tra: TDSMTransition;
begin
  // Add transitions to list
  if (owner=nil) or (fTransitions=nil) then exit;
  for i:=0 to owner.ComponentCount-1 do begin
    if owner.Components[i]=nil then continue;
    if owner.Components[i] is TDSMTransition then begin
       Tra:=(owner.Components[i] as TDSMTransition);
       if Tra.StateMachine=self then begin
         fTransitions.Add(Tra);
         Tra.MachineData.index:=fTransitions.Count-1;
         Tra.MachineData.OutputCount:=0;
         Tra.MachineData.ConnectionStatus:=[];
       end;
    end;
  end;
end;


procedure TDSMStateMachine.BuildStateOutputsList;
var i,idx: integer;
  Con: TDSMConnection;
  Sta: TDSMState;
begin
  // Add Connections to list of State Outputs
  if (owner=nil) or (fStatesOutputs=nil) then exit;
  for i:=0 to owner.ComponentCount-1 do begin
    if owner.Components[i]=nil then continue;
    if owner.Components[i] is TDSMConnection then begin
       con:=(owner.Components[i] as TDSMConnection);
       if (con.fLinkType=ltStateToTransition) and
          (Con.fStateLink<>nil) and (Con.fStateLink.fStateMachine=self) and
          (Con.fTransitionLink<>nil) and (Con.fTransitionLink.fStateMachine=self) then begin
         fStatesOutputs.Add(con);
         inc(Con.fStateLink.MachineData.OutputCount);
//         if Con.fTransitionLink<>nil then
         Con.fTransitionLink.MachineData.ConnectionStatus := Con.fTransitionLink.MachineData.ConnectionStatus + [itHasParent];
       end;
    end;
  end;

  // sort State Outputs List by State index
  fStatesOutputs.Sort(@SOStateIndexCompare);

  // Fill State machinedata
  idx:=-1;
  for i:=0 to fStatesOutputs.Count-1 do begin
    Sta:=TDSMConnection(fStatesOutputs.Items[i]).StateLink;
    if Sta.MachineData.index<>idx then begin
      idx:=Sta.MachineData.index;
      Sta.MachineData.OutputIndex:=i;
    end;
  end;
end;


procedure TDSMStateMachine.BuildTransitionOutputsList;
var i,idx: integer;
  Con: TDSMConnection;
  Tra: TDSMTransition;
begin
  // Add Connections to list of Transitions Outputs
  if (owner=nil) or (fTransOutputs=nil) then exit;
  for i:=0 to owner.ComponentCount-1 do begin
    if owner.Components[i]=nil then continue;
    if owner.Components[i] is TDSMConnection then begin
       con:=(owner.Components[i] as TDSMConnection);
       if (con.fLinkType=ltTransitionToState) and
          (Con.fTransitionLink<>nil) and (Con.fTransitionLink.fStateMachine=self) and
          (Con.fStateLink<>nil) and (Con.fStateLink.fStateMachine=self) then begin
         fTransOutputs.Add(con);
         inc(Con.fTransitionLink.MachineData.OutputCount);
//         if Con.fStateLink<>nil then
         Con.fTransitionLink.MachineData.ConnectionStatus:=Con.fTransitionLink.MachineData.ConnectionStatus+[itHasChild];
       end;
    end;
  end;

  // sort Transitions Outputs List by State index first, then by priority
  fTransOutputs.Sort(@TOTransIndexCompare);

  // Fill Transition machinedata
  idx:=-1;
  for i:=0 to fTransOutputs.Count-1 do begin
    Tra:=TDSMConnection(fTransOutputs.Items[i]).TransitionLink;
    if Tra.MachineData.index<>idx then begin
      idx:=Tra.MachineData.index;
      Tra.MachineData.OutputIndex:=i;
    end;
  end;
end;


procedure TDSMStateMachine.SetActiveState(Value: TDSMState);
begin
  if fActiveState <> Value then begin
    if (fActiveState<>nil) and fVisualFeedback then fActiveState.Invalidate;
    fActiveState := Value;
    if (fActiveState<>nil) and fVisualFeedback then fActiveState.Invalidate;
  end;
end;


procedure TDSMStateMachine.EvalNextState;
var i,base: integer;
    Con: TDSMConnection;
    Tra: TDSMTransition;
    fire: boolean;
    actPriority: integer;
    actPriorityFrac,tmpFrac:double;
begin
  if fActiveState=nil then exit;
  actPriority:=0;
  actPriorityFrac:=0;
  Tra:=nil;
  base:=fActiveState.MachineData.Index;
  for i:=0 to fActiveState.MachineData.OutputCount-1 do begin
    Con:=GetStateOutput(base,i);
    if Con.fTransitionLink=nil then continue;

    if tra=nil then begin // No firing transition found until now
      fire:=false;
      Con.fTransitionLink.FireTest(fire);
      if fire then begin  // A new firing transition was found
        tra:=Con.fTransitionLink;
        actPriority:=Tra.fPriority;
        actPriorityFrac:=random;
      end;
    end else begin  // A firing transition was found
      if Con.fTransitionLink.fPriority<actPriority then break;
      fire:=false;
      Con.fTransitionLink.FireTest(fire);
      if fire then begin // Another firing transition was found
        tmpFrac:=random;
        if tmpFrac>actPriorityFrac then begin
          tra:=Con.fTransitionLink;
          actPriorityFrac:=tmpFrac;
        end;
      end;
    end;
  end;
  if tra<>nil then begin
    fActiveTransition:=tra;
    fNextState:=GetTransitionOutput(tra.MachineData.index,0).fStateLink;
  end else begin
    fNextState:=fActiveState;
  end;
end;


procedure TDSMStateMachine.CommitNextState;
begin
//  SetActiveState(fNextState);
  if (fActiveState=nil) or (fNextState=nil) or (fActiveTransition=nil) then exit;

  // Deactivate active state
  if Assigned(fActiveState.fStateDeactivation) then
    fActiveState.fStateDeactivation(fActiveState);
  if fVisualFeedback then fActiveState.Invalidate;

  // Report Transition fire
  if Assigned(fActiveTransition.fFiring) then
    fActiveTransition.fFiring(fActiveTransition);

  fPreviousState:=fActiveState;
  fActiveState := fNextState;

  // Activate Next State
  if Assigned(fActiveState.fStateActivation) then
    fActiveState.fStateActivation(fActiveState);
  if fVisualFeedback then fActiveState.Invalidate;

end;


procedure TDSMStateMachine.StepIt;
begin
  EvalNextState;
  if fActiveState <> fNextState then
    CommitNextState;
  if (fActiveState<>nil) then fActiveState.StateActiveActions;
end;

//-------------------------------------------------------------------------------------------------------------
// Registration

procedure Register;
begin
  RegisterComponents('5dpo', [TDSMStateMachine]);
  RegisterComponents('5dpo', [TDSMState]);
  RegisterComponents('5dpo', [TDSMTransition]);
  RegisterComponents('5dpo', [TDSMConnection]);
end;

initialization
  {$i TSdpoDSM.lrs}


end.