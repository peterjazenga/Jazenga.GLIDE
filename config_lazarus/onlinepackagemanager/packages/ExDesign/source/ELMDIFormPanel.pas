{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELMDIFormPanel;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf,LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, GraphType,
  ELMDIForm;

type

  MDIFormArray = array of TplMDIForm;

TplMDIFormsPanel = class(TCustomPanel)
  private
    FMDIFormIndex,FActiveMDIForm: integer;
    FMaximized, FKeepLastMDIForm, FWireframeMoveResize: boolean;
    FBorderWidth, FTitleHeight: integer;
    FBorderColor, FInactiveColor, FTitleColor: TColor;
    FonMaximize : TNotifyEvent;
    FOnResize : TNotifyEvent;
    FonActiveMDIFormChange : TNotifyEvent;
    FOnCreateMDIForm : TNotifyEvent;
    FOnDeleteMDIForm : TNotifyEvent;
    FMDIForm: MDIFormArray;
    DestroyTimer:TTimer;
    DestroyPending: array of TplMDIForm;
    DestroyPendingCount: integer;
    DestroyCriticalSection: TCriticalSection;
    FWindowList: TmenuItem;
    FWindowListOffset:integer;
    DefaultPos:TPoint;
    function GetMDIFormCount:integer;
    function GetActiveMDIForm: TplMDIForm;
    function GetActiveObject: TFrame;
    procedure MDIFormClose(Sender: TObject);
    procedure MDIFormMaximize(Sender: TObject);
    procedure MDIFormRestore(Sender: TObject);
    procedure CaptionChange(Sender: TObject);
    procedure MDIFormEnter(Sender: TObject);
    procedure FocusMDIFormClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SetMaximized(value:boolean);
    procedure SetBorderWidth(value:integer);
    procedure SetTitleHeight(value:integer);
    procedure SetBorderColor(value:TColor);
    procedure SetInactiveColor(value:TColor);
    procedure SetTitleColor(value:TColor);
    procedure SetWindowList(value:TmenuItem);
    procedure SetWireframeMoveResize(value:boolean);
    procedure SetResize(Sender: TObject);
    procedure DestroyMDIFormTimer(Sender: TObject);
  protected
    procedure Loaded; override;
  public

    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    property MDIForms: MDIFormArray read FMDIForm;
    function NewMDIForm(Aname:string=''):TplMDIForm;
    function AddMDIForm(aMDIFormFrame:TplMDIForm):integer;
    function NexMDIForm:integer;
    procedure Cascade;
    procedure TileVertical;
    procedure TileHorizontal;
    procedure SetActiveMDIForm(n:integer);
    property MDIFormCount: integer read GetMDIFormCount;
    property ActiveMDIForm: TplMDIForm read GetActiveMDIForm;
    property ActiveObject: TFrame read GetActiveObject;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property InactiveBorderColor: TColor read FInactiveColor write SetInactiveColor;
    property TitleColor: TColor read FTitleColor write SetTitleColor;
  published
    property WindowList: TmenuItem read FWindowList write SetWindowList;
    property Maximized: boolean read FMaximized write SetMaximized;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property TitleHeight: integer read FTitleHeight write SetTitleHeight;
    property KeepLastMDIForm: boolean read FKeepLastMDIForm write FKeepLastMDIForm;
    property WireframeMoveResize: boolean read FWireframeMoveResize write SetWireframeMoveResize;
    property onMaximize: TNotifyEvent read FonMaximize write FonMaximize;
    property onActiveMDIFormChange: TNotifyEvent read FonActiveMDIFormChange write FonActiveMDIFormChange;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnCreateMDIForm : TNotifyEvent read FOnCreateMDIForm write FOnCreateMDIForm;
    property OnDeleteMDIForm : TNotifyEvent read FOnDeleteMDIForm write FOnDeleteMDIForm;

    published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation


constructor TplMDIFormsPanel.Create(AOwner:TComponent);
begin
inherited Create(AOwner);
DoubleBuffered:=true;
FMDIFormIndex:=-1;
FActiveMDIForm:=-1;
FWindowListOffset:=0;
setlength(FMDIForm,0);
color:=clwhite;
align:=alClient;
BevelOuter:=bvNone;
BevelWidth:=1;
FBorderWidth:=3;  FTitleHeight:=12;
FBorderColor:=clActiveCaption;
FInactiveColor:=clInactiveCaption;
FTitleColor:=clCaptionText;
{$ifdef lclgtk}
FBorderColor:=$C00000;
FInactiveColor:=clGray;
FTitleColor:=clWhite;
{$endif}
DefaultPos:=Point(0,0);
FOnResize:=nil;
Inherited onResize:=@SetResize;
InitializeCriticalSection(DestroyCriticalSection);
DestroyPendingCount:=0;
SetLength(DestroyPending,DestroyPendingCount);
DestroyTimer:=TTimer.Create(self);
DestroyTimer.Enabled:=false;
DestroyTimer.Interval:=100;
DestroyTimer.OnTimer:=@DestroyMDIFormTimer;
end;

destructor  TplMDIFormsPanel.Destroy;
begin
try
DeleteCriticalSection(DestroyCriticalSection);
FActiveMDIForm:=-1;
inherited destroy;
except
end;
end;

Function TplMDIFormsPanel.NewMDIForm(Aname:string=''):TplMDIForm;
var m: TmenuItem;
begin
inc(FMDIFormIndex);
setlength(FMDIForm,FMDIFormIndex+1);
FMDIForm[FMDIFormIndex]:=TplMDIForm.Create(self);
FMDIForm[FMDIFormIndex].Caption:=Aname;
FMDIForm[FMDIFormIndex].Parent:=self;
FMDIForm[FMDIFormIndex].SetBorderWdth(BorderWidth);
FMDIForm[FMDIFormIndex].SetBorderColor(BorderColor);
FMDIForm[FMDIFormIndex].SetTitleHeight(TitleHeight);
FMDIForm[FMDIFormIndex].SetTitleColor(TitleColor);
FMDIForm[FMDIFormIndex].WireframeMoveResize:=WireframeMoveResize;
FMDIForm[FMDIFormIndex].onClose:=@MDIFormClose;
FMDIForm[FMDIFormIndex].onMaximize:=@MDIFormMaximize;
FMDIForm[FMDIFormIndex].onRestore:=@MDIFormRestore;
FMDIForm[FMDIFormIndex].onCaptionChange:=@CaptionChange;
FMDIForm[FMDIFormIndex].onEnter:=@MDIFormEnter;
FMDIForm[FMDIFormIndex].onCloseQuery:=@FormCloseQuery;
FMDIForm[FMDIFormIndex].Tag:=FMDIFormIndex;
FMDIForm[FMDIFormIndex].Top:=DefaultPos.Y;
FMDIForm[FMDIFormIndex].Left:=DefaultPos.X;
FActiveMDIForm:=FMDIFormIndex;
if Assigned(FWindowList) then begin
  try
  m:=TmenuItem.Create(self);
  m.Caption:='MDIForm '+ inttostr(FMDIFormIndex);
  m.Tag:=100+FMDIFormIndex;
  m.OnClick:=@FocusMDIFormClick;
  FWindowList.Add(m);
  except
  end;
end;
if Assigned(FOnCreateMDIForm) then FOnCreateMDIForm(FMDIForm[FMDIFormIndex]);
FMDIForm[FMDIFormIndex].Maximized:=FMaximized;
SetActiveMDIForm(FMDIFormIndex);
DefaultPos.X:=DefaultPos.X+FTitleHeight;
DefaultPos.Y:=DefaultPos.Y+FTitleHeight;
if DefaultPos.Y>10*FTitleHeight then begin
   DefaultPos.Y:=0;
   if DefaultPos.X>30*FTitleHeight then DefaultPos.X:=0;
end;
result:=FMDIForm[FMDIFormIndex];
end;


function TplMDIFormsPanel.AddMDIForm(aMDIFormFrame:TplMDIForm):integer;
var m: TmenuItem;
begin
if aMDIFormFrame=nil then exit;

inc(FMDIFormIndex);
setlength(FMDIForm,FMDIFormIndex+1);

FMDIForm[FMDIFormIndex]:=aMDIFormFrame;
FMDIForm[FMDIFormIndex].Parent:=self;
FMDIForm[FMDIFormIndex].WireframeMoveResize:=WireframeMoveResize;
FMDIForm[FMDIFormIndex].onClose:=@MDIFormClose;
FMDIForm[FMDIFormIndex].onMaximize:=@MDIFormMaximize;
FMDIForm[FMDIFormIndex].onRestore:=@MDIFormRestore;
FMDIForm[FMDIFormIndex].onCaptionChange:=@CaptionChange;
FMDIForm[FMDIFormIndex].onEnter:=@MDIFormEnter;
FMDIForm[FMDIFormIndex].onCloseQuery:=@FormCloseQuery;
FMDIForm[FMDIFormIndex].Tag:=FMDIFormIndex;
FActiveMDIForm:=FMDIFormIndex;
if Assigned(FWindowList) then
begin
  try
  m:=TmenuItem.Create(self);
  m.Caption:='MDIForm '+ inttostr(FMDIFormIndex);
  m.Tag:=100+FMDIFormIndex;
  m.OnClick:=@FocusMDIFormClick;
  FWindowList.Add(m);
  except
  end;
end;

SetActiveMDIForm(FMDIFormIndex);
result:=FMDIFormIndex;
end;

function TplMDIFormsPanel.NexMDIForm:integer;
var i: integer;
begin
result:=0;
if FMDIFormIndex>0 then begin
   i:=FActiveMDIForm;
   inc(i);
   if i>FMDIFormIndex then i:=0;
   SetActiveMDIForm(i);
   result:=i;
end;
end;

procedure TplMDIFormsPanel.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
CanClose:= not (KeepLastMDIForm and (MDIFormCount=1));
end;

procedure TplMDIFormsPanel.MDIFormClose(Sender: TObject);
var i,j,n: integer;
begin
if Assigned(FOnDeleteMDIForm) then FOnDeleteMDIForm(Sender);
n:=(Sender as TplMDIForm).Tag;
if Assigned(FWindowList) then begin
  j:=FWindowList.Count;
  for i:=0 to FWindowList.Count-1 do
   if FWindowList.Items[i].tag=100+n then begin
      FWindowList.Delete(i);
      j:=i;
      break;
   end;
  for i:=j to FWindowList.Count-1 do FWindowList.Items[i].tag:=FWindowList.Items[i].tag-1;
end;
for i:=n to FMDIFormIndex-1 do begin
   FMDIForm[i]:=FMDIForm[i+1];
   FMDIForm[i].Tag:=i;
end;
dec(FMDIFormIndex);
setlength(FMDIForm,FMDIFormIndex+1);
SetActiveMDIForm(FMDIFormIndex);
EnterCriticalSection(DestroyCriticalSection);
inc(DestroyPendingCount);
SetLength(DestroyPending,DestroyPendingCount);
DestroyPending[DestroyPendingCount-1]:=TplMDIForm(Sender);
DestroyTimer.Enabled:=true;
LeaveCriticalSection(DestroyCriticalSection);
end;

procedure TplMDIFormsPanel.MDIFormMaximize(Sender: TObject);
var i: integer;
begin
for i:=0 to FMDIFormIndex do begin
   FMDIForm[i].maximized:=true;
end;
FMaximized:=true;
if assigned(FonMaximize) then FonMaximize(self);
end;

procedure TplMDIFormsPanel.MDIFormRestore(Sender: TObject);
var i: integer;
begin
for i:=0 to FMDIFormIndex do begin
   FMDIForm[i].maximized:=false;
end;
FMaximized:=false;
if assigned(FonMaximize) then FonMaximize(self);
end;

procedure TplMDIFormsPanel.SetMaximized(value:boolean);
var i: integer;
begin
FMaximized:=value;
for i:=0 to FMDIFormIndex do begin
  FMDIForm[i].maximized:=FMaximized;
end;
if assigned(FonMaximize) then FonMaximize(self);
end;

procedure TplMDIFormsPanel.CaptionChange(Sender: TObject);
var i,n: integer;
begin
n:=(Sender as TplMDIForm).Tag+100;
if Assigned(FWindowList) then
 for i:=0 to FWindowList.Count-1 do
   if FWindowList.Items[i].tag=n then begin
      FWindowList.Items[i].Caption:=(Sender as TplMDIForm).Caption;
      break;
   end;
end;

function TplMDIFormsPanel.GetMDIFormCount:integer;
begin
result:= FMDIFormIndex+1;
end;

function TplMDIFormsPanel.GetActiveMDIForm: TplMDIForm;
begin
if FActiveMDIForm>=0 then
   result:=FMDIForm[FActiveMDIForm]
else
   result:=nil;
end;

function TplMDIFormsPanel.GetActiveObject: TFrame;
begin
if FActiveMDIForm>=0 then
   result:=FMDIForm[FActiveMDIForm].DockedObject
else
   result:=nil;
end;

procedure TplMDIFormsPanel.SetActiveMDIForm(n:integer);
var i:integer;
    change: boolean;
begin
try
if (n<0)or(n>FMDIFormIndex) then exit;
if (parent<>nil) and parent.visible and (n>=0) then begin
  FMDIForm[n].BringToFront;
end;
except
end;
change:=(FActiveMDIForm<>n);
FActiveMDIForm:=n;
try
for i:=0 to FMDIFormIndex do begin
  if i=n then FMDIForm[i].SetBorderColor(FBorderColor)
         else FMDIForm[i].SetBorderColor(FInactiveColor);
end;
if change and assigned(FonActiveMDIFormChange) then FonActiveMDIFormChange(self);
except
end;
end;

procedure TplMDIFormsPanel.FocusMDIFormClick(Sender: TObject);
begin
SetActiveMDIForm((Sender as TmenuItem).Tag-100);
end;

procedure TplMDIFormsPanel.MDIFormEnter(Sender: TObject);
begin
SetActiveMDIForm((Sender as TplMDIForm).Tag);
end;

procedure TplMDIFormsPanel.SetBorderWidth(value:integer);
var i:integer;
begin
FBorderWidth:=value;
for i:=0 to FMDIFormIndex do begin
  FMDIForm[i].BorderWidth:=FBorderWidth;
end;
end;

procedure TplMDIFormsPanel.SetTitleHeight(value:integer);
var i:integer;
begin
FTitleHeight:=value;
for i:=0 to FMDIFormIndex do begin
  FMDIForm[i].SetTitleHeight(FTitleHeight);
end;
end;

procedure TplMDIFormsPanel.SetBorderColor(value:TColor);
begin
FBorderColor:=value;
if FActiveMDIForm>=0 then FMDIForm[FActiveMDIForm].SetBorderColor(FBorderColor);
end;

procedure TplMDIFormsPanel.SetInactiveColor(value:TColor);
var i:integer;
begin
FInactiveColor:=value;
for i:=0 to FMDIFormIndex do begin
  if i<>FActiveMDIForm then FMDIForm[i].SetBorderColor(FBorderColor);
end;
end;

procedure TplMDIFormsPanel.SetTitleColor(value:TColor);
var i:integer;
begin
FTitleColor:=value;
for i:=0 to FMDIFormIndex do begin
  FMDIForm[i].SetTitleColor(FTitleColor);
end;
end;

procedure TplMDIFormsPanel.SetWireframeMoveResize(value:boolean);
var i:integer;
begin
FWireframeMoveResize:=value;
for i:=0 to FMDIFormIndex do begin
  FMDIForm[i].WireframeMoveResize:=FWireframeMoveResize;
end;
end;

procedure TplMDIFormsPanel.SetWindowList(value:TmenuItem);
begin
FWindowList:=value;
FWindowListOffset:=FWindowList.Count;
end;

procedure TplMDIFormsPanel.SetResize(Sender: TObject);
var i: integer;
begin
if Maximized then
  for i:=0 to FMDIFormIndex do begin
    FMDIForm[i].top:=0;
    FMDIForm[i].left:=0;
    FMDIForm[i].Width:=ClientWidth;
    FMDIForm[i].Height:=ClientHeight;
  end;
if Assigned(FOnResize) then  FOnResize(Sender);
end;

procedure TplMDIFormsPanel.Cascade;
var i,x,y:integer;
begin
Maximized:=false;
if MDIFormCount>0 then begin
x:=0; y:=0;
for i:=0 to FMDIFormIndex do begin
  FMDIForm[i].RestoreSize;
  FMDIForm[i].top:=x;
  FMDIForm[i].left:=y;
  FMDIForm[i].BringToFront;
  x:=x+FTitleHeight;
  y:=y+FTitleHeight;
  if y>10*FTitleHeight then begin
     y:=0;
     if x>30*FTitleHeight then x:=0;
  end;
end;
SetActiveMDIForm(FMDIFormIndex);
end;
end;

procedure TplMDIFormsPanel.TileVertical;
var i,j,dx,dy,nx,ny,x,y,n:integer;
    d: double;
begin
Maximized:=false;
if MDIFormCount>0 then begin
  d:=round(100*sqrt(FMDIFormIndex+1))/100;
  ny:=trunc(d);
  if frac(d)=0 then nx:=ny
    else if frac(d)<0.5 then nx:=ny+1
         else begin ny:=ny+1; nx:=ny; end;
  dx:=clientwidth div nx;
  dy:=clientheight div ny;
  for i:=0 to nx-1 do begin
   for j:=0 to ny-1 do begin
    x:=i*dx;
    y:=j*dy;
    n:=i*ny+j;
    if n<FMDIFormIndex then begin
      FMDIForm[n].top:=y;
      FMDIForm[n].left:=x;
      FMDIForm[n].width:=dx;
      FMDIForm[n].height:=dy;
    end;
    if n=FMDIFormIndex then begin
      FMDIForm[n].top:=y;
      FMDIForm[n].left:=x;
      FMDIForm[n].width:=dx;
      FMDIForm[n].height:=clientheight-y;
    end;
   end;
  end;
end;
end;

procedure TplMDIFormsPanel.TileHorizontal;
var i,j,dx,dy,nx,ny,x,y,n:integer;
    d: double;
begin
Maximized:=false;
if MDIFormCount>0 then begin
  d:=round(100*sqrt(FMDIFormIndex+1))/100;
  nx:=trunc(d);
  if frac(d)=0 then ny:=nx
    else if frac(d)<0.5 then ny:=nx+1
         else begin nx:=nx+1; ny:=nx; end;
  dx:=clientwidth div nx;
  dy:=clientheight div ny;
  for i:=0 to ny-1 do begin
   for j:=0 to nx-1 do begin
    x:=j*dx;
    y:=i*dy;
    n:=i*nx+j;
    if n<FMDIFormIndex then begin
      FMDIForm[n].top:=y;
      FMDIForm[n].left:=x;
      FMDIForm[n].width:=dx;
      FMDIForm[n].height:=dy;
    end;
    if n=FMDIFormIndex then begin
      FMDIForm[n].top:=y;
      FMDIForm[n].left:=x;
      FMDIForm[n].width:=clientwidth-x;
      FMDIForm[n].height:=dy;
    end;
   end;
  end;
end;
end;

procedure TplMDIFormsPanel.DestroyMDIFormTimer(Sender: TObject);
var i,n: integer;
begin
DestroyTimer.Enabled:=false;
EnterCriticalSection(DestroyCriticalSection);
n:=DestroyPendingCount-1;
if n>=0 then begin
  for i:=0 to n do begin
     if DestroyPending[i]<>nil then
        DestroyPending[i].Free;
  end;
end;
DestroyPendingCount:=0;
SetLength(DestroyPending,DestroyPendingCount);
LeaveCriticalSection(DestroyCriticalSection);
end;

procedure TplMDIFormsPanel.Loaded;
var i: integer;
begin
  inherited Loaded;

  if self.ControlCount<1 then exit;

  for i:=0 to self.ControlCount-1 do
    begin
      if self.Controls[i] is TplMDIForm then
        self.AddMDIForm(TplMDIForm(self.Controls[i]));
    end;
  //..........................
end;

end.
