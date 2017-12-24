{September 2010 Suny D.
 Based upon Alexander Obukhov's MenuToolbar. .
 I don't modified his code. I writed the code new and took some things from
 him.
Code-state:
1. Menu Clicks don't executed i think LCL has BUG again.
  LCl-BUG: toollbar.submenu clicks dont executed by lcl

  I used ToolButton.DropdownMenu and now it is ok.

2. It it ToolBar with only one new property.
  Menu: Set it to MainMenu and you have MainMenu on toolbar!<
3. Crossplatform- it can be used under windows and linux(tested with gtk2)
  Under gtk2- menuitems dont autopopup with mousemove.
  Under Linux- I have to find how to catch mouse-events if menus opened.
}

unit menutbar;

{$i ..\extends.inc}
{$i ..\dlcompilers.inc}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
{$IFDEF WINDOWS}
  Types, Messages,
{$ENDIF}
  ComCtrls,
  Menus,
  {$IFDEF LCLwin32}LMessages,{$ENDIF}
  Controls;

type

  { TMenuToolBar }

  TMenuToolBar = class(TToolBar)
  private
    FMenu : TMenu;
    FOnMenuCreating ,
    FOnMenuCreated  : TNotifyEvent;
    FCreate : Boolean;
    procedure SetAutoCreate(const Value: Boolean);
{$IFNDEF FPC}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
{$ENDIF}
  protected
    procedure SetMenu(const Value: TMenu); virtual;
    {$IFDEF LCLn32}procedure WndProc(var Message: TLMessage); override;{$ENDIF}
{$IFDEF FPC}
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
{$ENDIF}
    procedure DoOnMenuCreated; virtual;
    procedure DoOnMenuCreating; virtual;
    procedure AutoLoadMenu; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadMenu; virtual;
    procedure Loaded; override;
    destructor Destroy; override;
  published
    property Menu: TMenu read FMenu write SetMenu;
    property AutoCreate: Boolean read FCreate write SetAutoCreate default True;
    property OnMenuCreated: TNotifyEvent read FOnMenuCreated write FOnMenuCreated;
    property OnMenuCreating: TNotifyEvent read FOnMenuCreating write FOnMenuCreating;
    property EdgeBorders default [];
    property Flat default true;
    property List default true;
    property ShowCaptions default true;
    property AutoSize default true;
  end;

  
implementation

uses
{$IFNDEF FPC}
    Forms,
{$IFDEF WINDOWS}Windows,{$ENDIF}
{$ELSE}
{$IFDEF WINDOWS}LCLType,{$ENDIF}
{$ENDIF}

  fonctions_proprietes;


{$IFDEF LCLn32}
var
 HookButton: TToolButton;
 Hook: System.THandle; //HHOOK;


function MTBarHookProc(nCode: Integer; wParam: Longint; var Msg: TMsg): Longint; stdcall;
var Pt: TPoint;
    TB: TToolButton;
    Control: TControl;
    Toolbar: TMenuToolbar;
    ASMallPoint : TSmallPoint;
begin
  if (nCode = MSGF_MENU) and Assigned(HookButton) then
   case Msg.Message of
    WM_MOUSEMOVE:
      begin
        ASMallPoint := TSmallPoint(Msg.lParam);
        Pt.x:=ASMallPoint.x;
        Pt.y:=ASMallPoint.y;
        Toolbar:= HookButton.Parent as TMenuToolbar;
        Pt:= Toolbar.ScreenToClient(Pt);
        if Assigned(Toolbar) and PtInRect(Toolbar.ClientRect, Pt) then
         begin
            Control:= Toolbar.ControlAtPos(Pt, False);
            if (Control is TToolButton) then
             begin
                TB:= Control as TToolButton;
                if (TB<>HookButton) then
                 Toolbar.;
                  PostMessage(Toolbar.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLong(Pt.x, Pt.y));
            end;
        end;
      end;

   end; //from case

  if Hook<>0 then Result:= CallNextHookEx(Hook, nCode, wParam, Longint(@Msg));
end;
{$ENDIF}

constructor TMenuToolBar.Create(TheOwner: TComponent);
begin
  inherited;
  EdgeBorders := [];
  Flat:=true;
  List:=true;
  ShowCaptions:=true;
  AutoSize:=true;
  FMenu:=nil;
  FOnMenuCreated := nil;
  FOnMenuCreating := nil;
  FCreate := True;
end;

destructor TMenuToolBar.Destroy;
begin
  {$IFDEF LCLn32}UnhookWindowsHookEx(Hook);{$ENDIF}
  inherited;
end;

{$IFDEF LCLn32}
procedure TMenuToolBar.WndProc(var Message: TLMessage);
var TB: TControl;
    Pt: TPoint;
begin
  if (not (csDesigning in ComponentState))then
   case Message.Msg of

   WM_LBUTTONDOWN: begin
        Pt.x:= (TSmallPoint(Message.lParam)).X;
        Pt.y:= (TSmallPoint(Message.lParam)).y;
        TB:= ControlAtPos(Pt, False);
        if (TB is TToolButton) then
         begin
          inherited;
          HookButton:= TB as TToolButton;
          if Assigned(HookButton.OnClick) then DefaultHandler(Message);
          if HookButton.DropdownMenu<>nil then
           begin // MenuItem<>nil; HookButton.MenuHandle ;Tag<>0 then
            if Hook=0 then Hook:= SetWindowsHookEx(WH_MSGFILTER, windows.HOOKPROC(@MTBarHookProc), 0, GetCurrentThreadID);
            Message.Msg:= WM_LBUTTONUP;
            DefaultHandler(Message);
          end;
        end;
      end;

    WM_RBUTTONDOWN: begin
        Pt:= {$IFDEF CPU64}TPoint({$ELSE}SmallPointToPoint(TSmallPoint{$ENDIF}(Message.lParam));
        TB:= ControlAtPos(Pt, False);
        if (TB is TToolButton) then
         begin
          inherited;
          HookButton:= TB as TToolButton;
          if Assigned(HookButton.OnClick) then DefaultHandler(Message);
            if HookButton.DropdownMenu<>nil then begin // MenuItem<>nil; HookButton.MenuHandle ;HookButton.Tag<>0
              if Hook=0 then Hook:= SetWindowsHookEx(WH_MSGFILTER, windows.HOOKPROC(@MTBarHookProc), 0, GetCurrentThreadID);
              Message.Msg:= WM_LBUTTONUP;
              DefaultHandler(Message);
            end;
        end;
      end;

   end; //from case

  inherited;
end;


{$ENDIF}

{$IFDEF FPC}
procedure TMenuToolBar.DoOnShowHint(HintInfo: PHintInfo);
{$ELSE}
procedure TMenuToolBar.CMHintShow(var Message: TMessage);
{$ENDIF}
begin
{$IFDEF FPC}
  with HintInfo^ do
{$ELSE}
  with TCMHintShow(Message).HintInfo^ do
{$ENDIF}
   Begin
     HintControl := ControlAtPos(CursorPos,True);
     if HintControl = nil Then
      Begin
        HintControl:=Self;
        inherited;
      end
     Else
      Begin
        if HintControl.Hint > ''
         Then HintStr:=HintControl.Hint
         Else HintStr:=fs_getComponentProperty(HintControl,CST_PROPERTY_CAPTION);
{$IFNDEF FPC}
        inherited;
{$ENDIF}
      End;
   end;
end;

procedure TMenuToolBar.DoOnMenuCreated;
Begin
  if Assigned(FOnMenuCreated) Then
    FOnMenuCreated ( Self );

end;

procedure TMenuToolBar.DoOnMenuCreating;
Begin
  if Assigned(FOnMenuCreating) Then
    FOnMenuCreating ( Self );

end;

procedure TMenuToolBar.AutoLoadMenu;
begin
  if FCreate
  and ( csDesigning in ComponentState )
  and not ( csCreating in ControlState ) Then
    LoadMenu;
end;

procedure TMenuToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ( Operation <> opRemove )
  or ( csDestroying in ComponentState ) Then
    Exit;

  // Suppression d'un menu inexistant
  if    Assigned   ( FMenu )
  and ( AComponent = FMenu )
   then
    FMenu := nil;
end;

procedure TMenuToolBar.SetAutoCreate(const Value: Boolean);
begin
  FCreate:=Value;
  AutoLoadMenu;
end;

procedure TMenuToolBar.SetMenu(const Value: TMenu);

begin
  if (FMenu=Value) then exit;
  FMenu:=Value;
  AutoLoadMenu;
end;

procedure TMenuToolBar.LoadMenu;

  procedure aCreatePopupFromMenu(SrcMenuItem, DestMenuItem: TMenuItem);
   var i: Integer;
       MovingMenuItem: TMenuItem;
   begin
     for i := SrcMenuItem.Count - 1 downto 0 do
      begin
       MovingMenuItem := SrcMenuItem.Items[i];
       SrcMenuItem.Delete(i);
       DestMenuItem.Insert(0, MovingMenuItem);
      end;
   end;

var aTB: TToolButton;
    i: integer;
begin
  if Assigned(FMenu) then
   begin
    DoOnMenuCreating;
    while ControlCount>0 do Controls[0].Destroy; //delete old menubuttons
    for I:=0  to FMenu.Items.Count-1 do
     begin
      aTB:= TToolButton.Create(Self);
      with FMenu.Items[I] do
        begin
          aTB.Name:= Self.Name + Name;
          aTB.Tag:= Tag;
          aTB.Caption:= Caption;
          aTB.Hint:= Hint;
          aTB.OnClick:= OnClick;
          aTB.ImageIndex:= ImageIndex;
        end;
      aTB.Style:= tbsButton;
      {Onclick from MenuItem-Submenus don't called by LCL i must use DropDownMenu
      aTB.MenuItem:=FMenu.Items[I]; }
      //create dropdownmenu
      if FMenu.Items[I].Count > 0 Then
        if (not (csDesigning in ComponentState))then
         begin //else MenuItems will be freed
          aTB.DropdownMenu:=TPopupMenu.Create(self);
          aTB.DropdownMenu.Images:=Images;
          aCreatePopupFromMenu(FMenu.Items[I], aTB.DropdownMenu.Items);
        end;
      if Self <> aTB.Parent Then
        aTB.Parent:= Self;
    end;
    // ordering
    for I:=0  to FMenu.Items.Count-1 do
      RepositionButton(FMenu.Items[i].MenuIndex);
    DoOnMenuCreated;
  end;
end;



procedure TMenuToolBar.Loaded;
begin
  inherited Loaded;
  AutoLoadMenu;
end;

end.

