unit u_scrollclones;

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls,
  U_OnFormInfoIni,
  {$IFDEF VERSIONS}
    fonctions_version,
  {$ENDIF}
  Controls;

const
  {$IFDEF VERSIONS}
    gVer_ScrollClone: T_Version = (Component: 'Composant TExtClonedPanel';
      FileUnit: 'u_scrollclones';
      Owner: 'Matthieu Giroux';
      Comment:
      'Gestion de liste d''images dans les données.';
      BugsStory : '1.0.1.0 : Problems in events.'+#13#10+
                  '1.0.0.0 : Tested, Reinit procedure.'+#13#10+
                  '0.9.0.0 : Non testée.';
      UnitType: 3;
      Major: 1; Minor: 0; Release: 1; Build: 0);

  {$ENDIF}
    CST_MAX_CLONED_COLS = 1000;

type
  TExtClonedPanel = class;
  TCloneEvent = procedure ( const Sender : TExtClonedPanel; const ASource,ADestination : TControl) of object;
{ TExtClonedPanel }

 TExtClonedPanel = class ( TScrollBox )
  private
    FPanelCloned : TPanel;
    FCols, FRows : Word;
    FAutoControls : {$IFDEF FPC}TFPList{$ELSE}TList{$ENDIF};
    FOnClone,
    FOnCloningPanel, FOnCloningControl : TCloneEvent;
    FOnBeginClones,
    FOnEndClones : TNotifyEvent;
    FFormIni : TOnFormInfoIni;
    function GetAutoControl ( Index : Integer ):TControl;
    function GetAutoControlCount : Integer;
  protected
    procedure SetCols ( const Avalue : Word ); virtual;
    procedure SetRows ( const Avalue : Word ); virtual;
    procedure Notification ( AComponent : TComponent ; AOperation : TOperation ); override;
    procedure AutoSetPanel; virtual;
    procedure ControlEvent ( const AControlSrc,AControlDest : TControl ); virtual;
    procedure PanelClonedEvent ( const ApanelSrc,ApanelDest : TPanel ); virtual;
    procedure PanelCloningEvent ( const ApanelSrc,ApanelDest : TPanel ); virtual;
    procedure SetPanelCloned ( const Apanel : TPanel ); virtual;
  public
    procedure AutoCreateColsRows; virtual;
    procedure Reinit; virtual;
    procedure Loaded; override;
    constructor Create ( AOwner : TComponent ); override;
    destructor  Destroy; override;
    property AutoControlCount : Integer read GetAutoControlCount;
    property AutoControls [ Index : Integer ] : TControl read GetAutoControl;
    property Cols : Word read FCols write SetCols default 1;
    property Rows : Word read FRows write SetRows default 1;
  published
    property FormIni : TOnFormInfoIni read FFormIni write FFormIni;
    property PanelCloned : TPanel read FPanelCloned write SetPanelCloned;
    property OnCloned : TCloneEvent read FOnClone write FOnClone;
    property OnBeginClones : TNotifyEvent read FOnBeginClones write FOnBeginClones;
    property OnEndClones : TNotifyEvent read FOnEndClones write FOnEndClones;
    property OnCloningControl : TCloneEvent read FOnCloningControl write FOnCloningControl;
    property OnCloningPanel   : TCloneEvent read FOnCloningPanel   write FOnCloningPanel;
  End;

implementation

uses fonctions_dbobjects, fonctions_objects,
     fonctions_components,fonctions_dbcomponents,fonctions_proprietes;
{ TExtClonedPanel }

procedure TExtClonedPanel.SetCols(const Avalue: Word);
begin
  if  ( FCols <> Avalue )
  and ( Avalue > 0 ) Then
   Begin
    FCols:=Avalue;
    AutoCreateColsRows;
   end;
end;

function TExtClonedPanel.GetAutoControl( Index: Integer):TControl;
begin
  if  ( Index >= 0 )
  and ( Index < FAutoControls.Count )
   Then Result := TControl(FAutoControls [ Index ])
   Else Result := nil;
end;

function TExtClonedPanel.GetAutoControlCount: Integer;
begin
  Result:=FAutoControls.Count;
end;

procedure TExtClonedPanel.Loaded;
var li_i,li_j:Integer;
begin
  inherited Loaded;
  AutoCreateColsRows;
  if  not ( csDesigning in ComponentState )
  and Assigned(FFormIni) then
   with FFormIni do
    for li_i := 0 to ControlCount-1 do
     if not (Controls[li_i]=FPanelCloned)
      Then
       with Controls[li_i] do
        for li_j := 0 to ControlCount-1 do
          ReadIniComponent(Controls[li_j],Owner);
end;

procedure TExtClonedPanel.SetRows(const Avalue: Word);
begin
  begin
    if  ( FRows <> Avalue )
    and ( Avalue > 0 ) Then
     Begin
      FRows:=Avalue;
      AutoCreateColsRows;
     end;
  end;
end;

procedure TExtClonedPanel.AutoSetPanel;
Begin
  case FPanelCloned.Align of
    alClient :
      Begin
        if ( FCols > 1 ) and ( FRows > 1 ) Then FPanelCloned.Align:= alNone
         else if ( FCols > 1 ) Then FPanelCloned.Align:= alLeft
         Else if ( FRows > 1 ) Then FPanelCloned.Align:= alTop;

      end;
    alLeft : if FRows > 1 Then  FPanelCloned.Align := alNone;
    alTop  : if FCols > 1 Then  FPanelCloned.Align := alNone;
  end;
end;

procedure TExtClonedPanel.ControlEvent(const AControlSrc,AControlDest: TControl);
begin
  if Assigned(FOnCloningControl)
   Then FOnCloningControl ( Self, AControlSrc, AControlDest);
end;

procedure TExtClonedPanel.PanelClonedEvent ( const ApanelSrc,ApanelDest : TPanel );
begin
  if Assigned(FOnClone)
   Then FOnClone ( Self, ApanelSrc, ApanelDest );
end;

procedure TExtClonedPanel.PanelCloningEvent(const ApanelSrc,ApanelDest: TPanel);
begin
  if Assigned(FOnCloningPanel) Then
   FOnCloningPanel ( Self,ApanelSrc,ApanelDest );
end;

procedure TExtClonedPanel.SetPanelCloned(const Apanel: TPanel);
begin
  if FPanelCloned <> Apanel Then
    Begin
     FPanelCloned:=Apanel;
     if assigned ( FPanelCloned ) Then
       AutoCreateColsRows;
    end;
end;

procedure TExtClonedPanel.AutoCreateColsRows;
var i, j, k, ltag : Integer;
    LEndName : String;
    LPanel : TPanel;
    LControlDest,LControlSrc : TControl;
Begin
  if not Assigned(FPanelCloned)
  or ( csDesigning  in ComponentState )
  or ( csDestroying in ComponentState )
  or ( csLoading    in ComponentState   )
   Then
    Exit;
  AutoSetPanel;
  BeginUpdateBounds;
  if Assigned(Parent) Then
    Parent.Visible:=False;
  try
    for i := FAutoControls.Count - 1 downto 0 do
       (TObject(FAutoControls[i])).Destroy;
    FAutoControls.Clear;
    if assigned ( FOnBeginClones ) Then
     FOnBeginClones ( Self );
    for i := 1 to FCols do
     for j := 1 to FRows do
      Begin
        lTag := i * CST_MAX_CLONED_COLS + j;
        if ( i = 1 ) and ( j = 1 )
        Then
         with FPanelCloned do
          Begin
           Tag  := ltag;
           for k := 0 to ControlCount - 1 do
            Begin
              LControlDest := Controls [ k ];
              LControlDest.Tag  := ltag;
            end;
          End
        Else
          Begin
            LPanel := fcon_CloneControlWithDB( FPanelCloned, Owner ) as TPanel;
            PanelCloningEvent ( FPanelCloned, LPanel );
            FAutoControls.Add(LPanel);
            with LPanel do
              Begin
                Parent := Self;
                Left  := ( i - 1 ) * (FPanelCloned.Width  + FPanelCloned.Left) + 1;
                Top   := ( j - 1 ) * (FPanelCloned.Height + FPanelCloned.Top ) + 1 ;
                LEndName:= IntToStr(i) + '_' + IntToStr(j);
                Name := FPanelCloned.name + '_' + LEndName;
                Caption:=FPanelCloned.Caption;
                Tag  := ltag;
                TabOrder:=j*FCols+i;
                with FPanelCloned do
                for k := 0 to ControlCount - 1 do
                 Begin
                   LControlSrc  := Controls [ k ];
                   LControlDest := fcon_CloneControlWithDB ( LControlSrc, Owner );
                   FAutoControls.Add(LControlDest);
                   with LControlDest do
                     Begin
                      Parent := LPanel;
                      Name := LControlSrc.Name + LEndName;
                      p_SetComponentProperty ( LControlDest, CST_PROPERTY_CAPTION, fs_getComponentProperty ( LControlSrc, CST_PROPERTY_CAPTION ));
                      Tag  := ltag;
                     end;
                   if assigned (FFormIni)
                     Then FFormIni.ReadIniComponent(LControlDest,Owner);
                   ControlEvent(LControlSrc,LControlDest);
                 end;
               if assigned (FFormIni) Then FFormIni.ReadIniComponent(LPanel,Owner);
               PanelClonedEvent ( FPanelCloned, LPanel );
              end;
            PanelClonedEvent  ( FPanelCloned, LPanel );
          end;
      end;
  finally
    if Assigned(Parent) Then
      Parent.Visible:=True;
    EndUpdateBounds;
  end;
  if assigned ( FOnEndClones ) Then
   FOnEndClones ( Self );
end;

procedure TExtClonedPanel.Reinit;
begin
  Cols := 1;
end;

procedure TExtClonedPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if  ( AOperation <> opRemove ) Then Exit;

  if ( AComponent = FPanelCloned ) Then
   Begin
     PanelCloned:=nil;
   end;
  if ( AComponent = FFormIni ) Then
   Begin
     FormIni:=nil;
   end;
end;

constructor TExtClonedPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanelCloned:=nil;
  FRows:=1;
  FCols:=1;
  FAutoControls := {$IFDEF FPC}TFPList{$ELSE}TList{$ENDIF}.Create;
  FOnBeginClones:=nil;
  FOnClone:=nil;
  FOnCloningControl:=nil;
  FOnCloningPanel:=nil;
end;

destructor TExtClonedPanel.Destroy;
begin
  inherited Destroy;
  FAutoControls.Free;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion(gVer_ScrollClone);
{$ENDIF}
end.

