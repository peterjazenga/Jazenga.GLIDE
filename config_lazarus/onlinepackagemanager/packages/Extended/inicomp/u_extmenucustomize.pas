unit u_extmenucustomize;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\extends.inc}

interface

uses
  Classes, Menus,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Forms;

  {$IFDEF VERSIONS}
const
      gVer_TExtMenuCustomize : T_Version = ( Component : 'Composant TExtMenuCustomize' ;
                                                 FileUnit : 'u_extmenucustomize' ;
                                                 Owner : 'Matthieu Giroux' ;
                                                 Comment : 'Gestion de l''ini pour un menu avec appel à la fenêtre de personnalisation.' ;
                                                 BugsStory : '0.9.9.0 : Testing' +#10
                                                           + '0.9.0.0 : Gestion en place.';
                                                 UnitType : 3 ;
                                                 Major : 0 ; Minor : 9 ; Release : 9 ; Build : 0 );

  {$ENDIF}
type

  { TExtMenuCustomize }
  TExtMenuCustomize = class(TComponent)
  private
    FAutoIni : Boolean;
    FMenuIni, FMainMenu : TMenu;
    FOnMenuChange : TNotifyEvent;
  protected
    procedure LoadAMenuNode ( const AMenuItemToCopy, AMenuParent : TMenuItem ; const EndSection : String ); virtual;
    procedure SaveAMenuNode ( const AMenuItem : TMenuItem ; const EndSection : String ); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    function  LoadIni ( const EndSection : String = '' ) : Boolean; virtual;
    function  SaveIni ( const EndSection : String = '' ) : Boolean; virtual;
    procedure Loaded; override;
    procedure Click; virtual;
    procedure MenuChange; virtual;
  published
    property AutoIni : Boolean read FAutoIni write FAutoIni default False;
    property MenuIni : TMenu read FMenuIni write FMenuIni;
    property MainMenu : TMenu read FMainMenu write FMainMenu;
    property OnMenuChange : TNotifyEvent read FOnMenuChange write FOnMenuChange;
  end;

implementation

uses
  {$IFDEF VIRTUALTREES}
   U_CustomizeMenu,
  {$ENDIF}
   fonctions_init,
   fonctions_ini,
   fonctions_string,
   fonctions_components;

const MENUINI_SECTION_BEGIN = 'CustomizedMenu.' ;
      MENUINI_MENU_POSITION = '.Position' ;
{ TExtMenuCustomize }

procedure TExtMenuCustomize.Loaded;
begin
  inherited Loaded;
  if FAutoIni
  and not ( csDesigning in ComponentState ) Then
    LoadIni;
end;

procedure TExtMenuCustomize.LoadAMenuNode(const AMenuItemToCopy, AMenuParent: TMenuItem;const EndSection : String );
var i : Integer;
    LMenuToAdd : TMenuItem ;
    lsname : String;
begin
  lsname := fs_GetBeginingOfString(AMenuItemToCopy.Name,CST_NUMBERS);
  if  f_IniReadSectionBol(MENUINI_SECTION_BEGIN+FMenuIni.Name+EndSection, lsname, False )
   Then
    Begin
      LMenuToAdd := fmi_CloneMenuItem ( AMenuItemToCopy, FMenuIni );
      LMenuToAdd.MenuIndex:=f_IniReadSectionInt(MENUINI_SECTION_BEGIN+FMenuIni.Name+EndSection, lsname+MENUINI_MENU_POSITION, FMenuIni.Items.Count );
      AMenuParent.Add ( LMenuToAdd );
    end
   Else
    LMenuToAdd := AMenuParent;
  for i := 0 to AMenuItemToCopy.Count - 1 do
    Begin
      LoadAMenuNode(AMenuItemToCopy [ i ], LMenuToAdd, EndSection);
    end;
end;

procedure TExtMenuCustomize.SaveAMenuNode(const AMenuItem: TMenuItem; const EndSection : String );
var i : Integer;
    lsname : String;
begin
  lsname := fs_GetBeginingOfString(AMenuItem.Name,CST_NUMBERS);
  p_IniWriteSectionBol(MENUINI_SECTION_BEGIN+FMenuIni.Name+EndSection, lsname, True );
  p_IniWriteSectionInt(MENUINI_SECTION_BEGIN+FMenuIni.Name+EndSection, lsname+MENUINI_MENU_POSITION, AMenuItem.MenuIndex );
  for i := 0 to AMenuItem.Count - 1 do
    Begin
      SaveAMenuNode(AMenuItem [ i ], EndSection);
    end;
end;

procedure TExtMenuCustomize.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ( Operation <> opRemove )
  or ( csDestroying in ComponentState ) Then
    Exit;

  // Suppression de menus inexistants
  if    Assigned   ( FMenuIni )
  and ( AComponent = FMenuIni )
   then
    FMenuIni := nil;
  if    Assigned   ( FMainMenu )
  and ( AComponent = FMainMenu )
   then
    FMainMenu := nil;
end;

constructor TExtMenuCustomize.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAutoIni := False;
  FMenuIni := nil;
  FMainMenu := nil;
end;

function TExtMenuCustomize.LoadIni ( const EndSection : String = '' ): Boolean;
begin
  Result := False;
  if  assigned ( FMenuIni )
  and assigned ( FMainMenu )
   Then
    Begin
      FIniFile := f_GetMemIniFile;
      if assigned ( FIniFile ) Then
        if FIniFile.SectionExists(MENUINI_SECTION_BEGIN+FMenuIni.Name + EndSection)
         Then
          Begin
            Result := True;
            FMenuIni.Items.Clear;
            LoadAMenuNode(FMainMenu.Items, FMenuIni.Items, EndSection);
            MenuChange;
          end
        Else
         Begin
           SaveIni ( EndSection );
           MenuChange;
         end;
    end;
end;

function TExtMenuCustomize.SaveIni ( const EndSection : String = '' ): Boolean;
begin
  Result := False;
  FIniFile := f_GetMemIniFile;
  if assigned ( FIniFile )
   Then
    Begin
      Result := True;
      FIniFile.EraseSection(MENUINI_SECTION_BEGIN+FMenuIni.Name + EndSection);
      SaveAMenuNode(FMenuIni.Items, EndSection);
    end;
end;

procedure TExtMenuCustomize.Click;
begin
  {$IFDEF VIRTUALTREES}
  if not assigned ( F_CustomizeMenu )
   Then
    F_CustomizeMenu := TF_CustomizeMenu.Create(Application);
  F_CustomizeMenu.MenuCustomize := Self;
  F_CustomizeMenu.ShowModal;
  if FAutoIni
  and assigned ( FMenuIni )
  and ( FMenuIni.Items.Count > 0 ) Then
    SaveIni;
  MenuChange;
  {$ENDIF}
end;

procedure TExtMenuCustomize.MenuChange;
begin
  if Assigned(FOnMenuChange) Then
   FOnMenuChange ( Self );
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtMenuCustomize );
{$ENDIF}
end.

