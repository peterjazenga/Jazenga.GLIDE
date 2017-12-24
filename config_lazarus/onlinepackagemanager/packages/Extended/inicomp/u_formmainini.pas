unit U_FormMainIni;
// Unité de la Version 2 du projet FormMain
// La version 1 TFormMain n'est pas sa fenêtre parente

// Le module crée des propriété servant à la gestion du fichier INI
// Il gère la déconnexion
// Il gère la gestion des touches majuscules et numlock
// Il gère les forms enfants
// créé par Matthieu Giroux en décembre 2007

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, lmessages,
{$ELSE}
  Windows, OleDb, Messages,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF TNT}
  TNTForms,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, fonctions_ini, IniFiles,
  U_FormAdapt;

{$IFDEF VERSIONS}
  const
    gVer_TFormMainIni : T_Version = (  Component : 'Composant Fenêtre principale' ;
                                       FileUnit : 'U_FormMainIni' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Fiche principale deuxième version.' ;
                                       BugsStory : '1.2.0.0 : TF_FormAdapt inherit.' + #13#10
                                                 + '1.1.1.1 : Debuging.' + #13#10
                                                 + '1.1.1.0 : Changing fi_findForm to ffor_findForm.' + #13#10
                                                 + '1.1.0.2 : Some fb_CreateChild to fp_CreateChild, Creating p_CloseForm from p_CloseMDI.' + #13#10
                                                 + '1.1.0.1 : No static method on protected and public.' + #13#10
                                                 + '1.1.0.0 : Passage en générique.' + #13#10
                                                 + '1.0.0.0 : Gestion INI, de fiches et du clavier.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 2 ; Release : 0 ; Build : 0 );

{$ENDIF}
type
  { TF_FormMainIni }

  TF_FormMainIni = class(TF_FormAdapt)
  private
    {$IFDEF SFORM}
    FBoxChilds : TWinControl;
    {$ENDIF}
    { Déclarations privées }
    // Gestion du clavier
    gEv_OldActivate    ,
    gEv_OldDeActivate  : TNotifyEvent ;
    {$IFNDEF FPC}
    gt_Buffer : TKeyboardState;
    {$ENDIF}
    ge_WriteSessionIni,
    ge_ReadSessionIni,
    ge_WriteMainIni,
    ge_ReadMainIni : TIniEvent ;
    gh_WindowHandle : HWND;
    FAutoIni    : Boolean ;

    procedure p_modalStart ( Aobj_Objet : Tobject );
    procedure p_modalEnded ( Aobj_Objet : Tobject );
  protected
    // Termine l'appli sans sauver le fichier IN
    procedure p_TerminateWithoutIni ; virtual;
    // Gestion du clavier à la reprise
    procedure p_ApplicationActivate(Sender: TObject); virtual;
    procedure p_ApplicationDeActivate(Sender: TObject); virtual;

    // A appeler si on n'appelle pas le constructeur
    procedure p_CreeFormMainIni (AOwner:TComponent); virtual;

  public
    { Déclarations publiques }
    gb_CloseQuery : Boolean ;
    // Procédure appelée quand il n'y a pas de connexion
    procedure p_NoConnexion; virtual;
    procedure p_CloseQueryChildForms(const ab_Free: Boolean);

    // Non connecté
    procedure p_PbConnexion; virtual;
    // Connecté
    procedure p_Connectee; virtual;
    procedure p_FreeChildForms ; virtual;
    function CloseQuery: Boolean; override;
    // Touche enfoncée
    function IsShortCut(var ao_Msg: {$IFDEF FPC} TLMKey {$ELSE} TWMKey {$ENDIF}): Boolean; override;
    // Constructeur et destructeur
    Constructor Create ( AOwner : TComponent ); override;
    destructor Destroy; override;
    {Lit le fichier ini
    pour le composant form TF_FormMainIni
    avec connexion d'une base ADO
    et appel de la procédure p_InitialisationParamIni dans la form si AutoReadIni,
    de la procédure p_IniInitialisation s'il n'existe pas de fichier INI}
    function f_IniGetConfigFile: TIniFile; virtual;
    function f_GetIniFile : TIniFile; virtual;

    // Procédure mettant à jour la procédure virtuelle p_setMajNumResult
    procedure p_MiseAJourMajNumScroll; virtual;

    // Procédures qui sont appelées automatiquement pour l'initialisation et la sauvegarde
    // Initialisation du fichier INI
    procedure p_InitialisationParamIni; virtual;
    // Sauvegarde du fichier INI
    procedure p_SauvegardeParamIni; virtual;
    // Après la Sauvegarde du fichier INI
    procedure p_ApresSauvegardeParamIni; virtual;
    // Ecriture de l'ini dans le descendant
    procedure p_WriteDescendantIni(const amif_Init: TIniFile); virtual;
    // Lecture de l'ini dans le descendant
    procedure p_ReadDescendantIni(const amif_Init: TIniFile); virtual;
    // Gestion du clavier
    // Entrée : les trois touches : MAJ NUM SCROLLLOCK
    procedure p_SortieMajNumScroll ( const ab_MajEnfoncee, ab_NumEnfoncee, ab_ScrollEnfoncee : boolean ) ; virtual;
    procedure DoShow; override;
  published
    {$IFDEF SFORM}
    property BoxChilds : TWinControl read FBoxChilds write FBoxChilds stored True ;
    {$ENDIF}
    property AutoIni    : Boolean read FAutoIni write FAutoIni stored True default True ;
    property ReadMainIni : TIniEvent read ge_ReadMainIni write ge_ReadMainIni ;
    property WriteMainIni : TIniEvent read ge_WriteMainIni write ge_WriteMainIni ;
    property ReadSessionIni : TIniEvent read ge_ReadSessionIni write ge_ReadSessionIni ;
    property WriteSessionIni : TIniEvent read ge_WriteSessionIni write ge_WriteSessionIni ;

  end;


var
  gb_MotPasseEstValide : Boolean ;

implementation

uses fonctions_erreurs, TypInfo,
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
  fonctions_keyboard,
  fonctions_system,
  fonctions_forms;




{ TF_FormMainIni }

////////////////////////////////////////////////////////////////////////////////
// Constructeur de l'objet TF_FormMainIni
// Initialise le fichier ini
////////////////////////////////////////////////////////////////////////////////
Constructor TF_FormMainIni.Create(AOwner:TComponent);
begin
  {$IFDEF SFORM}
  FBoxChilds := nil;
  {$ENDIF}
  Inherited create  (AOwner);
  p_CreeFormMainIni (AOwner);
end;

destructor TF_FormMainIni.Destroy;
begin
  f_GetMemIniFile;
  p_IniQuitte;
  inherited Destroy;
end;

// A appeler si on n'appelle pas le constructeur
procedure TF_FormMainIni.p_CreeFormMainIni (AOwner:TComponent);
begin
  gb_CloseQuery := False ;
  gb_ModalStarted := False ;
  gh_WindowHandle := 0;

  if not (csDesigning in ComponentState) then //si on est pas en mode conception
    begin
      {$IFDEF DELPHI}
      (AOwner as TApplication).OnModalBegin := p_modalStart ;
      (AOwner as TApplication).OnModalEnd   := p_modalEnded ;
      {$ENDIF}

      gEv_OldActivate   := (AOwner as TApplication).OnActivate;
      gEv_OldDeActivate := (AOwner as TApplication).OnDeActivate;
      (AOwner as TApplication).OnActivate   := p_ApplicationActivate;
      (AOwner as TApplication).OnDeActivate := p_ApplicationDeActivate;
    end;
  gs_NomApp := fs_GetSoftName;
  // Lecture des fichiers INI
  if FAutoIni Then
    f_GetIniFile ;
End ;

procedure TF_FormMainIni.p_FreeChildForms;
var lw_i : Word ;
begin
  gb_CloseQuery := True ;
  for lw_i := Application.ComponentCount - 1 downto 0 do
    begin
      if  (Application.Components[lw_i] is TForm)
      and not (Application.Components[lw_i] is TF_FormMainIni)
      and (Application.Components[lw_i] <> Self) then
        Begin
          (Application.Components[lw_i] as TForm).Free;
          Application.ProcessMessages ;
        End ;
    end;
  gb_CloseQuery := False ;
end;

////////////////////////////////////////////////////////////////////////////////
//  Evènements de l'application
////////////////////////////////////////////////////////////////////////////////

//  Désactivation de l'application
// Sender : obligatoire ( l'application )
procedure TF_FormMainIni.p_ApplicationActivate(Sender: TObject);
begin
  p_MiseAJourMajNumScroll;
end;

//  DésActivation de l'application
// Sender : obligatoire ( l'application )
procedure TF_FormMainIni.p_ApplicationDeActivate(Sender: TObject);
begin
  // Enregistrer le clavier
  {$IFDEF DELPHI}
  GetKeyBoardState(gt_Buffer);
  {$ENDIF}
end;

// Procédure qui sera déclenchée lorsqu'une touche sera tapée dans l'application
// En entrée : le message créé quelconque
function TF_FormMainIni.IsShortCut ( var ao_Msg: {$IFDEF FPC} TLMKey {$ELSE} TWMKey {$ENDIF} ) : Boolean;

begin
  Result := inherited IsShortCut ( ao_Msg );
  // Mise à jour des touches spéciales
  p_MiseAJourMajNumScroll ;
end;

//////////////////////////////////////////////////////////////////////////
// Procédure virtuelle : p_WriteDescendantIni
// Description : écriture de l'ini dans le descendant
//////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_WriteDescendantIni ( const amif_Init : TIniFile );
begin
End ;

//////////////////////////////////////////////////////////////////////////
// Procédure virtuelle : p_ReadDescendantIni
// Description : lecture de l'ini dans le descendant
//////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_ReadDescendantIni ( const amif_Init : TIniFile );
begin
End ;

// Fonction de gestion du fichier INI avec nom de connexion (le nom de l'exe)
// Entrée : Le nom de la connexion qui en fait est le nom du fichier INI (en gros)
// Renvoie un fichier INI (même si c'est pas très utile) !!!
function TF_FormMainIni.f_IniGetConfigFile: TIniFile;
begin
  p_WriteDescendantIni ( FIniMain );
  if assigned ( ge_WriteMainIni ) Then
    ge_WriteMainIni ( Self, FIniMain );
  Result := FIniMain;
end;

// Fonction de gestion du fichier INI avec nom de connexion (le nom de l'exe)
// Entrée : Le nom de la connexion qui en fait est le nom du fichier INI (en gros)
// Renvoie un fichier INI (même si c'est pas très utile) !!!
// Init. du fichier INI lié à l'utilisateur
function TF_FormMainIni.f_GetIniFile: TIniFile;
begin
  Result := f_GetMainMemIniFile(ge_WriteSessionIni, ge_ReadSessionIni, Self);
  // Lit-on le fichier ini par la prcoédure virtuelle ?
  p_InitialisationParamIni;

  // Sauvegarde du fichier INI
//  fb_iniWriteFile ( Result, False );
end;

// Termine l'appli sans sauver le fichier INi
procedure TF_FormMainIni.p_TerminateWithoutIni ;
Begin
  FiniFile.Free;
  FiniFile := nil;
  Application.Terminate;
End ;

// Initialisation du fichier ini
procedure TF_FormMainIni.p_InitialisationParamIni;
begin
// procédure réécrite dans le fils
end;

// Sauvegarde ini
procedure TF_FormMainIni.p_SauvegardeParamIni;
begin
// procédure réécrite dans le fils
end;

// Après la sauvegarde ini
procedure TF_FormMainIni.p_ApresSauvegardeParamIni;
begin
// procédure réécrite dans le fils
end;

 // Gestion du clavier
 // Entrée : les trois touches : MAJ NUM SCROLLLOCK
procedure TF_FormMainIni.p_SortieMajNumScroll ( const ab_MajEnfoncee, ab_NumEnfoncee, ab_ScrollEnfoncee : boolean ) ;
begin
// procédure réécrite dans le fils
end;

procedure TF_FormMainIni.DoShow;
begin
  inherited DoShow;
  f_GetMemIniFile;
  p_IniOuvre;
end;

// Met à jour la procédure virtuelle
procedure TF_FormMainIni.p_MiseAJourMajNumScroll;
Begin
  // Procédure virtuelle appelée
  p_SortieMajNumScroll(fb_GetKeyState(VK_CAPITAL),
                       fb_GetKeyState(VK_NUMLOCK),
                       fb_GetKeyState(VK_SCROLL));
End ;

procedure TF_FormMainIni.p_CloseQueryChildForms ( const ab_Free : Boolean );
var lw_i : Word ;
begin
  gb_CloseQuery := True ;
  for lw_i := Application.ComponentCount - 1 downto 0 do
    begin
      if (Application.Components[lw_i] is TCustomForm) and
         (Application.Components[lw_i] <> Self) then
          if (Application.Components[lw_i] as TCustomForm).CloseQuery
           Then
            Begin
              if ab_Free
               Then
                Application.Components[lw_i].Free;
            End
          Else
            Begin
              MessageDlg ( GS_DECONNECTER_ANNULE, mtInformation, [ mbOK ], 0 );
              Abort ;
            End ;
    end;
  gb_CloseQuery := False ;
end;

function TF_FormMainIni.CloseQuery: Boolean;
begin
  gb_CloseQuery := True ;
  Result := inherited CloseQuery ;
  p_CloseQueryChildForms ( False );
  gb_CloseQuery := Result ;
end;

procedure TF_FormMainIni.p_modalEnded(Aobj_Objet: Tobject);
begin
  gb_ModalStarted := False ;
end;

procedure TF_FormMainIni.p_modalStart(Aobj_Objet: Tobject);
begin
  gb_ModalStarted := True ;
end;


////////////////////////////////////////////////////////////////////////////////
//  En cas de problème sur la base de données
////////////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_NoConnexion;
begin
  MessageDlg(GS_PB_CONNEXION, mtWarning, [mbOk], 0);
  // Méthode virtuelle
  p_PbConnexion;
end;

//////////////////////////////////////////////////////////////////////////
// Non connecté
procedure TF_FormMainIni.p_PbConnexion;
begin
// procédure réécrite dans le fils
end;

// Connecté
procedure TF_FormMainIni.p_Connectee;
begin
// procédure réécrite dans le fils
end;



initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_TFormMainIni );
{$ENDIF}
finalization
  p_FreeConfigFile;
end.
