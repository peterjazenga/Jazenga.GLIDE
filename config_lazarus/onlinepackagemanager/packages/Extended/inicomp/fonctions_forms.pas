unit fonctions_forms;
// Unité de la Version 2 du projet FormMain
// La version 1 TFormMain n'est pas sa fenêtre parente

// Le module crée des propriété servant à la gestion du fichier INI
// Il gère la déconnexion
// Il gère la gestion des touches majuscules et numlock
// Il gère les forms enfants
// créé par Matthieu Giroux en décembre 2007

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
{$IFDEF SFORM}
  CompSuperForm,
{$ENDIF}
  {$IFDEF EADO}
     ADODB,
  {$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF TNT}
  TNTForms,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$IFDEF FPC}
  LCLIntf, LCLType,
{$ELSE}
  Windows, OleDb, Messages,
{$ENDIF}
  Dialogs, ExtCtrls, fonctions_ini;

{$IFDEF VERSIONS}
  const
    gVer_fonctions_forms : T_Version = (  Component : 'Gestion de Fenêtres' ;
                                       FileUnit : 'fonctions_forms' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Fiche principale deuxième version.' ;
                                       BugsStory : '1.0.1.1 : Simplfying' +#13#10+
                                                   '1.0.1.0 : Resizing from OS' +#13#10+
                                                   '1.0.0.0 : Windows management from FormMainIni.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 0 ; Release : 1 ; Build : 1 );

{$ENDIF}

{$IFDEF FPC}
function ActiveMDIChild : TCustomForm;
procedure WindowMinimizeAll(Sender: TObject);
{$ENDIF}
function fb_ReinitWindow ( var afor_Form : TCustomForm ) : Boolean ;

function ffor_CreateForm ( const afor_FormClasse : TFormClass ) : TCustomForm;
function ffor_FindForm ( const as_FormNom : string ) : TCustomForm;
procedure p_CloseForm ( const as_FormNom : string );

procedure p_SetChildForm ( const afor_Reference: TCustomForm; const  afs_newFormStyle : TFormStyle );

function ffor_getForm   ( const as_FormNom, as_FormClasse : string  ): TForm; overload ;

function ffor_getForm   ( afor_FormClasse : TFormClass ): TForm; overload ;

// Création d'une form MDI avec changement du style Form
// renvoie True si la form existe
// as_FormNom : Nom de la form ; afor_FormClasse : Classe de la form ; var afor_Reference : Variable de la form
function ffor_CreateUniqueChild ( afor_FormClasse : TFormClass; const newFormStyle : TFormStyle; const ab_Ajuster : Boolean = True ; const aico_Icon : TIcon = nil): TCustomForm;
// Création d'une form MDI renvoie True si la form existe
// as_FormNom : Nom de la form ; afor_FormClasse : Classe de la form ; var afor_Reference : Variable de la form
function fp_CreateUniqueChild(const afor_FormClasse : TFormClass ; const newFormStyle: TFormStyle; const ab_Ajuster: Boolean = True ; const aico_Icon : TIcon = nil): Pointer; overload;

// Création d'une form modal
// renvoie True si la form existe
// afor_FormClasse : Classe de la form ;
// var afor_Reference : Variable de la form
// ab_Ajuster : Ajuster automatiquement
// aact_Action : Action à la Fermeture
function fb_CreateModal ( afor_FormClasse : TFormClass ; var afor_Reference : TForm ; const ab_Ajuster : Boolean  ; const aact_Action : TCloseAction ) : Boolean ;

// changement du style d'une form
// afor_Reference    : variable de la form
// newFormStyle      : style    de la form à mettre
function fb_setNewFormStyle ( const afor_Reference : TCustomForm; const afs_newFormStyle : TFormStyle; const ab_Ajuster : Boolean = True ): Boolean ; overload ;
function fb_setNewFormStyle ( const afor_Reference : TCustomForm; const afs_FormStyle: TFormStyle ; const ab_Modal : Boolean ; const awst_WindowState : TWindowState ; const apos_Position : TPosition ): Boolean; overload ;

var
  gb_ModalStarted : Boolean;


implementation

uses fonctions_proprietes, fonctions_erreurs, TypInfo,
  fonctions_system,
  fonctions_scaledpi,
  U_FormAdapt;

{ fonctions }



function ffor_CreateForm ( const afor_FormClasse : TFormClass ) : TCustomForm;
Begin
  Application.CreateForm ( afor_FormClasse, Result );
  if not ( Result is TF_FormAdapt ) Then
   Begin
    ScaleFormCreate(Result,ge_GlobalScaleForm);
    ScaleFormShow  (Result,ge_GlobalScaleForm);
   end;
end;

{------------------------------------------------------------------------------
 ---------------------- Fin Hook clavier pour le maj et le num ----------------
 ------------------------------------------------------------------------------}

    // Création d'une form MDI renvoie  la form qui existe
function ffor_FindForm ( const as_FormNom : string ) : TCustomForm;

var i : integer;
begin
  Result := nil;
  // Cette recherche ne fonctionne qu'avec les forms mdi child
  for i := 0 to Application.ComponentCount - 1 do
    if ( Application.Components [ i ] is TCustomForm )
    and ( Application.Components [ i ].Name = as_FormNom ) then
     Begin
      Result := Application.Components [ i ] as TCustomForm;
      Break;
     end;

end;

procedure p_CloseForm(const as_FormNom: string);
var
 lfor_Reference : TCustomForm;
begin
  lfor_Reference := ffor_FindForm ( as_FormNom );

  if lfor_Reference <> nil then
    lfor_Reference.Free;
end;


// Création d'une form MDI renvoie True si la form existe dans les enfants MDI
// as_FormNom        : nom      de la form
// afor_FormClasse   : classe   de la form
// afor_Reference    : variable de la form
// newFormStyle      : style    de la form à mettre
function ffor_CreateUniqueChild ( afor_FormClasse : TFormClass; const newFormStyle : TFormStyle; const ab_Ajuster : Boolean = True ; const aico_Icon : TIcon = nil): TCustomForm;
var
  li_i : integer;
  lico_icon : Ticon ;
begin
  Result := nil ;
    // Recherche sûre de fiches quelconques
  For li_i := Application.ComponentCount - 1 downto 0
   do if (  Application.Components [ li_i ] is TCustomForm )
     and (( Application.Components [ li_i ] as TCustomForm ).ClassType = afor_FormClasse )
    Then
      Begin
        Result := TCustomForm ( Application.Components [ li_i ]);
      End ;

      //Création si nil
  if ( Result = nil )
    Then
      Result := ffor_CreateForm ( afor_FormClasse );
  If  assigned ( aico_Icon      )
  and assigned ( Result )
  and (fobj_getComponentObjectProperty ( Result, 'Icon' ) is TIcon )
   Then
    Begin
      lico_icon := TIcon ( fobj_getComponentObjectProperty ( Result, 'Icon' ));
      if assigned ( lico_icon ) then
        Begin
          lico_icon.Modified := False ;
          lico_icon.PaletteModified := False ;
          if lico_icon.Handle <> 0 Then
            Begin
              lico_icon.ReleaseHandle ;
              lico_icon.CleanupInstance ;
            End ;
          lico_icon.Handle := 0 ;
          lico_icon.Assign ( aico_Icon );
          lico_icon.Modified := True ;
          lico_icon.PaletteModified := True ;

          Result.Invalidate ;

        End;
    End ;
    // Mise à jour de la form
  if Assigned(Result)
  and ab_Ajuster
  and ( Result is TCustomForm ) then
    fb_setNewFormStyle ( Result as TCustomForm , newFormStyle, ab_Ajuster );
end;



// procedure p_SetChildForm
// Setting FormStyle
// If using SuperForm, setting the child superform if not FormStyle is fsStayOnTop
// afor_Reference : Form Variable
// The style to set afs_newFormStyle
procedure p_SetChildForm(const afor_Reference: TCustomForm; const  afs_newFormStyle : TFormStyle );
var FBoxChilds:TWinControl;
const CST_BoxChilds = 'BoxChilds';
begin
{$IFDEF SFORM}
 if ( afor_Reference is TSuperForm )
  and not ( afs_newFormStyle in [fsStayOnTop]) Then
    with Application,MainForm do
    Begin
      BeginUpdateBounds;
    // Must use MainFormIni
    //       afor_Reference.AutoSize := True;
      ( afor_Reference as TSuperForm ).IncrustMode := aicAllClient;
      if   assigned ( GetPropInfo ( MainForm, CST_BoxChilds ))
      and  PropIsType      ( MainForm, CST_BoxChilds , tkClass) Then
        Begin
          FBoxChilds := TWinControl ( GetObjectProp   ( MainForm, CST_BoxChilds ));
          if not assigned ( FBoxChilds ) Then
            Begin
              FBoxChilds := TSuperBox.Create(MainForm);
              with FBoxChilds as TSuperBox do
                Begin
                  Parent := MainForm;
                  AutoScroll:=True;
                  Align:=alClient;
                end;
              SetObjectProp( MainForm, CST_BoxChilds, FBoxChilds );
            end;
    //       afor_Reference.Align := alClient;
         with FBoxChilds do
           Begin
             BeginUpdateBounds;
             with afor_Reference as TSuperForm do
               Begin
                ShowIncrust ( FBoxChilds );
               end;
             EndUpdateBounds;
           end;
        end
       Else
        ( afor_Reference as TSuperForm ).ShowIncrust ( MainForm );
       EndUpdateBounds;
     end
   else
{$ENDIF}
  p_SetComponentProperty ( afor_Reference, 'FormStyle', afs_newFormStyle );
  {$IFDEF FPC}
  afor_Reference.Show;
  {$ENDIF}
  afor_Reference.BringToFront ;
end;

// Création d'une form MDI renvoie True si la form existe dans les enfants MDI
// as_FormNom        : nom      de la form
// afor_FormClasse   : classe   de la form
// newFormStyle      : style    de la form à mettre
function fp_CreateUniqueChild(const afor_FormClasse : TFormClass ; const newFormStyle: TFormStyle; const ab_Ajuster: Boolean = True ; const aico_Icon : TIcon = nil): Pointer;
var lb_Unload : Boolean;
Begin
  Result := ffor_getForm ( afor_FormClasse );

      //Création si nil
 if ( Result = nil )
   Then Result := TForm ( ffor_CreateForm ( afor_FormClasse ));
    // Mise à jour de la form

 if Assigned(Result)
 and ab_Ajuster then
   Begin
     lb_Unload := fb_getComponentBoolProperty ( TComponent( Result ), 'DataUnload' );
     if not lb_Unload Then
       fb_setNewFormStyle( TForm ( Result ), newFormStyle, ab_Ajuster)
     else
       ( TForm ( Result )).Free ;
   End ;
end;

// Création d'une form modal
// renvoie True si la form existe
// afor_FormClasse : Classe de la form ;
// var afor_Reference : Variable de la form
// ab_Ajuster : Ajuster automatiquement
// aact_Action : Action à la Fermeture
function fb_CreateModal ( afor_FormClasse : TFormClass ; var afor_Reference : TForm ; const ab_Ajuster : Boolean  ; const aact_Action : TCloseAction ) : Boolean ;
begin
  Result := false ;
  afor_Reference := ffor_getForm ( afor_FormClasse );

      //Création si nil
 if ( afor_Reference = nil )
   Then
     afor_Reference := TForm ( ffor_CreateForm ( afor_FormClasse ))
   Else
    Result := True ;
    // Mise à jour de la form

  afor_Reference.FormStyle := fsNormal ;

  afor_Reference.Hide ;
  if ab_Ajuster
   Then
    Begin
      afor_Reference.Position    := poMainFormCenter ;
      afor_Reference.WindowState := wsNormal ;
      afor_Reference.BorderStyle := bsSingle ;
    End ;
  afor_Reference.Update ;
  afor_Reference.ShowModal;
  // On peut effectuer une action de fermeture après avoir montré une fiche modale
  if aact_Action = caFree
   then
    afor_Reference.Free
   else if aact_Action = caHide
    then
     afor_Reference.Hide
    else if aact_Action = caMiniMize
     then
      afor_Reference.WindowState := wsMiniMized ;
end;

// Récupération d'une form renvoie la form si existe dans les enfants
// as_FormNom        : nom      de la form
// as_FormClasse   : classe   de la form
function ffor_getForm ( const as_FormNom, as_FormClasse: string ): TForm ;
var
  li_i: integer;

begin
  // Initialisation
  Result          := nil ;
//  if (FormStyle <> fsMDIForm) then
//    Exit;

  for li_i := Application.ComponentCount - 1 downto 0 do
    if (Application.Components[li_i] is TForm) and
       ( lowercase (( Application.Components[li_i] as TForm).ClassName ) = lowercase ( as_FormClasse )) then
      begin
        Result := TForm ( Application.Components[li_i] );
      end;
End ;

// Récupération d'une form renvoie la form si existe dans les enfants
// as_FormNom        : nom      de la form
// as_FormClasse   : classe   de la form
function ffor_getForm ( afor_FormClasse : TFormClass ): TForm;
var
  li_i: integer;

begin
  Result := nil ;
    // Recherche sûre de fiches quelconques
  For li_i := Application.ComponentCount - 1 downto 0
   do if (  Application.Components [ li_i ] is TForm )
     and (( Application.Components [ li_i ] as TForm ).ClassType = afor_FormClasse )
    Then
      Begin
        Result := TForm ( Application.Components [ li_i ] );
      End ;
End ;

// Changement du style d'une form
// afor_Reference    : variable de la form
// newFormStyle      : style    de la form à mettre
// Résultat          : Le style a été changé
function fb_setNewFormStyle(const afor_Reference: TCustomForm; const afs_FormStyle: TFormStyle ; const ab_Modal : Boolean ; const awst_WindowState : TWindowState ; const apos_Position : TPosition ): Boolean;
begin
  Result := False ;
  if not ( assigned ( afor_Reference )) then
    Exit ;
  try
    // Le style a été changé
    Result := True ;

    if TPosition ( fli_getComponentProperty ( afor_Reference, 'Position' )) <> apos_Position Then
      p_SetComponentProperty ( afor_Reference, 'Position', apos_Position );
    if TWindowState ( fli_getComponentProperty ( afor_Reference ,'WindowState' )) <> awst_WindowState Then
      p_SetComponentProperty ( afor_Reference, 'WindowState', awst_WindowState );

    if not ( afs_FormStyle in [ fsMDIChild ]) Then
      p_SetComponentProperty ( afor_Reference, 'FormStyle', afs_FormStyle );

    // Mise à jour
    afor_Reference.Update ;

    // Affectation
    if ab_Modal
    and ( afs_FormStyle in [ fsNormal ]) Then
      begin
        afor_Reference.ShowModal ;
        Exit ;
      end ;

      // Affiche la fiche après les modifs
    if ( afs_FormStyle in [fsMDIChild]) Then
      p_setChildForm ( afor_Reference, afs_FormStyle )
    Else
      afor_Reference.Show;
  Except
  End ;
End ;

// Changement du style d'une form
// afor_Reference    : variable de la form
// newFormStyle      : style    de la form à mettre
// Résultat          : Le style a été changé
function fb_setNewFormStyle(const afor_Reference: TCustomForm; const afs_newFormStyle: TFormStyle; const ab_Ajuster: Boolean = True): Boolean;
//var acla_ClasseForm : TClass ;
begin
  Result := False;
  try
  //  acla_ClasseForm := afor_Reference.ClassType ;
    // Style différent
    if (afs_newFormStyle <> TFormStyle ( fli_getComponentProperty ( afor_Reference, 'FormStyle' ))) then
      begin
        // Le style a été changé
        Result := True ;

        // Affectation
        if gb_ModalStarted
        and ( afs_newFormStyle in [fsMDIChild, fsNormal ]) Then
          begin
            if TPosition ( fli_getComponentProperty ( afor_Reference , 'Position' )) <> poMainFormCenter Then
              p_SetComponentProperty ( afor_Reference, 'Position', poMainFormCenter );
            if TWindowState ( fli_getComponentProperty ( afor_Reference , 'WindowState' )) <> wsNormal Then
              p_SetComponentProperty ( afor_Reference, 'WindowState', wsNormal );
            afor_Reference.ShowModal ;
            Exit ;
          end
        Else
          if not ( afs_newFormStyle in [fsMDIChild]) Then
            p_SetComponentProperty ( afor_Reference, 'FormStyle', afs_newFormStyle );
      end;

    {$IFNDEF SFORM}
      // Option ajuster
    if ab_Ajuster
    and Result   then
      begin
      // Par dessus donc au centre
        if ( TFormStyle ( fli_getComponentProperty ( afor_Reference, 'FormStyle' )) = fsStayOnTop)
        and (    (TWindowState ( fli_getComponentProperty ( afor_Reference , 'WindowState' )) <> wsNormal         )
              or ( TPosition ( fli_getComponentProperty ( afor_Reference , 'Position' ))    <> poMainFormCenter )) then
          begin
            p_SetComponentProperty ( afor_Reference, 'Position', poMainFormCenter );
            p_SetComponentProperty ( afor_Reference, 'WindowState', wsNormal );
          end;

          // MDI enfant donc maximizée
        if  not gb_ModalStarted and ( afs_newFormStyle = fsMDIChild) then
          p_SetComponentProperty ( afor_Reference, 'WindowState', wsMaximized );
        // Mise à jour
        afor_Reference.Update ;
      end;
    {$ENDIF}

      // Affiche la fiche après les modifs
    if not gb_ModalStarted and ( afs_newFormStyle in [fsMDIChild]) Then
      p_setChildForm ( afor_Reference, afs_newFormStyle )
    Else
      afor_Reference.Show;
  Except
  End ;
end;


function fb_ReinitWindow(
  var afor_Form: TCustomForm): Boolean;
var lfs_FormStyle: TFormStyle ;
    lb_Modal : Boolean ;
    lwst_WindowState : TWindowState ;
    lcln_FormName   : String ;
    lico_Icone       : TIcon ;
    lpos_Position : Tposition ;
    lclt_ClassType : TClass ;
begin
  Result := False ;
  if  assigned ( FIniFile  )
  and assigned ( afor_Form ) Then
    Begin
      lclt_ClassType := afor_Form.ClassType ;
      lfs_FormStyle  := TFormStyle ( fli_getComponentProperty ( afor_Form ,'FormStyle' ));
      lcln_FormName  := afor_Form.Name ;
      lpos_Position  := TPosition ( fli_getComponentProperty ( afor_Form ,'Position' ));
      lwst_WindowState := afor_Form.WindowState ;
      lb_Modal := gb_ModalStarted ;
      lico_Icone := TIcon.Create ;
      if ( fobj_getComponentObjectProperty ( afor_Form, 'Icon' ) is TIcon ) then
        Begin
          lico_Icone.Assign ( TIcon ( fobj_getComponentObjectProperty ( afor_Form, 'Icon' )));
        End;
      try
        if afor_Form.CloseQuery  Then
          Begin
            afor_Form.Free ;
            afor_Form := Nil ;
            p_IniDeleteSection ( lcln_FormName );
            afor_Form := ffor_CreateUniqueChild ( TFormClass ( lclt_ClassType ), lfs_FormStyle, False, lico_Icone );
            if assigned ( afor_Form ) Then
              Begin
                fb_setNewFormStyle ( afor_Form, lfs_FormStyle, lb_Modal, lwst_WindowState, lpos_Position );
                Result := True ;
              End ;
          End ;

      finally
        if lico_Icone.HandleAllocated Then
          Begin
            lico_Icone.ReleaseHandle ;
          End ;
        lico_Icone.Free ;
      End ;
    End ;
end;

{$IFDEF FPC}
procedure WindowMinimizeAll(Sender: TObject);
var li_i : Integer ;
Begin
  for li_i := 0 to Application.ComponentCount -1 do
    Begin
      If  ( Application.Components[ li_i ] <> Application.MainForm )
      and ( Application.Components[ li_i ] is TCustomForm ) Then
        ( Application.Components[ li_i ] as TCustomForm ).WindowState := wsMinimized;
    End ;
End;

function ActiveMDIChild : TCustomForm;
var li_i : Integer ;
Begin
  Result := Application.MainForm ;
  for li_i := 0 to Application.ComponentCount -1 do
    Begin
      If  ( Application.Components[ li_i ] <> Application.MainForm )
      and ( Application.Components[ li_i ] is TCustomForm )
      and (( Application.Components[ li_i ] as TCustomForm ).Active ) Then
        Result := Application.Components[ li_i ] as TCustomForm ;
    End ;
End;
{$ENDIF}



initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_forms );
{$ENDIF}
end.
