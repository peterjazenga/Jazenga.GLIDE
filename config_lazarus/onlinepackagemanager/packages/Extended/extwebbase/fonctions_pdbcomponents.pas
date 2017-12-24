unit fonctions_pdbcomponents;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses SysUtils,
  {$IFDEF DELPHI_9_UP}
     WideStrings,
  {$ENDIF}
  {$IFDEF EADO}
    ADODB,
  {$ENDIF}
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  fonctions_db,
  Classes ;

type
  TOnExecuteQuery = procedure ( const adat_Dataset: TComponent );
  TOnSetQuery = procedure ( const adat_Dataset: TDataset ; const AOwner : TComponent);
  TOnOptimiseDatabase = function ( const AConnection : TComponent ;
                                   const as_database, as_user, as_password, APathSave : String ;
                                   const ASt_Messages : TStrings; const acom_ControlMessage, acom_owner : TComponent):Boolean;
  TOnOpenCloseDatabase = function ( const AConnection : TComponent ;
                               const ab_Open : Boolean ;
                               const ab_showError : Boolean  ):Boolean;
  TOnExecuteCommand = procedure ( const as_SQL: {$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF} );
  TOnExecuteScriptServer = procedure ( const AConnection : TComponent; const as_SQL: {$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF} );
  TSetConnectComponents = procedure ( const cbx_Protocol, ch_ServerConnect, ed_Base, ed_Host, ed_Password, ed_User, ed_Catalog, ed_Collation: TComponent );

  // database name to load
var gs_DefaultDatabase : String = '';


const
  {$IFDEF VERSIONS}
  gVer_fonctions_db_components : T_Version = ( Component : 'Gestion des données d''une fiche' ;
                                         FileUnit : 'fonctions_dbcomponents' ;
      			                 Owner : 'Matthieu Giroux' ;
      			                 Comment : 'Fonctions gestion des données avec les composants visuels.' ;
      			                 BugsStory : 'Version 1.1.3.0 : Auto Commit.' + #13#10 +
                                                     'Version 1.1.2.0 : Clone ibquery with sql.' + #13#10 +
                                                     'Version 1.1.1.0 : Clone query with sql.' + #13#10 +
                                                     'Version 1.1.0.2 : Too many code on fb_inserecompteur, creating functions.' + #13#10 +
                                                     'Version 1.1.0.1 : Simplify function fb_InsereCompteur.' + #13#10 +
                                                     'Version 1.1.0.0 : Ajout de fonctions d''automatisation.' + #13#10
                                                   + 'Version 1.0.0.0 : Gestion des donnÃ©es rétilisable.';
      			                 UnitType : 1 ;
      			                 Major : 1 ; Minor : 1 ; Release : 3 ; Build : 0 );

  {$ENDIF}
  CST_CONNECTED   = 'Connected';
  ge_OnExecuteQuery: TOnExecuteQuery = nil;
  ge_OnSetAutoCommit: TOnSetQuery = nil;
  ge_OnRefreshDataset : TSpecialFuncDataset = nil;
  ge_OnOpenOrCloseDatabase: TOnOpenCloseDatabase = nil;
  ge_OnOptimiseDatabase: TOnOptimiseDatabase = nil;
  ge_OnExecuteCommand: TOnExecuteCommand = nil;
  ge_OnExecuteScriptServer: TOnExecuteScriptServer = nil;
  ge_SetConnectComponentsOnCreate : TSetConnectComponents = nil;

procedure p_setComponentData ( const awin_Control : TWinControl ; const ads_Source : TDataSource ; const afn_DataField : String );
function fcon_CloneControlWithDB ( const acom_AObject : TControl ; const AOwner : TComponent ) : TControl;

implementation

uses Variants,  fonctions_erreurs, fonctions_string,
  {$IFNDEF FPC}
  DBTables,
{$ENDIF}
{$IFDEF EDBEXPRESS}
     SQLExpr,
 {$ENDIF}
   fonctions_proprietes, TypInfo,
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
   Dialogs, fonctions_components;

// cloning a control with data connection
function fcon_CloneControlWithDB ( const acom_AObject : TControl ; const AOwner : TComponent ) : TControl;
Begin
  Result:= fcon_CloneControl ( acom_AObject, AOwner );
  p_SetComponentObjectProperty( Result, CST_DBPROPERTY_DATASOURCE, fobj_getComponentObjectProperty(acom_AObject, CST_DBPROPERTY_DATASOURCE));
  p_SetComponentProperty      ( Result, CST_DBPROPERTY_DATAFIELD , fs_getComponentProperty(acom_AObject, CST_DBPROPERTY_DATAFIELD));
end;


procedure p_setComponentData ( const awin_Control : TWinControl ; const ads_Source : TDataSource ; const afn_DataField : String );
Begin
  p_setComponentProperty       ( awin_Control, 'DataField' , afn_DataField );
  p_setComponentObjectProperty ( awin_Control, 'Datasource', ads_Source );
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_db_components );
{$ENDIF}
end.
