unit fonctions_dbcomponents;

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
  DB, Controls,
  {$IFDEF EADO}
    ADODB,
  {$ENDIF}
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  fonctions_db,
  Classes ;


const
  {$IFDEF VERSIONS}
  gVer_fonctions_db_components : T_Version = ( Component : 'Gestion des données d''une fiche' ;
                                         FileUnit : 'fonctions_dbcomponents' ;
      			                 Owner : 'Matthieu Giroux' ;
      			                 Comment : 'Fonctions gestion des données avec les composants visuels.' ;
      			                 BugsStory : 'Version 1.1.3.1 : Filter consts.' + #13#10 +
                                                     'Version 1.1.3.0 : Auto Commit.' + #13#10 +
                                                     'Version 1.1.2.0 : Clone ibquery with sql.' + #13#10 +
                                                     'Version 1.1.1.0 : Clone query with sql.' + #13#10 +
                                                     'Version 1.1.0.2 : Too many code on fb_inserecompteur, creating functions.' + #13#10 +
                                                     'Version 1.1.0.1 : Simplify function fb_InsereCompteur.' + #13#10 +
                                                     'Version 1.1.0.0 : Ajout de fonctions d''automatisation.' + #13#10
                                                   + 'Version 1.0.0.0 : Gestion des donnÃ©es rétilisable.';
      			                 UnitType : 1 ;
      			                 Major : 1 ; Minor : 1 ; Release : 3 ; Build : 1 );

  {$ENDIF}
  CST_CONNECTED   = 'Connected';
  CST_DATASET_FILTER      = 'Filter' ;
  CST_DATASET_FILTERED    = 'Filtered' ;

procedure p_setComponentData ( const acon_Control : TControl ; const ads_Source : TDataSource ; const afn_DataField : String );
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


procedure p_setComponentData ( const acon_Control : TControl ; const ads_Source : TDataSource ; const afn_DataField : String );
Begin
  p_setComponentProperty       ( acon_Control, 'DataField' , afn_DataField );
  p_setComponentObjectProperty ( acon_Control, 'Datasource', ads_Source );
  writeln ( 'fieldw:'+afn_DataField);
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_db_components );
{$ENDIF}
end.
