unit fonctions_dbobjects;

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
  DB,
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


function fb_RefreshDataset ( const aDat_Dataset : TDataset ): Boolean ; overload;
function fb_RefreshDataset ( const aDat_Dataset : TDataset; const ab_GardePosition : Boolean ): Boolean ; overload;
procedure p_AutoConnection ( const adat_Dataset : TDataset; const AConnect : Boolean = True );
procedure p_OpenSQLQuery(const adat_Dataset: Tdataset; const as_Query : String );
function  fs_getSQLQuery ( const adat_Dataset : Tdataset ): String;
procedure p_SetSQLQuery(const adat_Dataset: TComponent; const as_Query : String ); overload;
procedure p_AddSQLQuery(const adat_Dataset: Tdataset; const as_Query : String );
procedure p_SetConnexion ( const acom_ADataset : TComponent ; acco_Connexion : TComponent );
procedure p_SetComponentsConnexions ( const acom_Form : TComponent ; acco_Connexion : TComponent );
function  fb_RefreshDatasetIfEmpty ( const adat_Dataset : TDataset ) : Boolean ;
procedure p_ExecuteSQLQuery ( const adat_Dataset : TComponent ; const as_Query :String ; const ab_ShowException : boolean = True );
procedure p_ExecuteSQLCommand ( const as_Command :{$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF} ; const ab_ShowException : boolean = True );
procedure p_optimiseDatabase ( const AConnection : TComponent;
                               const as_database, as_user, as_password, APathSave : String );
function fb_OptimiseDatabase  ( const AConnection : TComponent ;
                                const as_database, as_user, as_password, APathSave : String ;
                                const ASt_Messages : TStrings;
                                const acom_ControlMessage, acom_owner : TComponent):Boolean;
function fb_OpenCloseDatabase ( const AConnection  : TComponent ;
                                const ab_Open : Boolean ;
                                const ab_showError : Boolean = False   ):Boolean;
function fb_TestConnection ( const Connexion : TComponent ; const lb_ShowMessage : Boolean ) : Boolean;
procedure p_ExecuteSQLScriptServer ( const AConnection : TComponent; const as_Command :{$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF} ; const ab_ShowException : boolean = True );
function fdat_CloneDatasetWithoutSQL ( const adat_ADataset : TDataset ; const AOwner : TComponent ; const aautoCommit : Boolean = False ) : TDataset;
function fdat_CloneDatasetWithSQL ( const adat_ADataset : TDataset ; const AOwner : TComponent ; const aautoCommit : Boolean = False ) : TDataset;
function fdat_CloneDatasetWithoutSQLWithDataSource ( const adat_ADataset : Tdataset ; const AOwner : TComponent ; var ads_Datasource : TDatasource  ) : Tdataset;
function fds_GetOrCloneDataSource ( const acom_Component : TComponent ; const as_SourceProperty, as_Query : String ; const AOwner : TComponent ; const adat_ADatasetToCopy : Tdataset ) : Tdatasource;
function fb_GetSQLStrings (const adat_ADataset : TComponent ; var astl_SQLQuery : TStrings{$IFDEF DELPHI_9_UP}; var awst_SQLQuery : TWideStrings {$ENDIF}):Boolean;
function fcom_CloneConnexion ( const acco_AObject : TComponent ; const AOwner : TComponent ) : TComponent;
function fb_GetParamsDataset (const adat_ADataset : TComponent ; var aprs_ParamSource: TParams ; var Astl_Params : TStrings {$IFDEF EADO} ; var aprs_ParamterSource: TParameters {$ENDIF}): Boolean;
function fb_SetParamQuery(const adat_Dataset : TDataset ; const as_Param: String): Boolean;
function fb_LocateSansFiltre ( const aado_Seeker : TDataset ; const as_Fields : String ; const avar_Records : Variant ; const ach_Separator : Char ): Boolean ;
procedure p_LocateInit ( const aado_Seeker : TDataset ; const as_Table, as_Fields, as_Condition : String );
function fb_AssignSort ( const adat_Dataset : TDataset ; const as_ChampsOrdonner : String ):Boolean; overload;
function fb_AssignSort ( const adat_Dataset : TDataset ; const astl_list : TStrings ; const ai_ChampsOrdonner : Integer ):Boolean; overload;
function fb_DatasetFilterLikeRecord ( const as_DatasetValue, as_FilterValue : String ; const ab_CaseInsensitive : Boolean ): Boolean ;
procedure p_setParamDataset (const adat_ADataset : Tdataset ; const as_ParamName : String ; const avar_Value : Variant );
function fb_FieldRecordExists  ( const adat_Dataset, adat_DatasetQuery : TDataset ;
                               const as_Table, as_FieldName : String;
                               const ab_DBMessageOnError  : Boolean ): Boolean;
function fb_RecordExists ( const adat_DatasetQuery : TDataset ;
                           const as_Table, as_Where : String;
                           const ab_DBMessageOnError  : Boolean ): Boolean;
procedure p_ShowSQLError ( const AException, ASQL : String );
procedure p_SetAutoCommit ( const AQuery : TDataset ; const AOwner : TComponent );

var ge_DataSetErrorEvent : TDataSetErrorEvent ;
    gch_SeparatorCSV: Char = ';';


implementation

uses Variants,  fonctions_string,
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
  fonctions_dialogs,
  fonctions_objects;

// Set Auto Commit
procedure p_SetAutoCommit ( const AQuery : TDataset ; const AOwner : TComponent );
Begin
  if assigned (ge_OnSetAutoCommit) Then
   ge_OnSetAutoCommit ( AQuery, AOwner );
end;

// Show db error
procedure p_ShowSQLError ( const AException, ASQL : String );
Begin
  MyShowMessage ( AException + ':' +#13#10+ ASQL );
End;

// getting sql from query
function fb_GetSQLStrings (const adat_ADataset : TComponent ; var astl_SQLQuery : TStrings{$IFDEF DELPHI_9_UP}; var awst_SQLQuery : TWideStrings {$ENDIF}):Boolean;
Begin
  Result := fb_GetStrings(adat_ADataset,CST_DBPROPERTY_SQL,astl_SQLQuery{$IFDEF DELPHI_9_UP}, awst_SQLQuery {$ENDIF});
end;

// execute query with optional module
procedure p_ExecuteSQLQuery ( const adat_Dataset : TComponent ; const as_Query :String ; const ab_ShowException : boolean = True );
Begin
  p_SetSQLQuery ( adat_Dataset, as_Query );
  try
    if assigned ( ge_OnExecuteQuery ) Then
     ge_OnExecuteQuery ( adat_Dataset );
  Except
    on E:Exception do
      if ab_ShowException Then
      p_ShowSQLError(E.Message,as_Query);
  end;
End ;

//////////////////////////////////////////////////////////////////////
// Fonction retournant la connexion copiée avec le lien SGBD
//  acco_AObject : La connexion à cloner
//  AOwner       : Le futur propriétaire du composant
// Résultat de la fonction : la connexion copiée avec le lien SGBD
//////////////////////////////////////////////////////////////////////
function fcom_CloneConnexion ( const acco_AObject : TComponent ; const AOwner : TComponent ) : TComponent;
Begin
  Result := TComponent(fcom_CloneObject ( acco_AObject, AOwner ));
  // ADO
  if  VarIsStr( fvar_getComponentProperty(acco_AObject,CST_DBPROPERTY_CONNECTIONSTRING)  )  Then
    Begin
      p_SetComponentProperty( Result, CST_DBPROPERTY_CONNECTIONSTRING, fvar_getComponentProperty(acco_AObject,CST_DBPROPERTY_CONNECTIONSTRING));
    End ;
  if  ( fobj_getComponentObjectProperty(acco_AObject,CST_DBPROPERTY_CONNECTION) <> nil )  Then
    Begin
      p_SetComponentObjectProperty( Result, CST_DBPROPERTY_CONNECTION, fobj_getComponentObjectProperty(acco_AObject,CST_DBPROPERTY_CONNECTION));
    End ;

End;

//////////////////////////////////////////////////////////////////////
// Fonction retournant le dataset copié avec le lien SGBD et sa requête copiée
//  adat_AObject : Le dataset à cloner
//  AOwner       : Le futur propriétaire du composant
// Résultat de la fonction : le dataset copié avec le lien SGBD
//////////////////////////////////////////////////////////////////////
function fdat_CloneDatasetWithSQL ( const adat_ADataset : TDataset ; const AOwner : TComponent ; const aautoCommit : Boolean = False ) : TDataset;
var AStrings : TStrings;
    li_i : Integer;
   aprs_ParamSource: TParams ;
   Astl_Params : TStrings;
   {$IFDEF EADO}aprs_ParamterSource: TParameters ;{$ENDIF}
   {$IFDEF DELPHI_9_UP}awst_SQLQuery : TWideStrings; {$ENDIF}
Begin
  Result := fdat_CloneDatasetWithoutSQL(adat_ADataset,AOwner,aautoCommit);
  AStrings := nil;
  Astl_Params := nil;
  {$IFDEF EADO}
  aprs_ParamterSource := nil;
  {$ENDIF}
  if fb_GetSQLStrings(adat_ADataset,AStrings{$IFDEF DELPHI_9_UP}, awst_SQLQuery {$ENDIF})
   Then if Assigned ( AStrings )
    then p_SetSQLQuery(Result,AStrings.Text)
    {$IFDEF DELPHI_9_UP}Else p_SetSQLQuery(Result,awst_SQLQuery.Text){$ENDIF};
  if fb_GetParamsDataset(adat_ADataset,aprs_ParamSource,Astl_Params{$IFDEF EADO},aprs_ParamterSource{$ENDIF}) Then
   Begin
     if Assigned(aprs_ParamSource) Then
       for li_i := 0 to aprs_ParamSource.Count - 1 do
         with aprs_ParamSource [ li_i ] do
          p_setParamDataset(Result,Name,Value)
     {$IFDEF EADO}
     Else
       if Assigned(aprs_ParamterSource) Then
         for li_i := 0 to aprs_ParamterSource.Count - 1 do
           with aprs_ParamterSource [ li_i ] do
            p_setParamDataset(Result,Name,Value)
     {$ENDIF};
   end;

end;

//////////////////////////////////////////////////////////////////////
// Fonction retournant le dataset copié avec le lien SGBD
//  adat_AObject : Le dataset à cloner
//  AOwner       : Le futur propriétaire du composant
// Résultat de la fonction : le dataset copié avec le lien SGBD
//////////////////////////////////////////////////////////////////////
function fdat_CloneDatasetWithoutSQL ( const adat_ADataset : TDataset ; const AOwner : TComponent ; const aautoCommit : Boolean = False ) : TDataset;
var aobj_Update : TObject;
Begin
  Result := TDataset(fcom_CloneObject ( adat_ADataset, AOwner ));

  with Result do
   Begin
     AfterPost     :=adat_ADataset.AfterPost;
     AfterCancel   :=adat_ADataset.AfterCancel;
     AfterClose    :=adat_ADataset.AfterClose;
     AfterDelete   :=adat_ADataset.AfterDelete;
     AfterEdit     :=adat_ADataset.AfterEdit;
     AfterInsert   :=adat_ADataset.AfterInsert;
     AfterOpen     :=adat_ADataset.AfterOpen;
     AfterRefresh  :=adat_ADataset.AfterRefresh;
     AfterScroll   :=adat_ADataset.AfterScroll;
     BeforePost    :=adat_ADataset.BeforePost;
     BeforeCancel  :=adat_ADataset.BeforeCancel;
     BeforeClose   :=adat_ADataset.BeforeClose;
     BeforeDelete  :=adat_ADataset.BeforeDelete;
     BeforeEdit    :=adat_ADataset.BeforeEdit;
     BeforeInsert  :=adat_ADataset.BeforeInsert;
     BeforeOpen    :=adat_ADataset.BeforeOpen;
     BeforeRefresh :=adat_ADataset.BeforeRefresh;
     BeforeScroll  :=adat_ADataset.BeforeScroll;
   end;

  p_SetConnexion ( Result, fobj_getComponentObjectProperty(adat_ADataset,CST_DBPROPERTY_CONNECTION) as TComponent);
  // ADO
  if  assigned ( GetPropInfo ( adat_ADataset, CST_DBPROPERTY_CONNECTIONSTRING ))  Then
    Begin
      p_SetComponentProperty( Result, CST_DBPROPERTY_CONNECTIONSTRING, fvar_getComponentProperty(adat_ADataset,CST_DBPROPERTY_CONNECTIONSTRING));
    End ;

  // LAZARUS
  if  assigned ( GetPropInfo ( adat_ADataset,CST_DBPROPERTY_SQLCONNECTION))  Then
    Begin
      p_SetComponentObjectProperty( Result, CST_DBPROPERTY_SQLCONNECTION, fobj_getComponentObjectProperty(adat_ADataset,CST_DBPROPERTY_SQLCONNECTION));
    End ;
  // DB NET PROCESSOR
  if  assigned ( GetPropInfo ( adat_ADataset,CST_DBPROPERTY_ONLINECONN))  Then
    Begin
      p_SetComponentObjectProperty( Result, CST_DBPROPERTY_ONLINECONN, fobj_getComponentObjectProperty(adat_ADataset,CST_DBPROPERTY_ONLINECONN));
    End ;
  // DBEXPRESS IBX
  if  assigned ( GetPropInfo ( adat_ADataset,CST_DBPROPERTY_DATABASE))  Then
    Begin
      p_SetComponentObjectProperty( Result, CST_DBPROPERTY_DATABASE, fobj_getComponentObjectProperty(adat_ADataset,CST_DBPROPERTY_DATABASE));
    End ;
  // bDE
  if assigned ( GetPropInfo ( adat_ADataset,CST_DBPROPERTY_DATABASENAME))  Then
    Begin
      p_SetComponentProperty( Result, CST_DBPROPERTY_DATABASENAME, fvar_getComponentProperty(adat_ADataset,CST_DBPROPERTY_DATABASENAME));
    End ;
  if assigned ( GetPropInfo ( adat_ADataset,CST_DBPROPERTY_SESSIONNAME))  Then
    Begin
      p_SetComponentProperty( Result, CST_DBPROPERTY_SESSIONNAME, fvar_getComponentProperty(adat_ADataset,CST_DBPROPERTY_SESSIONNAME));
    End ;
  if assigned ( GetPropInfo ( adat_ADataset,CST_DBIBPROPERTY_AutoTrans))  Then
    Begin
      p_SetComponentProperty( Result, CST_DBIBPROPERTY_AutoTrans, fvar_getComponentProperty(adat_ADataset,CST_DBIBPROPERTY_AutoTrans));
    End ;
  aobj_Update := fobj_getComponentObjectProperty( adat_ADataset,CST_DBPROPERTY_UPDATEOBJECT);
  if assigned ( aobj_Update ) Then
    Begin
      p_SetComponentObjectProperty( Result, CST_DBPROPERTY_UPDATEOBJECT, fcom_CloneObject(aobj_Update as TComponent,AOwner));
    End ;
  p_SetComponentBoolProperty( Result, 'ReadOnly', False );
  p_SetComponentBoolProperty( Result, 'AutoCalcFields', True );

  if aautoCommit Then
   Begin
     p_SetAutoCommit(Result,AOwner);
   End
  else
    // IBX
    if assigned ( GetPropInfo ( adat_ADataset,CST_DBPROPERTY_TRANSACTION))  Then
      Begin
        p_SetComponentObjectProperty( Result, CST_DBPROPERTY_TRANSACTION, fobj_getComponentObjectProperty(adat_ADataset,CST_DBPROPERTY_TRANSACTION));
      End ;

End ;

// Cloning a datasource with SQL
function fds_GetOrCloneDataSource ( const acom_Component : TComponent ; const as_SourceProperty, as_Query : String ; const AOwner : TComponent ; const adat_ADatasetToCopy : Tdataset ) : Tdatasource;
var lobj_source : TObject;
Begin
  Result := nil;
  // Propriété ListSource
  if   assigned ( GetPropInfo ( acom_Component, as_SourceProperty ))
  and  PropIsType      ( acom_Component, as_SourceProperty , tkClass) Then
    Begin
      lobj_source := GetObjectProp   ( acom_Component, as_SourceProperty );
      if not assigned ( lobj_source ) Then
        Begin
          fdat_CloneDatasetWithoutSQLWithDataSource ( adat_ADatasetToCopy, AOwner, Result );
          SetObjectProp( acom_Component, as_SourceProperty, Result );
          p_SetSQLQuery( Result.DataSet, as_Query );
        End
       Else
         Result := lobj_source as TDatasource ;
    End;
End;

// Create a datasource with cloned no sql dataset
function fdat_CloneDatasetWithoutSQLWithDataSource ( const adat_ADataset : TDataset ; const AOwner : TComponent ; var ads_Datasource : TDatasource  ) : TDataset;
Begin
  Result := TDataset( fdat_CloneDatasetWithoutSQL ( adat_ADataset, AOwner ));
  if not assigned ( ads_Datasource ) Then
    ads_Datasource := TDatasource.create ( AOwner );
  ads_Datasource.DataSet := Result;
End;

/////////////////////////////////////////////////////////////////////////
// fonction fb_GetParamsDataset
// Retourne les paramètre d'un query
// adat_ADataset : le query
// Retours : aprs_ParamSource aprs_ParamterSource les paramètres éventuellement ADO
/////////////////////////////////////////////////////////////////////////

function fb_GetParamsDataset (const adat_ADataset : TComponent ; var aprs_ParamSource: TParams ; var Astl_Params : TStrings {$IFDEF EADO} ; var aprs_ParamterSource: TParameters {$ENDIF}): Boolean;
var lobj_SQL : TObject ;
begin
  Result := false;
  aprs_ParamSource := nil;
{$IFDEF EADO}
  aprs_ParamterSource := nil;
{$ENDIF}
  lobj_SQL := fobj_getComponentObjectProperty (adat_ADataset, 'Params' );
  if ( lobj_SQL is TParams ) Then
    Begin
      aprs_ParamSource:= lobj_SQL as TParams ;
      Result := True
    end
  else
   if  assigned ( GetPropInfo ( adat_ADataset, CST_DBPROPERTY_CLIENTPARAM )) Then
     Begin
       p_ChampsVersListe( Astl_Params, fs_getComponentProperty(adat_ADataset, CST_DBPROPERTY_CLIENTPARAM),CST_DBPROPERTY_ENDPARAM );
     End
  {$IFDEF EADO}
    else
      Begin
         lobj_SQL := fobj_getComponentObjectProperty ( adat_ADataset, 'Parameters' );
         if ( lobj_SQL is TParameters ) Then
           Begin
             aprs_ParamterSource := lobj_SQL as TParameters ;
             Result := True
           End;
      End
  {$ENDIF};
end;

/////////////////////////////////////////////////////////////////////////
// procedure p_setParamDataset
// Retourne les paramètre d'un query
// adat_ADataset : le query
// as_ParamName  : Le paramètre
// avar_Value    : sa valeur à affecter
/////////////////////////////////////////////////////////////////////////

procedure p_setParamDataset (const adat_ADataset : Tdataset ; const as_ParamName : String ; const avar_Value : Variant );
var lobj_Params1 :  TParams ;
    lprm_Param   :  TParam ;
    lstl_params : TStrings;
{$IFDEF EADO}
    lobj_Params2   :  TParameters ;
    lprm_Parameter :  TParameter ;
{$ENDIF}
begin
  lobj_Params1 := nil;
  lprm_Param   := nil;
  lstl_params  := nil;
  {$IFDEF EADO}
  lobj_Params2   := nil;
  lprm_Parameter := nil;
  {$ENDIF}
  if fb_GetParamsDataset ( adat_ADataset, lobj_Params1, lstl_params{$IFDEF EADO}, lobj_Params2{$ENDIF} ) Then
    Begin
      if assigned ( lobj_Params1 ) then
        Begin
          lprm_Param := lobj_Params1.FindParam(as_ParamName);
          if not assigned ( lprm_Param ) then
            Begin
              lprm_Param := lobj_Params1.Add as TParam;
              lprm_Param.Name := as_ParamName;
            End;
          with lprm_Param do
            Begin
              ParamType := ptInput;
              Value := avar_Value ;
            End;
        End
{$IFDEF EADO}
      else if assigned ( lobj_Params2 ) then
        Begin
          lprm_Parameter := lobj_Params2.FindParam(as_ParamName);
          if not assigned ( lprm_Parameter ) then
            Begin
              lprm_Parameter := lobj_Params2.AddParameter ;
              lprm_Parameter.Name := as_ParamName;
            End;
          with lprm_Parameter do
            Begin
              Direction := pdInput;
              Value := avar_Value ;
            End;
        End
{$ENDIF}else
         Begin
           with lstl_params do
           if IndexOfName(as_ParamName)> -1
            Then lstl_params [ IndexOfName(as_ParamName) ] := VarToStr(avar_Value)
            else Add ( as_ParamName+'='+VarToStr(avar_Value) );
          p_SetComponentProperty(adat_ADataset,CST_DBPROPERTY_CLIENTPARAM,fs_ListeVersChamps(lstl_params,CST_DBPROPERTY_ENDPARAM));
         end;
    End;
end;

// universal opening a query
procedure p_OpenSQLQuery ( const adat_Dataset : Tdataset ; const as_Query : String );
Begin
  if assigned ( adat_Dataset ) Then
    Begin
      p_SetSQLQuery ( adat_Dataset, as_Query );
      adat_Dataset.Open;
    End ;
End ;

//////////////////////////////////////////////////////////////////////////////////////////////
// procedure p_SetConnexion
// Affecte la connexion d'un dataset
// acom_Form : le dataset
// acco_Connexion : La connexion à affecter au dataset
//////////////////////////////////////////////////////////////////////////////////////////////
procedure p_SetConnexion ( const acom_ADataset : TComponent ; acco_Connexion : TCOmponent );
Begin
  if ( acom_ADataset is Tdataset ) then
    p_SetComponentObjectProperty( acom_ADataset , CST_DBPROPERTY_CONNECTION, acco_Connexion );
{$IFDEF EADO}
  if gb_IniADOsetKeySet
  and ( acom_ADataset is TCustomADOdataset )
  and (( acom_ADataset as TCustomADOdataset ).LockType <> ltReadOnly )
   then
     Begin
//      ( acom_ADataset as TCustomADOdataset ).CursorLocation := clUseServer;
      ( acom_ADataset as TCustomADOdataset ).CursorType := ctKeyset;
     End;
{$ENDIF}
End;

//////////////////////////////////////////////////////////////////////////////////////////////
// procedure p_SetComponentsConnexions
// Affecte la connexion d'un module de données ou d'une fiche
// acom_Form : le datamodule ou la tform
// acco_Connexion : La connexion à affecter aux datasets
//////////////////////////////////////////////////////////////////////////////////////////////
procedure p_SetComponentsConnexions ( const acom_Form : TComponent ; acco_Connexion : TComponent );
var li_i : Integer ;
Begin
  for li_i := 0 to acom_Form.ComponentCount - 1 do
    p_SetConnexion( acom_Form.Components [ li_i ], acco_Connexion );
End;

///////////////////////////////////////////////////////////////////////////////
// procedure p_SetSQLQuery
// affecte la requête d'un query
// adat_Dataset : le query
// as_Query : la chaine à affecter au query
///////////////////////////////////////////////////////////////////////////////

procedure p_SetSQLQuery(const adat_Dataset: TComponent; const as_Query : String ); overload;
var lobj_SQL : TObject ;
    lprm_Params : TParams ;
    lstl_Params : TStrings;
    {$IFDEF EADO}
    lprm_Parameters : TParameters ;
    {$ENDIF}
Begin
 lobj_SQL := fobj_getComponentObjectProperty ( adat_Dataset, CST_DBPROPERTY_SQL );
 if assigned ( lobj_SQL ) Then
   Begin
     lprm_Params := nil;
     lstl_Params := nil;
     fb_GetParamsDataset ( adat_Dataset, lprm_Params, lstl_Params{$IFDEF EADO}, lprm_Parameters {$ENDIF});
     if assigned ( lprm_Params ) then
       lprm_Params.Clear
     {$IFDEF EADO}
     else if assigned ( lprm_Parameters ) then
       lprm_Parameters.Clear
     {$ENDIF}
      else p_SetComponentProperty(adat_Dataset,CST_DBPROPERTY_CLIENTPARAM, '');
     if ( lobj_SQL is TStrings ) Then
      with lobj_SQL as TStrings do
       Begin
         BeginUpdate ;
         Text := as_query ;
         EndUpdate ;
       End
{$IFDEF DELPHI_9_UP}
      else if ( lobj_SQL is TWideStrings ) Then
       with lobj_SQL as TWideStrings do
        Begin
          Beginupdate;
          Text := as_query ;
          EndUpdate;
        End;
{$ENDIF}
   ;
   End;
End ;

///////////////////////////////////////////////////////////////////////////////
// fonction fs_getSQLQuery
// retourne la requête d'un query
// adat_Dataset : le query
// result : la chaine de la requête
///////////////////////////////////////////////////////////////////////////////

function fs_getSQLQuery ( const adat_Dataset : Tdataset ): String;
var lobj_SQL : TObject ;
Begin
 lobj_SQL := fobj_getComponentObjectProperty ( adat_Dataset, CST_DBPROPERTY_SQL );
 if assigned ( lobj_SQL ) Then
   Begin
     if ( lobj_SQL is TStrings ) Then
      with lobj_SQL as TStrings do
       Begin
         BeginUpdate ;
         Result := Text ;
         EndUpdate ;
       End
{$IFDEF DELPHI_9_UP}
      else if ( lobj_SQL is TWideStrings ) Then
       with lobj_SQL as TWideStrings do
        Begin
          Beginupdate;
          Result := Text ;
          EndUpdate;
        End;
{$ENDIF}
   ;
   End;
End ;

// filtering adding a like on sql query
function fb_DatasetFilterLikeRecord ( const as_DatasetValue, as_FilterValue : String ; const ab_CaseInsensitive : Boolean ): Boolean ;
Begin
  Result := False ;
  if  length ( as_DatasetValue ) >= length ( as_FilterValue ) Then
    if ab_CaseInsensitive Then
      Begin
        if lowerCase ( copy ( as_DatasetValue, 1, length ( as_FilterValue ))) = lowerCase ( as_FilterValue ) Then
          Result := True ;
      End
    Else
      Begin
        if ( copy ( as_DatasetValue, 1, length ( as_FilterValue )) = as_FilterValue ) Then
          Result := True ;
      End
End ;


// simplyfying refresh
function fb_RefreshDataset ( const aDat_Dataset : TDataset ): Boolean ;
Begin
  Result := fb_RefreshDataset ( aDat_Dataset, True );
End;

// universal refresh ( maybe )
function fb_RefreshDataset ( const aDat_Dataset : TDataset; const ab_GardePosition : Boolean ): Boolean ;
var {$IFDEF WITH_TBOOKMARK}
    lbkm_Bookmark : TBookmark ;
    {$ELSE}
    lbkm_Bookmark : TBookmarkStr ;
    {$ENDIF}
    lvar_Sort : Variant;
    ls_Sort : String ;
Begin
  Result := False ;
  if ab_GardePosition Then
    lbkm_Bookmark := aDat_Dataset.Bookmark ;
  try
    lvar_Sort := fvar_getComponentProperty( aDat_Dataset, 'Sort' );
    if VarIsStr ( lvar_Sort ) then
      ls_Sort := lvar_Sort;
    if not assigned ( ge_OnRefreshDataset )
    or not ge_OnRefreshDataset ( aDat_Dataset ) Then
        Begin
          aDat_Dataset.Close ;
          aDat_Dataset.Open ;
        End ;
    if ab_GardePosition Then
      aDat_Dataset.Bookmark := lbkm_Bookmark ;
    if ( ls_Sort <> '' )
      Then p_SetComponentProperty ( aDat_Dataset, 'Sort', ls_Sort );
    Result := True ;
  except
  End ;
End ;

// universal connect connection
procedure p_AutoConnection ( const adat_Dataset : TDataset; const AConnect : Boolean = True );
var lobj_Connect : TObject;
Begin
  if assigned ( adat_Dataset ) Then
    Begin
      lobj_Connect := fobj_getComponentObjectProperty(adat_Dataset, CST_DBPROPERTY_CONNECTION );
      if lobj_Connect is TComponent then
        p_SetComponentBoolProperty(lobj_Connect as TComponent,'Active', aconnect);
    End;
End;

///////////////////////////////////////////////////////////////////////////////
// procedure p_AddSQLQuery
// Ajoute une chaine sanns retour chariot à un query
// adat_Dataset : le query
// as_Query : la chaine à ajouter
///////////////////////////////////////////////////////////////////////////////
procedure p_AddSQLQuery ( const adat_Dataset : Tdataset ; const as_Query : String );
var lobj_SQL : TObject ;
Begin
 lobj_SQL := fobj_getComponentObjectProperty ( adat_Dataset, CST_DBPROPERTY_SQL );
 if assigned ( lobj_SQL ) Then
   Begin
     if ( lobj_SQL is TStrings ) Then
       ( lobj_SQL as TStrings ).Add ( as_query )
{$IFDEF DELPHI_9_UP}
      else if ( lobj_SQL is TWideStrings ) Then
       ( lobj_SQL as TWideStrings ).Add ( as_query )
{$ENDIF}
   ;
   End;
End ;

///////////////////////////////////////////////////////////////////////////////
// Créé un paramètre dans le query après l'avoir vérifié
// avar_EnregistrementCle : un variant à mettre dans la form propriétaire
//                          et à ne toucher qu'avec cette fonction
// adat_Dataset           : Le dataset de la clé
// as_Cle                 : LA clé
////////////////////////////////////////////////////////////////////////////////
function fb_SetParamQuery(const adat_Dataset : TDataset ;
  const as_Param: String): Boolean;
var lobj_SQL : TObject ;
Begin
  Result := False;
  lobj_SQL := fobj_getComponentObjectProperty ( adat_Dataset, 'Params' );
  if ( lobj_SQL is TParams ) Then
    Begin
      with lobj_SQL as TParams do
        if FindParam(as_Param)= nil then
          with Add as TParam do
            Begin
              Name := as_Param;
              Result := True;
            End;
    End;
{$IFDEF EADO}
  if not assigned ( lobj_SQL )
    Then
      lobj_SQL := fobj_getComponentObjectProperty ( adat_Dataset, 'Parameters' );
  if ( lobj_SQL is TParameters ) Then
    with lobj_SQL as TParameters do
        if FindParam(as_Param)= nil then
          with Add as TParameter do
            Begin
              Name := as_Param;
              Result := True;
            End;
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// Procédure : fb_RefreshDatasetIfEmpty
// Description : Rafraichit le dataset filtré quand il est vide
////////////////////////////////////////////////////////////////////////////////
function fb_RefreshDatasetIfEmpty ( const adat_Dataset : TDataset ) : Boolean ;
var ldoingAfter : TDataAction;
begin
  Result := False ;
  // Rafraichissement la première fois qu'on ne retrouve pas ce qu'on cherche
  if adat_Dataset.IsEmpty Then
    try
      Result := True ;
{$IFDEF EADO}
      if ( adat_Dataset is TCustomADODataset ) Then
        ( adat_Dataset as TCustomADODataset ).Requery
      Else
{$ENDIF}
        Begin
          adat_Dataset.Close ;
          adat_Dataset.Open ;
        End ;
    Except
      on e: EDatabaseError do
       begin
         ldoingAfter := daRetry;
         p_GereExceptionEvent ( adat_Dataset, e, ldoingAfter );
       end;
    End ;
End ;

function fb_LocateSansFiltre ( const aado_Seeker : TDataset ; const as_Fields : String ; const avar_Records : Variant ; const ach_Separator : Char ): Boolean ;
var ls_Filter : String ;
Begin
  Result := False ;
  if pos ( as_Fields, ach_Separator +'' ) <= 0 Then
    Begin
      aado_Seeker.Close ;
      ls_Filter :=  fs_getSQLQuery ( aado_Seeker );
      if ( pos ( 'where', lowercase (ls_filter))> 0 ) Then
        ls_Filter := ls_Filter + CST_SQL_AND
       else
        ls_Filter := ls_Filter + CST_SQL_WHERE ;
      ls_Filter := ls_Filter + as_Fields + '=' ;
      if ( aado_Seeker.FieldByName ( as_Fields ).DataType in CST_DELPHI_FIELD_STRING )
        Then
          ls_Filter := ls_Filter + '''' + fs_stringDbQuote (VarToStr (avar_Records)) + ''''
        Else
          ls_Filter := ls_Filter + fs_stringDbQuote ( VarToStr ( avar_Records )) ;
      p_SetSQLQuery ( aado_Seeker, ls_Filter  );
      aado_Seeker.Open ;
      Result := aado_Seeker.RecordCount > 0 ;
    End ;
End ;

// auto table on query
procedure p_LocateInit ( const aado_Seeker : TDataset ; const as_Table, as_Fields, as_Condition : String );
var ls_Filter : String ;
Begin
  if trim(as_Fields) = ''
   Then ls_Filter := CST_SQL_SELECT + CST_SQL_ALL + CST_SQL_FROM + as_Table
   Else ls_Filter := CST_SQL_SELECT+as_Fields+CST_SQL_FROM + as_Table ;
  if as_Condition <> '' Then
    ls_Filter := CST_SQL_WHERE + as_Condition ;
  p_SetSQLQuery ( aado_Seeker, ls_Filter  );
  aado_Seeker.Open ;
End ;


// Trier le dataset en cours
// as_ChampsOrdonner : Le sort à affecter
function fb_AssignSort ( const adat_Dataset : TDataset ; const as_ChampsOrdonner : String ):Boolean;
Begin

   if  assigned ( adat_Dataset )
   and          ( adat_Dataset.Active ) Then
     If assigned ( GetPropInfo ( adat_Dataset, 'Sort' ))
      Then
       Begin
         p_SetComponentProperty ( adat_Dataset, 'Sort', as_ChampsOrdonner );
         Result := True;
        End
       Else If assigned ( GetPropInfo ( adat_Dataset, 'SortedFields' ))
        Then
         Begin
           p_SetComponentProperty ( adat_Dataset, 'SortedFields', as_ChampsOrdonner );
           Result := True;
          End
         Else
          Result := False;
//  Showmessage (( gdl_DataLink.DataSet as TCustomADODataset ).Sort );
End ;


function fb_AssignSort ( const adat_Dataset : TDataset ; const astl_list : TStrings ; const ai_ChampsOrdonner : Integer ):Boolean;
Begin
  if  ( ai_ChampsOrdonner >= 0 )
  and ( ai_ChampsOrdonner < astl_list.count )
   Then Result := fonctions_dbobjects.fb_AssignSort(adat_Dataset, astl_list [ai_ChampsOrdonner])
   Else Result := False;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : fb_KeyRecordExists
// Description :
// Paramètres : adat_Dataset : Le dataset du compteur
//              adat_DatasetQuery : The query dataset
//              as_Table         : La table du compteur
//              as_Where         : The Where clause
/////////////////////////////////////////////////////////////////////////////////
function fb_RecordExists ( const adat_DatasetQuery : TDataset ;
                           const as_Table, as_Where : String;
                           const ab_DBMessageOnError  : Boolean ): Boolean;
var li_Pos     : Integer ;
    ls_SQL : WideString ;
    ldoingAfter : TDataAction;
begin
  Result := False;

  adat_DatasetQuery.Close;
  // sélectionner le compteur maximum
  li_pos := pos ( 'where', lowercase ( as_Where ));
  if ( li_pos < 1 )
  or ( li_pos > 2 )
   Then ls_SQL:= CST_SQL_WHERE + as_Where
   else ls_SQL:= ' ' + as_Where;
  ls_SQL := CST_SQL_SELECT + CST_SQL_ALL + CST_SQL_FROM + as_Table + ls_SQL;
  try
    p_OpenSQLQuery ( adat_DatasetQuery, ls_SQL );
    Result := adat_DatasetQuery.RecordCount > 0;
  Except
    On E:EDatabaseError do
     Begin
       ldoingAfter := daRetry;
       p_GereExceptionEvent ( adat_DatasetQuery, E,ldoingAfter, ab_DBMessageOnError );
     end;
  End ;
end;


/////////////////////////////////////////////////////////////////////////////////
// Fonction : fb_KeyRecordExists
// Description :
// Paramètres : adat_Dataset : Le dataset du compteur
//              adat_DatasetQuery : The query dataset
//              aslt_Cle     : La clé du dataset
//              as_Table         : La table du compteur
//              as_fieldName    : The unique Fieldname
/////////////////////////////////////////////////////////////////////////////////
function fb_FieldRecordExists  ( const adat_Dataset, adat_DatasetQuery : TDataset ;
                               const as_Table, as_FieldName : String;
                               const ab_DBMessageOnError  : Boolean ): Boolean;
var ls_Where : WideString ;
begin
  if adat_Dataset.FindField ( as_FieldName ) is TNumericField
   then ls_Where := as_FieldName + '=' + adat_Dataset.FindField ( as_FieldName ).AsString
   else ls_Where := as_FieldName + '=''' + fs_stringDbQuote ( adat_Dataset.FindField ( as_FieldName ).AsString ) + '''';

  Result := fb_RecordExists ( adat_DatasetQuery, as_Table, ls_Where, ab_DBMessageOnError );
end;


// execute query with optional module
procedure p_ExecuteSQLCommand ( const as_Command :{$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF} ; const ab_ShowException : boolean = True );
Begin
  try
    if assigned ( ge_OnExecuteCommand ) Then
     ge_OnExecuteCommand ( as_Command );
  Except
    on E:Exception do
     if ab_ShowException Then
       p_ShowSQLError(E.Message,as_Command);
  end;
End ;

// open or close one type of database
function fb_OpenCloseDatabase ( const AConnection  : TComponent ;
                                const ab_Open : Boolean ;
                                const ab_showError : Boolean = False   ):Boolean;
begin
  if Assigned(ge_OnOpenOrCloseDatabase) Then
   Try
     Result:=ge_OnOpenOrCloseDatabase ( AConnection, ab_open, ab_showError );

   Except
   end;
End;

// optimise one original database
procedure p_optimiseDatabase ( const AConnection : TComponent;
                               const as_database, as_user, as_password, APathSave : String );
var ls_Message : String;
begin
  if Assigned(ge_OnOptimiseDatabase) Then
   Begin

    if ge_OnOptimiseDatabase ( AConnection, as_database, as_user, as_password, APathSave, nil, nil, AConnection )
     Then ls_Message:=gs_Optimising_database_is_a_success
     Else ls_Message:=gs_Error_Restore_Directory_does_not_exists;
   End;
end;

// optimise one original database with message
function fb_OptimiseDatabase  ( const AConnection : TComponent ;
                                const as_database, as_user, as_password, APathSave : String ;
                                const ASt_Messages : TStrings;
                                const acom_ControlMessage, acom_owner : TComponent):Boolean;
Begin
  if Assigned(ge_OnOptimiseDatabase) Then
    Result:=ge_OnOptimiseDatabase (AConnection,
                                   as_database, as_user, as_password, APathSave,
                                   ASt_Messages,
                                   acom_ControlMessage, acom_owner);
end;


// execute query with optional module
procedure p_ExecuteSQLScriptServer ( const AConnection : TComponent; const as_Command :{$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF} ; const ab_ShowException : boolean = True );
Begin
  try
    if assigned ( ge_OnExecuteScriptServer ) Then
     ge_OnExecuteScriptServer ( AConnection, as_Command );
  Except
    on E:Exception do
     if ab_ShowException Then
      p_ShowSQLError(E.Message,as_Command);
  end;
End ;

// Open connexion and errors
function fb_TestConnection ( const Connexion : TComponent ; const lb_ShowMessage : Boolean ) : Boolean;
Begin
  Result := False ;
  if not Assigned(ge_OnOpenOrCloseDatabase) Then
  try
    p_SetComponentBoolProperty ( Connexion, CST_CONNECTED, True );
  Except
    on E: Exception do
      Begin
        if lb_ShowMessage Then
          MyShowMessage ( gs_TestBad + ' : ' + #13#10 + E.Message );
        Exit ;
      End ;
  End ;
  if ( Assigned(ge_OnOpenOrCloseDatabase)
      and fb_OpenCloseDatabase(Connexion,True,False))
  or ( not Assigned(ge_OnOpenOrCloseDatabase)
      and fb_getComponentBoolProperty( Connexion, CST_CONNECTED ))
   Then
    Begin
      Result := True ;
      if lb_ShowMessage Then
        MyShowMessage ( gs_TestOk );
    End ;
End ;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_db_components );
{$ENDIF}
end.
