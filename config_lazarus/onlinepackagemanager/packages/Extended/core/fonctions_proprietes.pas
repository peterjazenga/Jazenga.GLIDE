unit fonctions_proprietes;

{$I ..\DLCompilers.inc}

interface
{
2004-08-27
Création de l'unité par Matthieu Giroux
}
uses Variants, TypInfo, Classes,
{$IFDEF DELPHI_9_UP}
     WideStrings ,
{$ENDIF}
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  fonctions_string ;

const
  {$IFDEF VERSIONS}
    gVer_mc_fonction_proprietes : T_Version = ( Component : 'Gestion des propriétés de zones' ;
                                                FileUnit : 'fonctions_proprietes' ;
                                                Owner : 'Matthieu Giroux' ;
                                      Comment : 'Fonctions de récupération de propriétés publiées.' ;
                                      BugsStory :  'Version 1.0.0.2 : TObject has got properties.' +
                                                   'Version 1.0.0.1 : Test de l''existence de la propiété pour fs_getComponentProperty' +
                                                   'Version 1.0.0.0 : Toutes les fonctions sont OK.';
                                      UnitType : 1 ;
                        	      Major : 1 ; Minor : 0 ; Release : 0 ; Build : 2 );
    {$ENDIF}

    CST_PROPERTY_DISPLAYFORMAT  = 'DisplayFormat';
    CST_PROPERTY_COLUMNS        = 'Columns';
    CST_PROPERTY_WIDTH          = 'Width';
    CST_PROPERTY_TEXT           = 'Text' ;
    CST_PROPERTY_INITIALDIR     = 'InitialDir' ;
    CST_PROPERTY_MAXVALUE       = 'MaxValue' ;
    CST_PROPERTY_IMAGES         = 'Images' ;
    CST_PROPERTY_CAPTION        = 'Caption' ;
    CST_PROPERTY_ITEMS          = 'Items'  ;
    CST_PROPERTY_VALUES         = 'Values'  ;
    CST_PROPERTY_VALUE          = 'Value'  ;
    CST_PROPERTY_ITEMINDEX      = 'ItemIndex'  ;
    CST_PROPERTY_CANVAS         = 'Canvas'  ;
    CST_PROPERTY_COUNT          = 'Count'  ;
    CST_PROPERTY_COLOR          = 'Color'  ;
    CST_PROPERTY_FONT           = 'Font'  ;

    CST_DBPROPERTY_LISTFIELD      = 'ListField';
    CST_DBPROPERTY_LOOKUPDISPLAY  = 'LookupDisplay';
    CST_DBPROPERTY_LISTSOURCE     = 'ListSource';
    CST_DBPROPERTY_LOOKUPSOURCE   = 'LookupSource';
    CST_DBPROPERTY_SEARCHSOURCE   = 'SearchSource';
    CST_DBPROPERTY_KEYFIELD       = 'KeyField';
    CST_DBPROPERTY_LOOKUPFIELD    = 'LookupField';
    CST_DBPROPERTY_SQL = 'SQL';
    CST_DBPROPERTY_SQLCONNECTION = 'SQLConnection';
    CST_DBPROPERTY_CONNECTION = 'Connection';
    CST_DBPROPERTY_CONNECTED  = 'Connected';
    CST_DBPROPERTY_ONLINECONN = 'OnLineConn';
    CST_DBPROPERTY_CONNECTIONSTRING = 'ConnectionString';
    CST_DBPROPERTY_TRANSACTION = 'Transaction';
    CST_DBPROPERTY_DATABASE = 'Database';
    CST_DBPROPERTY_DATASOURCE = 'Datasource';
    CST_DBPROPERTY_DATAFIELD = 'DataField';
    CST_DBPROPERTY_FIELDNAME = 'FieldName';
    CST_DBPROPERTY_DATABASENAME = 'DatabaseName';
    CST_DBPROPERTY_SESSIONNAME = 'SessionName';
    CST_DBPROPERTY_UPDATEOBJECT = 'UpdateObject';
    CST_DBPROPERTY_CLIENTPARAM = 'ClientParam';
    CST_DBPROPERTY_ZEOSDB = 'ZeosDBConnection';
    CST_DBPROPERTY_FIELDDEFS = 'FieldDefs';
    CST_DBPROPERTY_Active = 'Active';
    CST_DBPROPERTY_ENDPARAM = ';';
    CST_DBIBPROPERTY_AutoTrans = 'AllowAutoActivateTransaction';

type
  // On utilise les tableaux de variant pour plus tard :
  // gestion des clés à champs multiples
  tt_TableauxVarOption = Array of Record    // Items
        as_Key : tt_valeurStrings ;  // Multiple Key
        i_Option : Byte ;  // option
       End ;

  tset_OctetOptions = set of Byte;

procedure p_SetComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : Variant ); overload;
procedure p_SetComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ; const a_ValueToSet : Variant ); overload;
function fvar_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ) : Variant ; overload;
function fvar_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ) : Variant ; overload;
function fli_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const AResult : Longint = -1 ) : LongInt ; overload;
function fs_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ) : String ; overload;
function fb_getComponentBoolProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const AResult : Boolean = False ) : Boolean ;
procedure p_SetComponentBoolProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : Boolean );
procedure p_SetComponentObjectProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : TObject );
function fobj_getComponentObjectProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TObject ;
function fcla_getComponentClassProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TClass ;
procedure p_SetComponentMethodProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : TMethod );
procedure p_SetComponentMethodNameProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const aobj_ComponentMethod : TObject ; const a_MethodToSet : String );
function fmet_getComponentMethodProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TMethod ;
function fobj_getComponentStringsProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TStrings ;
{$IFDEF DELPHI_9_UP}
function fobj_getComponentWideStringsProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TWideStrings ;
{$ENDIF}
function fb_GetStrings (const acom_component : TObject ;const as_propertyname : String ; var astl_Strings : TStrings {$IFDEF DELPHI_9_UP}; var awst_Strings : TWideStrings {$ENDIF}): Boolean;

implementation


function fb_GetStrings (const acom_component : TObject ;const as_propertyname : String ; var astl_Strings : TStrings {$IFDEF DELPHI_9_UP}; var awst_Strings : TWideStrings {$ENDIF}): Boolean;
var lobj_Strings : TObject ;
begin
 lobj_Strings := fobj_getComponentObjectProperty ( acom_component, as_propertyname );
  if assigned ( lobj_Strings ) Then
    Begin
      Result := True;
      if ( lobj_Strings is TStrings ) Then
        astl_Strings  := lobj_Strings as TStrings
{$IFDEF DELPHI_9_UP}
        else if ( lobj_Strings is TWideStrings ) Then
          awst_Strings := lobj_Strings as TWideStrings
{$ENDIF}
        ;
    End
   else
    Result := false;
end;
function fb_getComponentBoolProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const AResult : Boolean = False ) : Boolean ;
Begin
  Result := AResult ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName );
End ;
procedure p_SetComponentBoolProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : Boolean );
Begin
 if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
 then SetPropValue    ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;
// Affecte une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : Variant ); overload;
Begin
 if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
 then SetPropValue    ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;
// Affecte une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// aTyp_PropertyType    : Type de propriété
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ; const a_ValueToSet : Variant ); overload;
Begin
 if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
 and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , aTyp_PropertyType)
 then SetPropValue    ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// aTyp_PropertyType    : Type de propriété
function fvar_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ) : Variant ;
Begin
  Result := Null ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , aTyp_PropertyType)
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName );
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
function fvar_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ) : Variant ;
Begin
  Result := Null ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName );
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
function fli_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const AResult : Longint = -1 ) : LongInt ;
Begin
  Result := AResult ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName, False );
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
function fs_getComponentProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ) : String ;
Begin
  Result := '' ;
  if   IsPublishedProp ( aComp_ComponentToSet, as_PropertyName )
    Then Result := getStrProp    ( aComp_ComponentToSet, as_PropertyName );
End ;
// Affecte une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentObjectProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : TObject );
Begin
  if   IsPublishedProp ( aComp_ComponentToSet, as_PropertyName )
  and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , tkClass)
  then SetObjectProp   ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;

// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fcla_getComponentClassProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TClass ;
Begin
  Result := nil ;
  if   assigned ( GetPropInfo ( aComp_Component, as_PropertyName ))
  and  PropIsType      ( aComp_Component, as_PropertyName , tkClass)
  then Result := GetObjectPropClass   ( aComp_Component, as_PropertyName );
End ;


// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fmet_getComponentMethodProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TMethod ;
Begin
  if   assigned ( GetPropInfo ( aComp_Component, as_PropertyName ))
  and  PropIsType      ( aComp_Component, as_PropertyName , tkMethod)
  then Result := GetMethodProp   ( aComp_Component, as_PropertyName )
  Else
    Begin
     Result.Data := aComp_Component;
     Result.Code := nil;
    End;
End ;


// Affecte une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentMethodProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const a_ValueToSet : TMethod );
Begin
  if   IsPublishedProp ( aComp_ComponentToSet, as_PropertyName )
  and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , tkMethod)
  then SetMethodProp   ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;


procedure p_SetComponentMethodNameProperty ( const aComp_ComponentToSet : TObject ; const as_PropertyName : String ; const aobj_ComponentMethod : TObject ; const a_MethodToSet : String );
var lmet_MethodeDistribuee : TMethod ;
Begin
  lmet_MethodeDistribuee.Data := aobj_ComponentMethod;
  lmet_MethodeDistribuee .Code := aobj_ComponentMethod.MethodAddress(a_MethodToSet);
  p_SetComponentMethodProperty ( aComp_ComponentToSet, as_PropertyName, lmet_MethodeDistribuee );
End ;
// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fobj_getComponentObjectProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TObject ;
Begin
  Result := nil ;
  if   assigned ( GetPropInfo ( aComp_Component, as_PropertyName ))
  and  PropIsType      ( aComp_Component, as_PropertyName , tkClass)
  then Result := GetObjectProp   ( aComp_Component, as_PropertyName );
End ;


// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fobj_getComponentStringsProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TStrings ;
var fobj_Objet : TObject ;
Begin
  Result := nil ;
  fobj_Objet := fobj_getComponentObjectProperty ( aComp_Component, as_PropertyName );
  if fobj_Objet is TStrings Then
    Result := fobj_Objet as TStrings ;               
End ;

// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
{$IFDEF DELPHI_9_UP}

function fobj_getComponentWideStringsProperty ( const aComp_Component : TObject ; const as_PropertyName : String ) : TWideStrings ;
var fobj_Objet : TObject ;
Begin
  Result := nil ;
  fobj_Objet := fobj_getComponentObjectProperty ( aComp_Component, as_PropertyName );
  if fobj_Objet is TWideStrings Then
    Result := fobj_Objet as TWideStrings ;
End ;
{$ENDIF}


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_mc_fonction_proprietes );
  {$ENDIF}
end.

