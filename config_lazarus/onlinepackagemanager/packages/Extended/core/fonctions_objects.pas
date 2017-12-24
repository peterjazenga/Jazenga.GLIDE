unit fonctions_objects;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses SysUtils,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  DB,
  Classes ;

  {$IFDEF VERSIONS}
const
  gVer_fonctions_components : T_Version = ( Component : 'Fonctions de personnalisation des composants' ;
                                         FileUnit : 'fonctions_components' ;
      			                 Owner : 'Matthieu Giroux' ;
      			                 Comment : 'Fonctions de gestion des composants visuels.' ;
      			                 BugsStory : 'Version 1.0.5.1 : More cloning control.'+#10
                                                   + 'Version 1.0.5.0 : Show CSV or HTML File.'+#10
                                                   + 'Version 1.0.4.0 : Centralizing setting MyLabel.'+#10
                                                   + 'Version 1.0.3.0 : Menu cloning.'+#10
                                                   + 'Version 1.0.2.0 : CSV and HTML Grid''s Export.'+#10
                                                   + 'Version 1.0.1.0 : Auto combo init.'+#10
                                                   + 'Version 1.0.0.0 : Ajout de fonctions d''automatisation.';
      			                 UnitType : 1 ;
      			                 Major : 1 ; Minor : 0 ; Release : 5 ; Build : 1 );

  {$ENDIF}
type TFieldMethod = function ( const AField : TField ;
                               var IsFirst : Boolean ;
                               const Separator : String; const AReplaceCaption : String = '' ):String;

procedure p_GoToEvent ( const AEvent : TNotifyEvent ; const ASender :TObject );
function  fb_AutoComboInit ( const acom_Combo : TComponent ):Boolean;
function fcom_CloneObject  ( const acom_AObject : TComponent ; const AOwner : TComponent ) : TComponent;

implementation

uses fonctions_string,
  fonctions_proprietes,
  fonctions_system;

procedure p_GoToEvent ( const AEvent : TNotifyEvent ; const ASender :TObject );
Begin
  if Assigned(AEvent)
   Then AEvent(ASender);
end;

//////////////////////////////////////////////////////////////////////
// Fonction retournant le composant copié sans la personnalisation
//  acom_AObject : Le composant à cloner
//  AOwner       : Le futur propriétaire du composant
// Résultat de la fonction : Le composant cloné sans la personnalisation
//////////////////////////////////////////////////////////////////////
function fcom_CloneObject ( const acom_AObject : TComponent ; const AOwner : TComponent ) : TComponent;
Begin
  Result := TComponent ( acom_AObject.ClassType.NewInstance );

  Result.Create ( AOwner );
End;

function AddFieldCSV ( const AField : TField ; var IsFirst : Boolean; const Separator : String; const AReplaceCaption : String = '' ):String;
  function AddField : String;
   Begin
     if AReplaceCaption = ''
      Then Result := StringReplace (  StringReplace ( StringReplace(AField.AsString, CST_ENDOFLINE, '\n', [ rfReplaceAll ] ), '\', '\\', [ rfReplaceAll ] ), '"', '\"', [ rfReplaceAll ] )
      Else Result := AReplaceCaption;
   end;

Begin
  if IsFirst Then
   Begin
    IsFirst := False;
    Result := AddField;
   end
  Else
   Begin
     Result := Separator + AddField;
   end;
end;

function AddFieldHTML ( const AField : TField ; var IsFirst : Boolean; const Separator : String; const AReplaceCaption : String = '' ):String;
  function AddField : String;
   Begin
     if AReplaceCaption = ''
      Then Result := StringReplace(AField.AsString, CST_ENDOFLINE, '<BR>'+CST_ENDOFLINE, [ rfReplaceAll ] )
      Else Result := AReplaceCaption;
   end;

Begin
  if IsFirst Then
   Begin
    IsFirst := False;
    Result := AddField;
   end
  Else
   Begin
     Result := Separator + AddField;
   end;
end;

function fb_AutoComboInit ( const acom_Combo : TComponent ):Boolean;
var astl_Items : TStrings;
Begin

  astl_Items:= TStrings( fobj_getComponentObjectProperty ( acom_Combo, CST_PROPERTY_ITEMS ));
  if ( astl_Items = nil )
  or ( astl_Items.Count = 0 )
   Then
     Begin
      Result:=False;
      Exit;
     end;
  if ( fli_getComponentProperty( acom_Combo, CST_PROPERTY_ITEMINDEX ) < 0 )
   Then
    p_SetComponentProperty( acom_Combo, CST_PROPERTY_ITEMINDEX, 0 );
  p_SetComponentProperty( acom_Combo, CST_PROPERTY_TEXT, astl_Items[fli_getComponentProperty( acom_Combo, CST_PROPERTY_ITEMINDEX )]);
  Result := True;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_components );
{$ENDIF}
end.
