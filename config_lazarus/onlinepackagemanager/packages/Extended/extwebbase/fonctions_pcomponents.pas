unit fonctions_pcomponents;

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
  Classes,
{$IFDEF UseRuntime}
  Ext, ExtPascal, ExtForm,
  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,
  ExtMenu,  ExtState;

type
  {$M+}
  TExtPanel_Tab = TExtPanel;
  TExtFormTextField_Grid = TExtFormTextField;
  TExtFormNumberField_Grid = TExtFormNumberField;
  TExtFormDateField_Grid = TExtFormDateField;
  TExtFormTimeField_Grid = TExtFormTimeField;
  TExtFormCheckbox_Grid = TExtFormCheckbox;
  TExtFormComboBox_Grid = TExtFormComboBox;
  {$M-}

{$ELSE}
  ExtP_Design_Ctrls, ExtP_Design_Grid;
{$ENDIF}
  {$IFDEF VERSIONS}
const
  gVer_fonctions_components : T_Version = ( Component : 'Fonctions de personnalisation des composants' ;
                                         FileUnit : 'fonctions_components' ;
      			                 Owner : 'Matthieu Giroux' ;
      			                 Comment : 'Fonctions de gestion des composants visuels.' ;
      			                 BugsStory : 'Version 1.1.0.0 : Restructure.'+#10
                                                   + 'Version 1.0.5.1 : More cloning control.'+#10
                                                   + 'Version 1.0.5.0 : Show CSV or HTML File.'+#10
                                                   + 'Version 1.0.4.0 : Centralizing setting MyLabel.'+#10
                                                   + 'Version 1.0.3.0 : Menu cloning.'+#10
                                                   + 'Version 1.0.2.0 : CSV and HTML Grid''s Export.'+#10
                                                   + 'Version 1.0.1.0 : Auto combo init.'+#10
                                                   + 'Version 1.0.0.0 : Ajout de fonctions d''automatisation.';
      			                 UnitType : 1 ;
      			                 Major : 1 ; Minor : 1 ; Release : 0 ; Build : 0 );

  {$ENDIF}

type TFieldMethod = function ( const AField : TextdaField ;
                               var IsFirst : Boolean ;
                               const Separator : String; const AReplaceCaption : String = '' ):String;
procedure ExportGridToHTML(const AFileName : String ; const AGrid : TCustomDBGrid;const ab_Header, ab_all : Boolean ; const As_Extension : String = 'html' ; const ab_showFile : Boolean = True );
procedure ExportGridToCSV (const AFileName : String ; const AGrid : TCustomDBGrid;const ab_Header, ab_all : Boolean ; const As_Extension : String = 'csv'; const aseparate : Char = ';' ; const ab_showFile : Boolean = True );
procedure ExportGridTo ( const AFieldMethod : TFieldMethod; const Afile : TStringList; const AGrid : TCustomDBGrid; const As_beginLine, as_endLine, as_beginCell, as_endCell, as_separator, As_beginHeader, As_EndHeader, As_beginText, As_EndText : String ; const ab_header : Boolean = False );
function fcon_CloneControl ( const acon_AControl : TControl  ; const AOwner : TComponent ) : TControl;
function fmi_CloneMenuItem ( const AMenuItem : TMenuItem; const amenuDestination : TMenu ): TMenuItem ;
procedure p_setMyLabel ( var Flabel : TCustomLabel; const ALabel :  TCustomLabel; const AOwner : TComponent);
procedure p_ComponentSelectAll ( const aobj_Component : TObject );

implementation

uses fonctions_string,
  fonctions_proprietes,
  fonctions_system,
  fonctions_objects,
  fonctions_languages;

procedure p_ComponentSelectAll ( const aobj_Component : TObject );
Begin
  if aobj_Component is TCustomEdit Then
    (aobj_Component as TCustomEdit ).SelectAll;
End;

function fmi_CloneMenuItem ( const AMenuItem : TMenuItem; const amenuDestination : TMenu ): TMenuItem ;
Begin
  Result := fcom_CloneObject( AMenuItem, amenuDestination ) as TMenuItem;
  with Result do
   Begin
     Tag        := AMenuItem.Tag;
     Hint       := AMenuItem.Hint;
     Caption    := AMenuItem.Caption;
     ImageIndex := AMenuItem.ImageIndex;
     OnClick    := AMenuItem.OnClick;
     Action     := AMenuItem.Action;
     Name       := AMenuItem.Name;
   end;
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

procedure ExportGridTo ( const AFieldMethod : TFieldMethod; const Afile : TStringList; const AGrid : TCustomDBGrid; const As_beginLine, as_endLine, as_beginCell, as_endCell, as_separator, As_beginHeader, As_EndHeader, As_beginText, As_EndText : String ; const ab_header : Boolean = False );
var i : Integer;
    IsFirst : Boolean;
    AString : String ;
    Acolumns : TDBGridColumns;

Begin
  Acolumns := fobj_getComponentObjectProperty( AGrid, 'Columns' ) as TDBGridColumns;
  IsFirst := True;
  AString := '';
  if ab_header Then
   for i := 0 to Acolumns.count - 1 do
    with Acolumns [ i ] do
     if Visible Then
      Begin
       if  not ( Field is TNumericField )
       and not ( Field is TStringField  )
       and not ( Field is TBooleanField ) then
        Continue;
       AppendStr ( AString, as_beginCell + As_beginHeader + AFieldMethod ( nil, IsFirst, as_separator, Title.Caption ) + As_EndHeader + as_endCell );
      end;

  with AGrid, (fobj_getComponentObjectProperty( AGrid, 'Datasource' ) as TDataSource ).Dataset do
    if not IsEmpty then
     Begin
       DisableControls;
       try
         First;
         repeat
           AString := As_beginLine;
           IsFirst := True;
           for i := 0 to Acolumns.count - 1 do
            with Acolumns [ i ] do
             if Visible Then
              Begin
               if  not ( Field is TNumericField )
               and not ( Field is TStringField  )
               and not ( Field is TBooleanField ) then
                Continue;
               AppendStr ( AString, as_beginCell + As_beginText + AFieldMethod ( Field, IsFirst, as_separator ) + As_EndText + as_endCell );
              end;
           Next;
           Afile.Add (AString+as_endLine);
         until Eof;
       finally
         EnableControls;
       end;
     end;
End;

procedure ExportGridToCSV (const AFileName : String ; const AGrid : TCustomDBGrid;const ab_Header, ab_all : Boolean ; const As_Extension : String = 'csv'; const aseparate : Char = ';' ; const ab_showFile : Boolean = True );
var astringlist : TStringList;
Begin
  astringlist := TStringList.Create;
  try
    ExportGridTo ( TFieldMethod (@AddFieldCSV), astringlist, AGrid, '','','','','"','"','"','"',aseparate, ab_Header);
    astringlist.SaveToFile(AFileName);
  finally
    astringlist.Free;
  end;
  if ab_showFile Then
   p_OpenFileOrDirectory(AFileName);
End;

procedure ExportGridToHTML(const AFileName : String ; const AGrid : TCustomDBGrid;const ab_Header, ab_all : Boolean ; const As_Extension : String = 'html' ; const ab_showFile : Boolean = True );
var astringlist : TStringList;
Begin
  astringlist := TStringList.Create;
  try
    astringlist.Add('<HTML><HEAD><meta http-equiv=Content-Type content="text/html; charset='+gs_HtmlCharset+'" /></HEAD><BODY><TABLE>');
    ExportGridTo ( AddFieldHTML, astringlist, AGrid, '<TR>','</TR>','<TD>','</TD>','','<STRONG>','</STRONG>','','', ab_Header);
    astringlist.Add('</TABLE></BODY></HTML>');
    astringlist.SaveToFile(AFileName);
  finally
    astringlist.Free;
  end;
  if ab_showFile Then
   p_OpenFileOrDirectory(AFileName);
End;

procedure p_setMyLabel ( var Flabel : TCustomLabel; const ALabel :  TCustomLabel; const AOwner : TComponent);
Begin
  if ALabel <> FLabel Then
    Begin
      FLabel := ALabel;
      if Assigned(FLabel) Then
        p_SetComponentObjectProperty ( FLabel, 'MyEdit', AOwner );
    End;
end;

function fcon_CloneControl ( const acon_AControl : TControl  ; const AOwner : TComponent ) : TControl;
Begin
  Result := TControl ( fcom_CloneObject ( acon_AControl, AOwner ));
  with Result do
   Begin
    Parent  := acon_AControl.Parent;
    Top     := acon_AControl.Top;
    Left    := acon_AControl.Left;
    Width   := acon_AControl.Width;
    Height  := acon_AControl.Height;
    Align   := acon_AControl.Align;
    Tag     := acon_AControl.Tag;
    Visible := acon_AControl.Visible;
    Anchors := acon_AControl.Anchors;
    Cursor  := acon_AControl.Cursor;
    p_SetComponentProperty ( Result, CST_PROPERTY_MAXVALUE  , fli_getComponentProperty ( acon_AControl, CST_PROPERTY_MAXVALUE ));
    p_SetComponentProperty ( Result, CST_PROPERTY_TEXT      , fs_getComponentProperty  ( acon_AControl, CST_PROPERTY_TEXT ));
    p_SetComponentProperty ( Result, CST_PROPERTY_CAPTION   , fs_getComponentProperty  ( acon_AControl, CST_PROPERTY_CAPTION ));
    p_SetComponentProperty ( Result, CST_PROPERTY_COLOR     , fli_getComponentProperty ( acon_AControl, CST_PROPERTY_COLOR ));
    p_SetComponentObjectProperty ( Result, CST_PROPERTY_FONT, fobj_getComponentObjectProperty ( acon_AControl, CST_PROPERTY_FONT ));
   end;
End;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_components );
{$ENDIF}
end.
