// Unité de gestion du fichier INI dont dépendant l'unité FormMainIni
// intégrant une form de gestion de fichier INI
unit fonctions_init;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface
uses
{$IFDEF FPC}
     LCLIntf, SQLDB,
{$ELSE}
     Windows, MaskUtils, SHLObj,
{$ENDIF}
     IniFiles, Forms, sysUtils, classes, ComCtrls,
{$IFDEF VIRTUALTREES}
     VirtualTrees,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  dialogs,
  {$IFDEF DELPHI_9_UP}
  WideStrings,
  {$ENDIF}
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
  DBGrids,Graphics;

type
  TIniEvent = procedure( const afor_MainObject : TObject ; const aini_iniFile : TCustomInifile ) of object;

{$IFDEF VERSIONS}
const
  gVer_fonctions_init : T_Version = ( Component : 'Gestion du fichier INI' ; FileUnit : 'fonctions_init' ;
                                      Owner     : 'Matthieu Giroux' ;
                                      Comment   : 'Première version de gestion du fichier INI.' + #13#10 + 'Certaines fonctions sont encore utilisées.' ;
                                      BugsStory : 'Version 1.0.6.0 : Restructure.' + #13#10 +
                                                  'Version 1.0.5.1 : Strange Format Font bug.' + #13#10 +
                                                  'Version 1.0.5.0 : No ItemIndex in strings R/W.' + #13#10 +
                                                  'Version 1.0.4.3 : Vista App Config Dir Bug on Destroy.' + #13#10 +
                                                  'Version 1.0.4.2 : Debuging.' + #13#10 +
                                                  'Version 1.0.4.1 : UTF 8.' + #13#10 +
                                                  'Version 1.0.4.0 : comboitems function.' + #13#10 +
                                                  'Version 1.0.3.2 : ini can be cutomized' + #13#10 +
                                                  'Version 1.0.3.1 : Function fs_GetIniDir' + #13#10 +
                                                  'Version 1.0.3.0 : Fonction fb_iniWriteFile' + #13#10 +
                                                  'Version 1.0.2.0 : Fonctions ini pour les listview,dbgrid, et virtualtrees' + #13#10 +
                                                  'Version 1.0.1.0 : Paramètre Utilisateur.' + #13#10 +
                                                  'Version 1.0.0.0 : La gestion est en place.' + #13#10 +
                                                  'On utilise plus cette unité complètement mais Fenêtre principale puis plus tard Mc Form Main INI.';
                                     UnitType : 1 ;
                                     Major : 1 ; Minor : 0 ; Release : 6 ; Build : 0 );
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//  Fonctions à appeler pour la gestion des fichiers INI
////////////////////////////////////////////////////////////////////////////////

  function FontToString(const Font:TFont):string;
  procedure StringToFont( Str:string;const Font:TFont);
  function f_IniReadGridFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TCustomDBGrid ): Boolean ;
{$IFDEF VIRTUALTREES}
  function f_IniReadVirtualTreeFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree ): Boolean ;
  procedure p_IniWriteVirtualTreeToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree );
{$ENDIF}
  function f_IniReadListViewFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TCustomListView ): Boolean ;

  procedure p_IniWriteGridToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TCustomDBGrid );
  procedure p_IniWriteListViewToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TCustomListView );

implementation

uses TypInfo,
     fonctions_ini,
     fonctions_string,
     fonctions_system,
     fonctions_dbobjects,
     fonctions_proprietes;




function FontToString(const Font:TFont):string;
begin
  Result:=Format('%s,%d,%d%d%d%d,%s', [Font.Name,Font.Size,
    Integer(fsBold in Font.Style),Integer(fsItalic in Font.Style),
      Integer(fsUnderline in Font.Style),Integer(fsStrikeOut in Font.Style),
      ColorToString(Font.Color)]);
end;

procedure StringToFont( Str:string;const Font:TFont);
const
  SEP=',';
  EXCEPT_MSG='Invalid string to font conversion.';
var
  i:Integer;
  tmpFont:TFont;
begin
  try
    // name
    i:=Pos(SEP,Str);
    if i=0 then raise EConvertError.Create(EXCEPT_MSG);
    Font.Name:=Copy(Str,1,i-1);
    Delete(Str,1,i);

    // size
    i:=Pos(SEP,Str);
    if i=0 then raise EConvertError.Create(EXCEPT_MSG);
    Font.Size:=StrToInt(Copy(Str,1,i-1));
    Delete(Str,1,i);

    // bold, italic, underline, strikethrough
    if Pos(SEP,Str)<>5 then raise EConvertError.Create(EXCEPT_MSG);
    Font.Style:= [];
    if Boolean(StrToInt(Copy(Str,1,1))) then Font.Style:=Font.Style+ [fsBold];
    if Boolean(StrToInt(Copy(Str,2,1))) then Font.Style:=Font.Style+ [fsItalic];
    if Boolean(StrToInt(Copy(Str,3,1))) then Font.Style:=Font.Style+ [fsUnderline];
    if Boolean(StrToInt(Copy(Str,4,1))) then Font.Style:=Font.Style+ [fsStrikeOut];

    Delete(Str,1,5);

    // colour
    i:=Pos(SEP,Str);
    if i=0
     then Font.Color:=StringToColor(Str)
     Else Font.Color:=StringToColor(Copy(Str,1,i-1))
  except
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : f_IniReadGridFromIni
// Description : Affecte les tailles de colonnes d'une grille à partir de l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               agd_grid     : La grille
//               Retour       : Une colonne au moins a été affectée
/////////////////////////////////////////////////////////////////////////////////

function f_IniReadGridFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TCustomDBGrid ): Boolean ;
var k, li_Width : Integer ;
    AColumns : TDBGridColumns;
begin
  Result := False ;
  AColumns := TDBGridColumns ( fobj_getComponentObjectProperty( agd_grid, CST_PROPERTY_COLUMNS));
  for k := 0 to aColumns.Count - 1 do
    Begin
{$IFDEF FPC}
      li_Width := aini_IniFile.ReadInteger( as_FormName, agd_grid.Name + '.' + (TColumn(aColumns[k])).FieldName, aColumns[k].Width);
{$ELSE}
      li_Width := aini_IniFile.ReadInteger( as_FormName, agd_grid.Name + '.' + aColumns[k].FieldName, aColumns[k].Width);
{$ENDIF}
      if li_Width > 0 Then
        Begin
          Result := True ;
          aColumns[k].Width := li_Width ;
        End ;
    End ;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : p_IniWriteGridToIni
// Description : Affecte les tailles de colonnes d'une grille vers l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               agd_grid     : La grille
/////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteGridToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TCustomDBGrid );
var k : Integer ;
  AColumns : TDBGridColumns;
begin
  AColumns := TDBGridColumns ( fobj_getComponentObjectProperty( agd_grid, CST_PROPERTY_COLUMNS));

  for k := 0 to aColumns.Count - 1 do
{$IFDEF FPC}
    aini_IniFile.WriteInteger ( as_FormName, agd_grid.Name + '.' + (Tcolumn(aColumns[k])).FieldName, aColumns[k].Width);
{$ELSE}
    aini_IniFile.WriteInteger ( as_FormName, agd_grid.Name + '.' + aColumns[k].FieldName, aColumns[k].Width);
{$ENDIF}
End ;

function flsc_GetListColumns ( const alv_ListView : TCustomListView ) : TListColumns ;
var lobj_Column : Tobject;
Begin
  lobj_Column := fobj_getComponentObjectProperty(alv_ListView, 'Columns' );
  if assigned ( lobj_Column )
  and ( lobj_Column is TListColumns ) Then
    Result := lobj_Column as TListColumns;
End;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : f_IniReadListViewFromIni
// Description : Affecte les tailles de colonnes d'une liste à partir de l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               alv_ListView : La liste
//               Retour       : Une colonne au moins a été affectée
/////////////////////////////////////////////////////////////////////////////////
function f_IniReadListViewFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TCustomListView ): Boolean ;
var k, li_Width : Integer ;
    llsc_Columns : TListColumns;
begin
  Result := False ;
  llsc_Columns := flsc_GetListColumns ( alv_ListView );
  if assigned ( llsc_Columns ) Then
    for k := 0 to llsc_Columns.Count - 1 do
      Begin
        li_Width := aini_IniFile.ReadInteger ( as_FormName, alv_ListView.Name + '.' + llsc_Columns[k].Caption, llsc_Columns[k].Width);
        if li_Width > 0 Then
          Begin
            Result := True ;
            llsc_Columns[k].Width := li_Width ;
          End ;
      End ;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : f_IniReadVirtualTreeFromIni
// Description : Affecte les tailles de colonnes d'un arbre à partir de l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               abvt_VirtualTree : L'arbre
//               Retour       : Une colonne au moins a été affectée
/////////////////////////////////////////////////////////////////////////////////

{$IFDEF VIRTUALTREES}
function f_IniReadVirtualTreeFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree ): Boolean ;
var
  lvt_EnteteArbre : TVTHeader ;
  k, li_Width : Integer ;
begin
  Result := False ;
  lvt_EnteteArbre := nil ;
  if  IsPublishedProp ( abvt_VirtualTree, 'Header'  )
  and  PropIsType     ( abvt_VirtualTree, 'Header' , tkClass)
  and ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ) is TVTHeader ) Then
    lvt_EnteteArbre := TVTHeader ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ));
  if assigned ( lvt_EnteteArbre ) Then
    for k := 0 to lvt_EnteteArbre.Columns.Count - 1 do
      Begin
        li_Width := aini_IniFile.ReadInteger( as_FormName, abvt_VirtualTree.Name + '.' + lvt_EnteteArbre.Columns[k].Text, lvt_EnteteArbre.Columns[k].Width);
        if li_Width > 0 Then
          Begin
            Result := True ;
            lvt_EnteteArbre.Columns[k].Width := li_Width ;
          End ;
      End ;
end;
/////////////////////////////////////////////////////////////////////////////////
// Fonction : p_IniWriteVirtualTreeToIni
// Description : Affecte les tailles de colonnes d'un arbre vers l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               abvt_VirtualTree : L'arbre
/////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteVirtualTreeToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree );
var k : Integer ;
    lvt_EnteteArbre : TVTHeader ;
begin
  lvt_EnteteArbre := nil ;
  if  IsPublishedProp ( abvt_VirtualTree, 'Header'  )
  and  PropIsType     ( abvt_VirtualTree, 'Header' , tkClass)
  and ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ) is TVTHeader ) Then
    lvt_EnteteArbre := TVTHeader ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ));
  if assigned ( lvt_EnteteArbre ) Then
    for k := 0 to lvt_EnteteArbre.Columns.Count - 1 do
      aini_IniFile.WriteInteger( as_FormName, abvt_VirtualTree.Name + '.' + lvt_EnteteArbre.Columns[k].Text, lvt_EnteteArbre.Columns[k].Width);
End ;
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////////
// Fonction : p_IniWriteListViewToIni
// Description : Affecte les tailles de colonnes d'une liste vers l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               alv_ListView : La liste
/////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteListViewToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TCustomListView );
var k : Integer ;
    llsc_Columns : TListColumns;
begin
  llsc_Columns := flsc_GetListColumns ( alv_ListView );
  if assigned ( llsc_Columns ) Then
    for k := 0 to llsc_Columns.Count - 1 do
      aini_IniFile.WriteInteger( as_FormName, alv_ListView.Name + '.' + llsc_Columns[k].Caption, llsc_Columns[k].Width);
End ;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_init );
{$ENDIF}
end.

