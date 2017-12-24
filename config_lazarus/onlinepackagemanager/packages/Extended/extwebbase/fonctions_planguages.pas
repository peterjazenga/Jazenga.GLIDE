unit fonctions_planguages;

interface

{$I ..\extends.inc}
{$I ..\DLCompilers.inc}
{$IFDEF FPC}
  {$MODE objfpc}{$H+}
{$ENDIF}

{

Créée par Matthieu Giroux le 01-2004

Fonctionnalités :

Création de la barre d'accès
Création du menu d'accès
Création du volet d'accès

Utilisation des fonctions
Administration

}

uses
{$IFDEF FPC}
   gettext, Translations,
{$ELSE}
   Windows, ToolWin,
{$ENDIF}

  Controls, Classes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF DELPHI_9_UP}
  WideStrings ,
{$ENDIF}
{$IFDEF TNT}
  DKLang,
{$ENDIF}
  SysUtils,fonctions_system;

var
      gb_ExisteFonctionMenu   : Boolean      ;   // Existe-t-il une fonction d'accès au menu
{$IFDEF TNT}
      Languages : TDKLanguageController= nil;
{$ELSE}
type TALanguage = Record
                   LittleLang : String;
                   LongLang   : String;
                  end;
     TTheLanguages = array of TALanguage ;
var ga_SoftwareLanguages : TTheLanguages;

{$ENDIF}
var       GS_EXT_LANGUAGES : String = '.properties';
          gs_HtmlCharset : String ={$IFDEF FPC}'utf-8'{$ELSE}'iso-8859-1'{$ENDIF};
          GS_INNER_LANG_SEPARATOR : Char = '_' ;

const // Evènements gérés
  CST_sdb_consts      = 'sdb_consts';
  CST_ldd_consts      = 'ldd_consts';
  CST_lclstrconsts    = 'lclstrconsts';
  CST_lazdatadeskstr  = 'lazdatadeskstr';
  CSt_lr_const        = 'lr_const' ;
  CST_u_languagevars  = 'u_languagevars' ;
  CST_unite_messages  = {$IFDEF FPC}'unite_messages'{$ELSE}'unite_messages_delphi'{$ENDIF};
  CST_unite_variables = 'unite_variables' ;
  CST_LNG_DIRECTORY = 'LangFiles' +DirectorySeparator ;
{$IFDEF VERSIONS}
  gver_fonctions_languages : T_Version = ( Component : 'Languages Management' ; FileUnit : 'fonctions_languages' ;
              			                 Owner : 'Matthieu Giroux' ;
              			                 Comment : 'Languages Management' ;
              			                 BugsStory : 'Version 0.9.9.0 : Centralising getting Properties.' + #13#10
                                                           + 'Version 0.9.0.0 : Created from fonctions_Objets_Dynamiques.' + #13#10;
              			                 UnitType : 1 ;
              			                 Major : 0 ; Minor : 9 ; Release : 9 ; Build : 0 );
{$ENDIF}


procedure p_RegisterALanguage ( const as_littlelang, as_longlang : String );
function fi_findLanguage  ( const as_littlelang, as_longlang : String ): Longint; overload;
function fi_findLanguage  ( const as_littlelang : String ): Longint; overload;
{$IFDEF FPC}
function GetUserLanguage: Ansistring;
function GetUserLongLanguage: Ansistring;
{$ENDIF}

function GetSystemCharset : String ;
{$IFDEF TNT}
function GetUserInfo ( const ai_LOCALEINFO : Integer ): string;
function GetLanguageCode ( ALANGID : LCID ) : string;
{$ENDIF}
procedure ChangeLanguage( iIndex : integer);
procedure p_ReplaceLanguageString ( const astl_FileToChange : TStrings ; const as_SearchedString, as_LabelToSet : String  ; const  Flags: TReplaceFlags = [] ) ; overload;
procedure p_ReplaceLanguageString ( const astl_FileToChange : TStrings ; const as_SearchedString: String  ; const  Flags: TReplaceFlags = [] ); overload;
procedure p_ReplaceLanguagesStrings ( const astl_FileToChange : TStrings ; const aa_SearchedStrings : Array of String );
function fb_LoadProperties ( const as_DirPath, as_BeginFile, as_Lang : String ; const ab_ErrorMessage : Boolean = False ):Boolean; overload;


implementation

uses
{$IFDEF TNT}
  TntSysUtils, TntSystem,
{$ENDIF}
  fonctions_string,
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
  Dialogs;

procedure p_ReplaceLanguageString ( const astl_FileToChange : TStrings ; const as_SearchedString, as_LabelToSet : String ; const  Flags: TReplaceFlags = [] ) ;
var li_i : Longint ;
    ls_line : String ;
begin
  for li_i := 0 to astl_FileToChange.Count - 1 do
    Begin
      ls_line := astl_FileToChange [ li_i ];
      if pos ( '[' + as_SearchedString + ']', ls_line ) > 0 Then
        Begin
          astl_FileToChange [ li_i ] :=  StringReplace( ls_line, '[' + as_SearchedString + ']', as_LabelToSet, Flags );
          if not (rfReplaceAll in Flags) Then
            Exit;
        end;
    end;
end;
procedure p_ReplaceLanguageString ( const astl_FileToChange : TStrings ; const as_SearchedString : String  ; const  Flags: TReplaceFlags = [] ) ;
begin
  p_ReplaceLanguageString ( astl_FileToChange, as_SearchedString, fs_GetLabelCaption(as_SearchedString), Flags);
end;

procedure p_ReplaceLanguagesStrings ( const astl_FileToChange : TStrings ; const aa_SearchedStrings : Array of String );
var li_i : Integer ;
begin
  for li_i:= low ( aa_SearchedStrings ) to High(aa_SearchedStrings) do
    Begin
      p_ReplaceLanguageString (astl_FileToChange, aa_SearchedStrings [ li_i ]);
    end;

end;

function fb_LoadProperties ( const as_DirPath, as_BeginFile, as_Lang : String ; const ab_ErrorMessage : Boolean = False ):Boolean;
Begin
  Result := fb_LoadProperties (as_DirPath+as_BeginFile+GS_INNER_LANG_SEPARATOR+as_Lang+GS_EXT_LANGUAGES, ab_ErrorMessage);
End;

procedure ChangeUnitLanguage( const as_Unit : String ; const ar_Language : TALanguage );
var ls_LangFileBegin : String;
Begin
  ls_LangFileBegin := GetAppDir + CST_LNG_DIRECTORY + as_Unit;
  if FileExists(ls_LangFileBegin + Format('.%s.po',[ar_Language.LittleLang]))
   Then Translations.TranslateUnitResourceStrings(as_Unit, ls_LangFileBegin + '.%s.po', ar_Language.LongLang, ar_Language.LittleLang)
   Else Translations.TranslateUnitResourceStrings(as_Unit, ls_LangFileBegin + '.po');

end;

procedure ChangeLanguage( iIndex : integer);
{$IFNDEF TNT}
var lr_Language : TALanguage ;
{$ENDIF}
begin
  if iIndex<0 then iIndex := 0; // When there's no valid selection in cbLanguage we use the default language (Index=0)
  {$IFDEF TNT}
  LangManager.LanguageID := LangManager.LanguageIDs[iIndex];
  {$ELSE}
  {$IFDEF FPC}
  lr_Language := ga_SoftwareLanguages [iIndex];
  ChangeUnitLanguage( CST_sdb_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_sdb_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_sdb_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_sdb_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_ldd_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_ldd_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_ldd_consts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_ldd_consts, lr_Language );
  ChangeUnitLanguage( CST_ldd_consts, lr_Language );
  ChangeUnitLanguage( CST_lclstrconsts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lclstrconsts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lclstrconsts, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lclstrconsts, lr_Language );
  ChangeUnitLanguage( CST_lazdatadeskstr, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lazdatadeskstr, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lazdatadeskstr, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lazdatadeskstr, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lazdatadeskstr, lr_Language );
  ChangeUnitLanguage( CST_lr_const, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lr_const, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lr_const, lr_Language );
  // Bug on 0.9.30 : must repeat
  ChangeUnitLanguage( CST_lr_const, lr_Language );
  // Own units
  ChangeUnitLanguage( CST_u_languagevars, lr_Language );
  ChangeUnitLanguage( CST_unite_messages, lr_Language );
  ChangeUnitLanguage( CST_unite_variables, lr_Language );
 // Translations.TranslateResourceStrings(as_Unit, fs_getSoftDir () + CST_LNG_DIRECTORY +'SoftLang.%s.po', ar_Language.LongLang, ar_Language.LittleLang);
  {$ENDIF}
  {$ENDIF}

end;

{$IFDEF TNT}
function GetLanguageCode ( ALANGID : LCID ) : string;
var
  Buffer: array [0..255] of Char;
begin
  if GetLocaleInfo(ALANGID, LOCALE_SISO639LANGNAME, @Buffer, SizeOf(Buffer)) > 0 then
    Result := LowerCase(Buffer);
end;

function GetUserInfo ( const ai_LOCALEINFO : Integer ): string;
var
  sz: Integer;
begin
  // Le premier appel nous sert uniquement à déterminer la longueur de la chaîne
  sz:= GetLocaleInfo(LOCALE_USER_DEFAULT, ai_LOCALEINFO, nil, 0);

  // Nous modifions la chaîne de résultat pour qu'elle puisse
  // contenir le texte complet.
  SetLength(result, sz - 1); // - 1 car la longueur contient le zéro terminal

  // Le deuxième appel nous retourne le nom de la langue dans la langue
  GetLocaleInfo(LOCALE_USER_DEFAULT, ai_LOCALEINFO,
    Pchar(result), sz);
End;
{$ENDIF}
function GetSystemCharset : String ;
Begin
  Result := 'ISO_8859_1' ;
End;

{$IFDEF FPC}
function GetUserLanguage: Ansistring;
var ls_Language : AnsiString;
begin
  ls_Language := '';
  GetLanguageIDs( Result, ls_Language );  //LOCALE_SNATIVELANGNAME
End;
{$ELSE}
  {$IFDEF TNT}
  function GetUserLanguage: string;
  begin
    Result := GetUserInfo ( LOCALE_SISO639LANGNAME );  //LOCALE_SNATIVELANGNAME
  End;
  {$ENDIF}
{$ENDIF}
{$IFDEF FPC}
function GetUserLongLanguage: Ansistring;
var ls_Language : AnsiString;
begin
  ls_language := '';
  GetLanguageIDs( ls_language, Result );  //LOCALE_SNATIVELANGNAME
End;
{$ELSE}
  {$IFDEF TNT}
  function GetUserLongLanguage: string;
  begin
    Result := GetUserInfo ( LOCALE_SNATIVELANGNAME );
  End;
  procedure p_RegisterLanguages ( const ame_menuLang : TMenuItem );
  var
    SR: TSearchRec;
    ls_Dir : String ;
    IsFound : Boolean;
  Begin
    ls_Dir := fs_getSoftDir + CST_LNG_DIRECTORY;
    try
      IsFound := FindFirst(ls_Dir + CST_SQL_ALL, faAnyFile, SR) = 0 ;
      while IsFound do
       begin
        if FileExists ( ls_Dir + SR.Name )
         then
          Begin
            LangManager.RegisterLangFile(ls_Dir + SR.Name);
          End ;
        IsFound := FindNext(SR) = 0;
      end;
      FindClose(SR);
    Except
      ShowMessage ( 'Error on registering lng Language files.' );
      FindClose(SR);
    End ;
    CreateLanguagesController ( ame_menuLang );
  end;
  {$ENDIF}
{$ENDIF}

function fi_findLanguage  ( const as_littlelang, as_longlang : String ): Longint;
var li_i : LongInt ;
Begin
  Result := -1;
  for li_i := 0 to high ( ga_SoftwareLanguages ) do
    with ga_SoftwareLanguages [ li_i ] do
      if  ( LittleLang = as_littlelang )
      and ( LongLang   = as_longlang   ) Then
        Result := li_i ;
end;

function fi_findLanguage  ( const as_littlelang : String ): Longint;
var li_i : LongInt ;
Begin
  Result := -1;
  for li_i := 0 to high ( ga_SoftwareLanguages ) do
    with ga_SoftwareLanguages [ li_i ] do
      if  ( LittleLang = as_littlelang ) Then
        Result := li_i ;
end;

procedure p_RegisterALanguage ( const as_littlelang, as_longlang : String );
var li_lang : Longint ;
Begin
  li_lang := fi_findLanguage  ( as_littlelang, as_longlang );
  if li_lang = -1 Then
    Begin
      SetLength(ga_SoftwareLanguages,high ( ga_SoftwareLanguages ) + 2 );
      with ga_SoftwareLanguages [ high ( ga_SoftwareLanguages ) ] do
        Begin
           LittleLang := as_littlelang;
           LongLang   := as_longlang;
        end;
    end;
end;




{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_languages );
{$ENDIF}
finalization
  p_FreeProperties;
{$IFDEF TNT}
//  Languages.Free;
//  Languages := nil;
{$ENDIF}
end.
