// unité contenant des fonctions de traitements de chaine
unit fonctions_string;

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

uses
{$IFNDEF FPC}
  Windows, MaskUtils,
{$ENDIF}
  SysUtils, StrUtils, Classes, LazUTF8Classes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Math ;

const
{$IFDEF VERSIONS}
    gVer_fonction_string : T_Version = ( Component : 'String management' ; FileUnit : 'fonctions_string' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'String traduction and format.' ;
                        			                 BugsStory : 'Version 1.1.1.0 : Adding fi_posSpace.' + #13#10 +
              			                	        	     'Version 1.1.0.0 : fs_EnlargeString function.' + #13#10 +
              			                	        	     'Version 1.0.9.0 : Adding FontToString and StringToFont.' + #13#10 +
              			                	        	     'Version 1.0.8.1 : Modify fs_SeparateTextFromWidth.' + #13#10 +
              			                	        	     'Version 1.0.8.0 : procedure p_SetStringMaxLength.' + #13#10 +
              			                	        	     'Version 1.0.7.0 : function fs_RemplaceMsgIfExists.' + #13#10 +
              			                	        	     'Version 1.0.6.0 : Creating fs_SeparateTextFromWidth.' + #13#10 +
              			                	        	     'Version 1.0.5.0 : Creating fs_ListeVersChamps.' + #13#10 +
              			                	        	     'Version 1.0.4.0 : fs_FormatText and other.' + #13#10 +
              			                	        	     'Version 1.0.3.1 : Upgrading fs_TextToFileName.' + #13#10 +
              			                	        	     'Version 1.0.3.0 : Moving function to DB functions.' + #13#10 +
              			                	        	     'Version 1.0.2.3 : UTF 8.' + #13#10 +
              			                	        	     'Version 1.0.2.2 : fs_TextToFileName of André Langlet.' + #13#10 +
              			                	        	     'Version 1.0.2.1 : Optimising.' + #13#10 +
              			                	        	     'Version 1.0.2.0 : Fonction fs_GetBinOfString.' + #13#10 +
              			                	        	     'Version 1.0.1.1 : Paramètres constantes plus rapides.' + #13#10 +
                        			                	     'Version 1.0.1.0 : Fonction fs_stringDbQuoteFilter qui ne fonctionne pas mais ne provoque pas d''erreur.' + #13#10 +
                        			                	     'Version 1.0.0.1 : Rectifications sur p_ChampsVersListe.' + #13#10 +
                        			                	     'Version 1.0.0.0 : Certaines fonctions non utilisées sont à tester.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 1 ; Release : 1 ; Build :  0);
{$ENDIF}
  CST_ENDOFLINE = #10;
  CST_DELIMITERS_CHAR = '-_ .,:[()]{}=+*';
  CST_ORD_GUILLEMENT = ord ( '''' );
  CST_ORD_POURCENT   = ord ( '%' );
  CST_ORD_ASTERISC   = ord ( '*' );
  CST_ORD_SOULIGNE   = ord ( '_' );
  CST_ORD_OUVRECROCHET   = ord ( '[' );
  CST_ORD_FERMECROCHET   = ord ( ']' );
  CST_NUMBERS = '0123456789';
  CST_ALPHABETA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

type
  tt_ValeurStrings = Array of String ; // first array : Multiple Key generated from Variant of Fieldvalues
  tt_TableauxStrings = Array of Array of String ; // first array : Multiple Key ; Second Array : Items
  TCharToUTF8Table = array[AnsiChar] of  AnsiChar;

var    gs_Lang : String = 'fr' ;

type
  TUArray = Array of Array [ 0.. 2 ] of Integer;
  TStringAArray = Array of AnsiString;
  TModeFormatText = (mftNone,mftUpper,mftLower,mftFirstIsMaj,mftFirstCharOfWordsIsMaj);

{$IFNDEF FPC}
  function fs_Dos2Win( const aText: string): string;
  function fs_Win2Dos( const aText: string): string;
  procedure AppendStr(var Dest: String; const S: String);
{$ENDIF}
  function fb_isFileChar(AChar:Char):boolean;
  function fs_TextToFileName(const Chaine:String; const ab_NoAccents :Boolean = True):AnsiString;
  function fs_getCorrectString ( const as_string : String ): String ;
  function fs_copyutf8 ( const astring : String; const apositionNonUTF8 : Int64 ; const aLengthNonUTF8 : Int64 ; const ab_strict : Boolean = False): String;
  procedure p_PutFirstCharOfWordsInMaj(var AChaine:String; const ANewWordChar : String = CST_DELIMITERS_CHAR );
  procedure p_FormatText(var Chaine:String ; const amft_Mode :TModeFormatText; const ab_NoAccents:Boolean = False );
  function fs_FormatText(const Chaine:String ; const amft_Mode :TModeFormatText; const ab_NoAccents:Boolean = False ):String;
  function fs_GetStringValue ( const astl_Labels : TStringList ; const as_Name : String ):String;
  function fs_EraseSpecialChars( const aText: string): string;
  function fs_ArgConnectString ( const as_connectstring, as_arg: string): string;
  function fs_GetBeginingOfString ( const as_text, as_endingstring: string): string;
  function fb_stringVide ( const aTexte: string): Boolean;
  function fs_stringDate(): string;
  function fs_stringDateTime(const aDateTime: TDateTime; const aFormat: string): string;
  function fs_stringCrypte( const as_Text: string): string;
  function fs_stringDecrypte( const as_Text: string): string;
  function fs_stringDecoupe( const aTexte: Tstrings; const aSep: string): string;
  function fs_stringChamp( const aString, aSep: string; aNum: Word): string;
  function ft_stringConstruitListe( const aTexte, aSep: string): TStrings;
  function fb_stringConstruitListe( const aTexte: string ; var aa_Result : TUArray ):Boolean;
  function fs_convertionCoordLambertDMS( const aPosition: string; aLongitude: Boolean): string;
  function fe_convertionCoordLambertDD( const aPosition: string): Extended;
  function fe_distanceEntrePointsCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string): Extended;
  function fb_controleDistanceCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string; const aDistance: Extended): Boolean;
  procedure p_ChampsVersListe(var astl_ChampsClePrimaire: TStrings; const aws_ClePrimaire : String ; ach_Separateur : Char );
  procedure p_SetStringMaxLength    ( var  as_string : String ; const ai_Maxlength : Integer );
  function fs_ListeVersChamps ( var astl_ChampsClePrimaire: TStrings; ach_Separateur : AnsiChar ):string;
  function fs_RemplaceMsg(const as_Texte: String; const aTs_arg: Array of String): String;
  function fs_RemplaceMsgIfExists(const as_Texte: String; const as_arg: String): String;
  function fi_PosSpace ( const as_Texte : String ; const ai_pos : Integer ): Integer ;
  function fs_RemplaceEspace ( const as_Texte : String ; const as_Remplace : String ): String ;
  function fs_EnlargeString  ( const as_texttoGet, as_includeChars, as_excludeChars : String ; const ai_pos : Integer; const ALeft : Boolean ):String ;

  function fs_RepeteChar     ( const ach_Caractere : Char ; const ali_Repete : Integer ):String ;
  function fi_CharCounter    ( const as_Texte : String ; const ach_Caractere : Char ):Longint;
  function fs_RemplaceChar   ( const as_Texte : String ; const ach_Origine, ach_Voulu : Char ) : String ;

  function fs_ReplaceChaine( as_Texte : String ; const as_Origine, as_Voulu : string):string;
  function fs_LastString(const ATextSeparator : string;const as_Texte : String ):Integer;
  function fs_GetBinOfString ( const astr_Source: AnsiString ): String;
  function fs_AddComma ( const as_Chaine : String ) : String ;
  function fs_Lettrage ( const ach_Lettrage: Char;
                         const ai64_Compteur : Int64 ;
                         const ali_TailleLettrage : Longint ): String ;
  function HexToByte(c: char): byte;
  function HexToBinary ( const ALines : TStrings ; const AStream : TStream ): Boolean;
  function fs_ReplaceEndOfLines ( const ALines : TStrings; const as_replace : String = '\n' ): String;
  function fs_IfThen ( const ab_IfTest : Boolean; const as_IfTrue, as_IfFalse : String ) : String;
  //traductions in FCL
  function fs_GetLabelCaption ( const as_Name : String ):WideString;

  var    gstl_Labels             : TStringlistUTF8 = nil ;

implementation

{$IFDEF FPC}
uses LazFileUtils, unite_messages,LazUTF8,
{$ELSE}
uses JclStrings, fonctions_system, unite_messages_delphi,
{$ENDIF}
     fonctions_dialogs;



const  SansAccents : TCharToUTF8Table
           = (#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,#16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,#32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,#48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,#64,
              'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',#91,#92,#93,#94,#95,#96,
              'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',#123,#124,#125,#126,#127,
              'A','A','A','A','A','A','A','C','E','E','E','E','I','I','I','I','D','N','O','O','O','O','O','x','O','U','U','U','U','Y','D','B',
              'a','a','a','a','a','a','a','c','e','e','e','e','i','i','i','i','o','n','o','o','o','o','o','/','o','u','u','u','u','y','d','y',
              #192,#193,#194,#195,#196,#197,#198,#199,#200,#201,#202,#203,#204,#205,#206,#207,#208,#209,#210,#211,#212,#213,#214,#215,#216,#217,#218,#219,#220,#221,#222,#223,#224,#225,#226,#227,#228,#229,#230,#231,#232,#233,#234,#235,#236,#237,#238,#239,#240,#241,#242,#243,#244,#245,#246,#247,#248,#249,#250,#251,#252,#253,#254,#255);

function fs_LastString(const ATextSeparator : string;const as_Texte : String ):Integer;
Begin
  if (ATextSeparator > '') Then
   Begin
    Result := pos ( ATextSeparator, as_Texte );
    while posEx ( ATextSeparator, as_Texte, Result+1 )>0 do
      Result:= posEx ( ATextSeparator, as_Texte, Result+1 );
   End
  Else
   Result:=-1;
end;

function HexToByte(c: char): byte;
begin
  case c of
    '0'..'9' : Result:=ord(c)-$30;
    'A'..'F' : Result:=ord(c)-$37; //-$41+$0A
    'a'..'f' : Result:=ord(c)-$57; //-$61+$0A
  else
    raise EConvertError.Create(GS_STRING_MUST_BE_HEXA);
  end;
end;
procedure p_SetStringMaxLength    ( var  as_string : String ; const ai_Maxlength : Integer );
Begin
  if Length(as_string)>ai_Maxlength Then
     as_string:=copy(as_string,1,ai_Maxlength);
End;
function fs_IfThen ( const ab_IfTest : Boolean; const as_IfTrue, as_IfFalse : String ) : String;
Begin
  if ab_IfTest
   Then Result := as_IfTrue
   else Result := as_IfFalse;
End;

function HexToBinary ( const ALines : TStrings ; const AStream : TStream ): Boolean;
var I : Integer;
    AByte : Byte;
    AChar, EndChar : PChar;
    ls_Line : String;
Begin
  Result:=False;
  for i := 0 to ALines.Count - 1 do
    Begin
     ls_Line := ALines [ i ];
     if length ( ls_Line ) > 0 Then
       try
         AChar:=@ls_Line[1];
         EndChar:=@ls_Line[Length(ls_Line)];
         while AChar <= EndChar do
           Begin
             AByte := HexToByte(AChar^);
             if not ( AChar^ in [' ',#13,#10]) then
               AStream.{$IFDEF FPC}WriteByte{$ELSE}Write{$ENDIF}
                       (abyte{$IFNDEF FPC},1{$ENDIF});
             inc ( AChar );
           end;
         Result:=true;
       except
         on EConvertError do Abort;
       end;
    end;
end;

function fb_isFileChar(AChar:Char):boolean;
Begin
  Result := AChar in ['0'..'9','A'..'z','-','.'];
end;

function fs_ReplaceWithTable(const s: AnsiString; const Table: TCharToUTF8Table ): AnsiString;
var
  i: Integer;
  Dest: PAnsiChar;
begin
  Result:=s;
  if s='' then
    exit;
  Dest:=@Result[1];
  for i:=1 to length(s) do begin
    Dest^:=Table[Dest^];
    inc(Dest);
  end;
end;


///////////////////////////////////////////////////////////////////////////////
//  FONCTIONS de conversion de caractères Dos <=> Windows et vice-versa
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// fonction : fs_Dos2Win
// Description : Convertit un texte OEM en ANSI
// aText : Le texte OEM
// Résultat : Le texte transformé en ANSI
///////////////////////////////////////////////////////////////////////////////
{$IFDEF DELPHI}
function fs_Dos2Win( const aText: string): string;
begin
  if aText = '' then Exit;
  SetLength(Result, Length(aText));
  OemToChar(@(aText), @(Result));
end;

///////////////////////////////////////////////////////////////////////////////
// fonction : fs_Win2Dos
// Description : Convertit un texte ANSI en OEM
// aText : Le texte ANSI
// Résultat : Le texte transformé en OEM
///////////////////////////////////////////////////////////////////////////////
function fs_Win2Dos( const aText: string): string;
begin
  if aText = '' then Exit;
  SetLength(Result, Length(aText));
  CharToOem(@(aText), @(Result));
end;
{$ENDIF}
///////////////////////////////////////////////////////////////////////////////
// fonction : fs_ArgConnectString
// Description :  Renvoie les données d'un argument d'une chaîne de connexion
// as_connectstring : La chaîne de connexion
// as_arg : Le nom de l'argument à récupérer
// Résultat : Les données de l'argument paramètres
///////////////////////////////////////////////////////////////////////////////
function fs_ArgConnectString ( const as_connectstring, as_arg: string): string;
var
  li_pos: integer;
  ls_chaine: string;

begin
  ls_chaine := as_connectstring;
  li_pos    := Pos(as_arg, ls_chaine);
  ls_chaine := RightStr(ls_chaine, Length(ls_chaine) - (li_pos + Length(as_arg)));
  li_pos    := Pos(';', ls_chaine);
  if li_pos > 0 then
    Result := LeftStr(ls_chaine, li_pos - 1)
  else
    Result := ls_chaine;
end;

///////////////////////////////////////////////////////////////////////////////
// fonction : fb_stringVide
// Description :  Renvoie True si le texte est blanc(s) ou NULL
// aTexte : La chaîne à tester
// Résultat : Renvoie True si le texte est blanc(s) ou NULL
///////////////////////////////////////////////////////////////////////////////
function fb_stringVide( const aTexte: string): Boolean;
begin
  Result := (Trim(aTexte) = '') or (aTexte = EmptyStr);
end;

///////////////////////////////////////////////////////////////////////////////
//  Cette fonction renvoie la date sous le format standard en string
///////////////////////////////////////////////////////////////////////////////
function fs_stringDate(): string;
begin
  result := DateTimeToStr(Now);
end;

///////////////////////////////////////////////////////////////////////////////
//  Cette fonction renvoie la date ou l'heure sous un format précis en string
///////////////////////////////////////////////////////////////////////////////
function fs_stringDateTime( const aDateTime: TDateTime; const aFormat: string):string;
begin
  DateTimeToString(result, aFormat, aDateTime);
end;

///////////////////////////////////////////////////////////////////////////////
//  Fonction pour crypter une chaîne
///////////////////////////////////////////////////////////////////////////////
function fs_stringCrypte( const as_Text: string): string;
var
  li_pos, li_i: integer;
  ls_text: string;

begin
  li_i := 62;
  ls_text := as_Text;
  for li_pos := 1 to Length(ls_text) do
    ls_text[li_pos] := Chr(Ord(ls_text[li_pos]) + li_i + li_pos);
  result := ls_text;
end;

///////////////////////////////////////////////////////////////////////////////
//  Fonction pour décrypter une chaîne
///////////////////////////////////////////////////////////////////////////////
function fs_stringDecrypte( const as_Text: string): string;
var
  li_pos, li_i: integer;
  ls_text: string;

begin
  li_i := 62;
  ls_text := as_Text;
  for li_pos := 1 to Length(ls_text) do
    ls_text[li_pos] := Chr(Ord(ls_text[li_pos]) - li_i - li_pos);
  Result := ls_text;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction qui découpe la chaine suivant le séparateur et renvoie la première partie.
///////////////////////////////////////////////////////////////////////////////
function fs_stringDecoupe( const aTexte: TStrings; const aSep: string): string;
// Cherche la première occurence du séparateur dans la chaine,
// découpe le morceau placé avant et le renvoie.
// La chaine passée en référence ne contient plus que le reste.
var
  i_p: integer;
  s_ret: string;

begin
  // position du séparateur
  i_p := Pos(aSep, aTexte.GetText );

  if i_p = 0 then
    begin
      s_ret := aTexte.GetText;
      aTexte.Text   := '';
    end
  else
    begin
      s_ret := MidStr(aTexte.Strings[0], 1, i_p - 1);
      aTexte.Text := MidStr(aTexte.Text, i_p + Length(aSep), Length(aTexte.GetText));
    end;
  result:= s_ret;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction ramenant une liste de string en supprimant le séparateur
///////////////////////////////////////////////////////////////////////////////
function ft_stringConstruitListe( const aTexte, aSep: string): TStrings;
var t_liste, t_chaine:TStrings;
begin
  // Exemple:
  // Si aTexte = "aaa;bbbb;cc;ddddd;eeee"
  // et aSep    = ";"
  // alors la fonction renvoie TStrings de 5 lignes
  t_liste := TStringList.Create;
  t_chaine := TStringList.Create;
  t_chaine.Text := aTexte;
  while not fb_stringVide(t_chaine.Text) do
  begin
    t_liste.add(fs_stringDecoupe(t_chaine,aSep));
  end;

  result := t_liste;
  t_liste.Free;
  t_chaine.Free;
end;


///////////////////////////////////////////////////////////////////////////////
//Fonction ramenant le Nieme champ d'une chaîne avec séparateur.
///////////////////////////////////////////////////////////////////////////////
  // Exemple:
  // Si aString = "aaa;bbbb;cc;ddddd;eeee"
  // et aSep    = ";"
  // et aNum    = 3
  // alors la fonction renvoie "cc"
function fs_stringChamp( const aString, aSep: string; aNum: word): string;
var i_pos1, i_pos2, li_compteur: integer;
begin
  // Initialisation
  Result := '';
  li_compteur := 0;
  i_pos1 := 1;
  if aNum < 1 then
    Exit; // Si on cherche à 0 : on quitte

  // Tant qu'on n'est pas rendu à anum et qu'il y a des champs
  while (li_compteur < aNum) and (i_pos1 <> 0) do
    begin
      // Incrémentation
      inc(li_compteur);
      // Si toujours inférieur au suivant
      if li_compteur < aNum then
        begin
          // Incrémente la position au suivant
          i_pos1 := posEx(aSep, aString, i_pos1) + 1;
          // Passe au suivant
          Continue;
        end;
      // Sinon récupération de la position de fin
      i_pos2 := posEx(aSep, aString, i_pos1);
      if i_pos2 = 0 then i_pos2 := Length(aString) + 1;
      // Et de la chaîne incluse
      Result := MidStr(aString, i_pos1, i_pos2 - i_pos1);
    end;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction de convertion d'une Position degré décimale en degré minutes secondes.
///////////////////////////////////////////////////////////////////////////////
function fs_convertionCoordLambertDMS( const aPosition: string; aLongitude: Boolean): string;
var
  ls_value: Extended;
  ls_Result, ls_mesure, ls_coord: string;

begin
  // Exemple:
  // aPosition = "48.98166666667"
  // aLongitude = true ;dans le cas où il s'agit d'une longitude ou une latitude
  // retourne String = "E 48°58'54''"
  ls_value := StrToFloat (aPosition);
  if aLongitude then
    if ls_value > 0 then
      ls_coord := 'E'
    else
      ls_coord := 'O'
  else
    if ls_value > 0 then
      ls_coord := 'N'
    else
      ls_coord := 'S';

  ls_value := abs(ls_value);

  ls_Result := ls_coord+' ';

  ls_mesure := '°';
  ls_Result := ls_Result + FormatFloat ('00',int(ls_value))+ls_mesure;

  ls_mesure := '''';
  ls_value := Frac(ls_value)*60;
  ls_Result := ls_Result + FormatFloat ('00',int(ls_value))+ls_mesure;

  ls_mesure := '''''';
  ls_value := Frac(ls_value)*60;
  ls_Result := ls_Result + FormatFloat ('00',int(ls_value))+ls_mesure;

  result := ls_Result;
end;

///////////////////////////////////////////////////////////////////////////////
//  Fonction de convertion d'une Position degré minutes secondes en degré décimale
///////////////////////////////////////////////////////////////////////////////
function fe_convertionCoordLambertDD( const aPosition: string): Extended;
var
  ls_string, ls_minutes, ls_degres, ls_secondes, ls_axe: string;
  li_signe, li_pos_deg, li_pos_min, li_pos_sec: integer;

begin
  // Exemple:
  // aPosition = "E 48°58'54''"
  // retourne String = "48.98166666667"
  li_signe :=1;
  ls_string := aPosition;

  ls_axe := MidStr(ls_string,0,1);
  if (ls_axe = 'O') or (ls_axe = 'S') then li_signe := -1;

  li_pos_deg := Pos('°',ls_string);
  li_pos_min := Pos('''',ls_string);
  li_pos_sec := Pos('''''',ls_string);

  ls_degres := MidStr(ls_string,3,li_pos_deg-3);
  ls_minutes  := MidStr(ls_string,li_pos_deg+1,li_pos_min-(li_pos_deg+1));
  ls_secondes:= MidStr(ls_string,li_pos_min+1,li_pos_sec-(li_pos_min+1));

  result := li_signe * (StrToFloat(ls_degres) + (StrToFloat(ls_minutes)/ 60)+(StrToFloat(ls_secondes)/3600));
end;

///////////////////////////////////////////////////////////////////////////////
// Fonction qui calcul la distance entre deux points =  Orthodromie
// Une route orthodromique entre deux points de la surface terrestre est représentée
// par le trajet réél le plus court possible entre ces deux points.
///////////////////////////////////////////////////////////////////////////////
function fe_distanceEntrePointsCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string): Extended;
var le_latitudedep, le_latitudearr, le_longitudedep, le_longitudearr: Extended;
begin
  le_latitudedep  := fe_convertionCoordLambertDD(aLatitudeDep);
  le_latitudearr  := fe_convertionCoordLambertDD(aLatitudeArr);
  le_longitudedep := fe_convertionCoordLambertDD(aLongitudeDep);
  le_longitudearr := fe_convertionCoordLambertDD(aLongitudeArr);

  // 6366 correspond au rayon moyen de la terre en KM.
  // Formule de l'orthodromie :
  // Ortho(A,B)=6366 x acos[cos(LatA) x cos(LatB) x cos(LongB-LongA)+sin(LatA) x sin(LatB)]
  Result := 6366 * ArcCos((sin(DegToRad(le_latitudedep)) * sin(DegToRad(le_latitudearr)))
            + (cos(DegToRad(le_latitudedep)) * cos(DegToRad(le_latitudearr))
            * cos(DegToRad(le_longitudedep) - DegToRad(le_longitudearr))));
end;


///////////////////////////////////////////////////////////////////////////////
// Fonction : fb_controleDistanceCoordLambert
// description : permet de vérifier qu'un point d'arrivée
// se trouve dans le périmètre du cercle dont le centre est le point de départ
// avec un rayon de aDistance KM
// aLatitudeDep : Lattitude de départ
// aLongitudeDep : Longitude de départ
// aLatitudeArr : Lattitude d'arrivée
// aLongitudeArr : Longitude d'arrivée
// aDistance     : Distance minimale reliant les deux points
///////////////////////////////////////////////////////////////////////////////
function fb_controleDistanceCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string; const aDistance: Extended): Boolean;
var le_result: Extended;
begin
  // distance entre les deux points
  le_result := fe_distanceEntrePointsCoordLambert(aLatitudeDep,aLongitudeDep,aLatitudeArr,aLongitudeArr);

  // vérifie si la distance est inférieure ou supérieure au rayon
  if le_result > aDistance then
    result := False
  else
    result := True;
end;


{$IFDEF COMPILER_10_UP}
procedure AppendStr(var Dest: String; const S: String);
begin
  Dest := Dest + S;
end;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// Fonction   : fs_ListeVersChamps
// Description : Création d'une liste à partir d'une chaîne avec des séparateurs
// astl_ChampsClePrimaire : Les champs listés en entrée
// as_Separateur        : Le séparateur
////////////////////////////////////////////////////////////////////////////////
function fs_ListeVersChamps ( var astl_ChampsClePrimaire: TStrings; ach_Separateur : AnsiChar ):string;
var li_i : Integer;
Begin
  Result:='';
  for li_i := 0 to astl_ChampsClePrimaire.Count - 1  do
    if li_i = 0
      Then Result := astl_ChampsClePrimaire [ li_i ]
      Else AppendStr(Result,ach_Separateur+ astl_ChampsClePrimaire [ li_i ]);
end;

////////////////////////////////////////////////////////////////////////////////
// Procédure   : p_ChampsVersListe
// Description : Création d'une liste à partir d'une chaîne avec des séparateurs
// astl_ChampsClePrimaire : Les champs listés en sortie
// as_ClePrimaire       : Les champs en entrée
// as_Separateur        : Le séparateur
////////////////////////////////////////////////////////////////////////////////
procedure p_ChampsVersListe(var astl_ChampsClePrimaire: TStrings; const aws_ClePrimaire : String ; ach_Separateur : Char );
var ls_TempoCles: String;
begin
  // Création des champs
  astl_ChampsClePrimaire.Free;
  astl_ChampsClePrimaire := TStringList.Create;
  ls_TempoCles := aws_ClePrimaire;
  if  pos(ach_Separateur, ls_TempoCles) = 0 then
    // Ajout du champ si un champ
    Begin
      if Trim ( ls_TempoCles ) <> '' Then
        astl_ChampsClePrimaire.Add(Trim(ls_TempoCles));
    End
  else
    // si plusieurs champs
    begin
      while pos(ach_Separateur, ls_TempoCles) > 0 do
        begin
          // Ajout des champs
          astl_ChampsClePrimaire.Add(Trim(Copy(ls_TempoCles, 1, Pos(ach_Separateur, ls_TempoCles) - 1)));
          ls_TempoCles := Copy(ls_TempoCles, Pos(ach_Separateur, ls_TempoCles) + 1, Length(ls_TempoCles));
        end;
      // Ajout du dernier champ
      astl_ChampsClePrimaire.Add(Trim(ls_TempoCles));
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_RemplaceMsg
// Description : remplace dans un text @ARG par un tableau d'arguments
// as_Texte : Texte source
// aTs_arg  : Chaînes à mettre à la place de @ARG
// Résultat : la chaîne avec les arguments
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceMsg(const as_Texte: String; const aTs_arg: Array of String): String;
var
  ls_reduct: String;
  li_pos, li_i: integer;

begin
  Result := '';
  ls_reduct := as_texte;
  li_pos := Pos('@ARG', ls_reduct);
  li_i := 0;

  while li_pos > 0 do
    begin
      Result := Result + LeftStr(ls_reduct, li_pos - 1) + ats_arg[li_i];
      ls_reduct := RightStr(ls_reduct, Length(ls_reduct) - (li_pos + 3));
      li_pos := Pos('@ARG', ls_reduct );
      li_i := li_i + 1;
    end;

  Result := Result + ls_reduct;
end;

////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_RemplaceMsgIfExists
// Description : remplace dans un text @ARG par un argument s'il existe
// as_Texte : Texte source
// as_arg  : Chaîne à tester à mettre à la place de @ARG
// Résultat : la chaîne avec les arguments
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceMsgIfExists(const as_Texte: String; const as_arg: String): String;
Begin
  if as_arg = ''
   Then Result := ''
   Else Result := fs_RemplaceMsg ( as_Texte, [as_arg] );

end;

////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_RemplaceEspace
// Description : remplace les espaces et le caractère 160 par une chaîne
// as_Texte : Texte source
// as_Remplace : chaîne remplaçant l'espace ou le cractère 160
// Résultat : la chaîne sans les espaces
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceEspace ( const as_Texte : String ; const as_Remplace : String ): String ;
var lpc_AChar : PChar ;
    lli_i, li_length : LongInt;
Begin
  Result := '';
  li_length := length ( as_Texte );
  if (li_length = 0 ) Then
    Exit;
  lpc_AChar := @as_Texte[1];
  for lli_i := 1 to li_length do
    Begin
      if ( lpc_AChar^ = ' ' )
      or ( lpc_AChar^ = {$IFDEF FORMATSETTING}FormatSettings.{$ENDIF}ThousandSeparator {160} )
        Then AppendStr ( Result, as_Remplace )
        Else AppendStr ( Result, lpc_AChar^ );
      inc ( lpc_AChar );
    end;
End ;

////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_RemplaceEspace
// Description : remplace les espaces et le caractère 160 par une chaîne
// as_Texte : Texte source
// as_Remplace : chaîne remplaçant l'espace ou le cractère 160
// Résultat : la chaîne sans les espaces
////////////////////////////////////////////////////////////////////////////////
function fi_PosSpace ( const as_Texte : String ; const ai_pos : Integer ): Integer ;
var lpc_AChar : PChar ;
    lli_i, li_length : LongInt;
Begin
  Result := 0;
  li_length:=length ( as_Texte );
  if ( li_length= 0)
  or ( ai_pos < 1 ) Then
    Exit;
  lpc_AChar := @as_Texte[ai_pos];
  for lli_i := 1 to li_length do
    Begin
      if ( lpc_AChar^ = #32 )
      or ( lpc_AChar^ = #20 )
        Then
         Begin
           Result := lpc_AChar - @as_Texte[1]+1;
           Exit;
         end;
      inc ( lpc_AChar );
    end;
  Result := li_length;
End ;

////////////////////////////////////////////////////////////////////////////////
// fonction : fs_RepeteChar
// Description : Répète un carctère n fois
// ach_Caractere  : Le caractère à répéter
// ali_Repete     : Le nombre de répétitions du caractère
// Résultat       : la chaîne avec le caractère répété
////////////////////////////////////////////////////////////////////////////////
function fs_RepeteChar     ( const ach_Caractere : Char ; const ali_Repete : Integer ):String ;
var lpc_AChar : PChar ;
    li_i : Integer;
Begin
  SetLength ( Result, ali_Repete );
  if ali_Repete = 0 Then
    Exit;
  lpc_AChar := @Result[1];
  for li_i := 1 to ali_Repete do
    Begin
      lpc_AChar^ := ach_Caractere;
      inc ( lpc_AChar );
    end;
End ;

////////////////////////////////////////////////////////////////////////////////
// fonction : fs_RepeteChar
// Description : Répète un carctère n fois
// ach_Caractere  : Le caractère à répéter
// ali_Repete     : Le nombre de répétitions du caractère
// Résultat       : la chaîne avec le caractère répété
////////////////////////////////////////////////////////////////////////////////
function fs_EnlargeString  ( const as_texttoGet, as_includeChars, as_excludeChars : String ; const ai_pos : Integer; const ALeft : Boolean ):String ;
var lpc_AChar,lpc_ACharLimit : PChar ;
    li_i : Integer;
    lb_exclude,lb_Include : Boolean;
Begin
  Result:='';
  if ai_pos < 0 Then
    Exit;
  lpc_AChar := @as_texttoGet[ai_pos];
  if ALeft
   Then lpc_ACharLimit := @as_texttoGet[1]
   Else lpc_ACharLimit := @as_texttoGet[length(as_texttoGet)];
  lb_exclude:=as_excludeChars>'';
  lb_Include:=as_includeChars>'';
  while ( lb_Include and (pos (lpc_AChar^,as_includeChars)>0))
  or    ( lb_exclude and (pos (lpc_AChar^,as_excludeChars)=0)) do
    Begin
      if ALeft
       Then
         Begin
          Result:=lpc_AChar^+Result;
          dec ( lpc_AChar );
          if lpc_AChar<lpc_ACharLimit Then Exit;
         end
       else
        Begin
         AppendStr(Result,lpc_AChar^);
         inc ( lpc_AChar );
         if lpc_AChar>lpc_ACharLimit Then Exit;
        end;
    end;
End ;

function fi_CharCounter    ( const as_Texte : String ; const ach_Caractere : Char ):Longint;
var lpc_AChar : PChar ;
    li_length : Int64;
Begin
  Result:=0;
  if as_Texte = '' Then
    Exit;
  lpc_AChar := @as_Texte[1];
  li_length := Length(as_Texte)+Int64(@as_Texte[1]);
  while Int64(lpc_AChar) < li_length do
    Begin
      if lpc_AChar^ = ach_Caractere Then
        inc ( Result );
      inc ( lpc_AChar );
    end;

end;

////////////////////////////////////////////////////////////////////////////////
// fonction : fs_RemplaceChar
// Description : Remplace un caractère par un autre dans une chaîne
// as_Texte       : Le texte à modifier
// ach_Origine    : Le caractère à remplacer
// ach_Voulu      : Le caractère de remplacement
// Résultat       : la chaîne avec le caractère de remplacement
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceChar   ( const as_Texte : String ; const ach_Origine, ach_Voulu : Char ) : String ;
var lpc_AChar : PChar ;
    li_i, li_length : LongInt;
Begin
  Result := as_Texte;
  li_length := length ( Result );
  if (li_length = 0 )
  or (ach_Origine = ach_Voulu) Then
    Begin
     Exit;
    end;
  lpc_AChar := @Result[1];
  for li_i := 1 to li_length do
    Begin
      if lpc_AChar^ = ach_Origine Then
        lpc_AChar^ := ach_Voulu;
      inc ( lpc_AChar );
    end;
End ;


////////////////////////////////////////////////////////////////////////////////
// fonction : fs_ReplaceChaine
// Description : Remplace un caractère par un autre dans une chaîne
// as_Texte       : Le texte à modifier
// as_Origine    : La chaîne à remplacer
// as_Voulu      : La chaîne de remplacement
// Résultat       : la chaîne modifiée
////////////////////////////////////////////////////////////////////////////////
function fs_ReplaceChaine( as_Texte : String ; const as_Origine, as_Voulu : string):string;
var li_pos1:integer;
begin
  li_pos1:=pos(as_Origine,as_Texte);

  Result :='';

  while (li_pos1<>0) do
  begin
  Result:= Result +copy(as_Texte,1,li_pos1-1)+ as_Voulu ;
  as_Texte:=copy(as_Texte,li_pos1+length(as_Origine),length(as_Texte)+1-(li_pos1+length(as_Origine)));    //le fait sauf au dernier passage
  li_pos1:=pos(as_Origine,as_Texte);
  end;
  Result := Result +as_Texte;
end;

////////////////////////////////////////////////////////////////////////////////
// Procédure : p_AddBinToString
// Description : Renvoie la version hexadécimale d'une chaine non ansi
// ast8_Abin   : Chaine qui doit être non ansi
// Résultat    : Résultat en hexa
////////////////////////////////////////////////////////////////////////////////

function fs_GetBinOfString ( const astr_Source: AnsiString ): String;
var
  C, L : Integer;
begin
  Result := '';
  if astr_Source <> '' then
  begin
    L := Length(astr_Source);
    C := 1;
    while C <= L do
    begin
      Result := Result + IntToHex( Byte(astr_Source[C]), 2 );
      Inc(C, 1);
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_Lettrage
// Description : crée un lettrage si le champ compteur est une chaîne
// Paramètres : ach_Lettrage : La lettre du compteur
//              ai64_Compteur : Le nombre du lettrage
//              ali_TailleLettrage : La longueur du champ lettrage
/////////////////////////////////////////////////////////////////////////////////
function fs_Lettrage ( const ach_Lettrage: Char;
                       const ai64_Compteur : Int64 ;
                       const ali_TailleLettrage : Longint ): String ;

Begin
  Result := ach_Lettrage + fs_RepeteChar ( '0', ali_TailleLettrage - length ( IntToStr ( ai64_Compteur )) - 1 ) + IntToStr ( ai64_Compteur );
End ;

function fs_AddComma ( const as_Chaine : String ) : String ;
Begin
  if as_Chaine <> ''
   Then  Result := ' (' +as_Chaine+')'
   Else  Result := '';
End;


function fs_GetStringValue ( const astl_Labels : TStringList ; const as_Name : String ): String;
var ls_temp:  String;
Begin
  if astl_Labels = nil Then
   Begin
     Result := as_Name ;
     exit;
   End;
  ls_temp := astl_Labels.Values [ as_Name ];
  Result  := fs_getCorrectString ( ls_temp );
  if ( Result = '' ) then
    Result := as_Name ;
End;

function fs_getCorrectString ( const as_string : String ): String ;
Begin
  {$IFDEF windows}
  {$IFDEF FPC}
  if  ( DLLreason = 0 )
  Then Result  := as_string
  Else Result  := UTF8decode ( as_string );
  {$ELSE}
  Result  := UTF8decode ( as_string );
  {$ENDIF}
  if ( Result = '' ) then
    Result := as_string ;
  {$ELSE}
  Result := as_string;
  {$ENDIF}

end;

{$IFNDEF FPC}
function fs_ReplaceAccents(const AInput: AnsiString): AnsiString;
const
  CodePage = 20127; //20127 = us-ascii
var
  WS: WideString;
begin
  WS := WideString(AInput);
  SetLength(Result, WideCharToMultiByte(CodePage, 0, PWideChar(WS),
    Length(WS), nil, 0, nil, nil));
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), Length(WS),
    PAnsiChar(Result), Length(Result), nil, nil);
end;
{$ENDIF}

procedure p_PutFirstCharOfWordsInMaj(var AChaine:String; const ANewWordChar : String = CST_DELIMITERS_CHAR);
var AChar :PChar;
    EndChar : PChar;
    lb_isnewword : Boolean;
    ls_temp,
    ls_string : String;
    {$IFDEF FPC}
    li_length : Integer;
    {$ENDIF}
begin
  if AChaine = '' Then
   Exit;
  AChar:=@AChaine[1];
  ls_string := '';
  EndChar:=@AChaine[Length(AChaine)];
  lb_isnewword:=True;
  while AChar<=EndChar do
    begin
     {$IFDEF FPC}
      li_length := UTF8CharacterLength ( AChar );
      ls_temp:=copy(AChaine,Int64(AChar-EndChar+@AChaine[1]),li_length);
      //TempChar := @ls_temp[1];
      SetLength(ls_temp, li_length);
      System.Move(AChar^, ls_temp[1], li_length);
      if lb_isnewword
       Then AppendStr(ls_string,UTF8UpperCase(ls_temp,gs_Lang))
       Else AppendStr(ls_string,UTF8LowerCase(ls_temp,gs_Lang));
      {$ELSE}
      if lb_isnewword
       Then AppendStr(ls_string,UpperCase(AChar^)[1])
       Else AppendStr(ls_string,LowerCase(AChar^)[1]);
      {$ENDIF}
      lb_isnewword:=pos(AChar^,ANewWordChar)>0;
      inc (Achar{$IFDEF FPC},li_length{$ENDIF});
    end;
  AChaine:=ls_string;
end;

// just for utf8
// copy a string from real aposition and real alength
// Because a utf8 char has a length between 1 and more.
// strict length includes double or triple length of special  chars
function fs_copyutf8 ( const astring : String; const apositionNonUTF8 : Int64 ; const aLengthNonUTF8 : Int64 ; const ab_strict : Boolean = False): String;
var AChar :PChar;
    li_length : Int64;
    EndChar : PChar;
    {$IFDEF FPC}
    li_pos ,
    li_charlength : Integer;
    PosChar : PChar;
    {$ENDIF}
begin
  li_length:=Length(astring);

  // less use of cpu
  if  ( apositionNonUTF8 = 1 )
  and (li_length <= aLengthNonUTF8) Then
   Begin
     Result:=astring;
     Exit;
   end;
  if (astring = '')
  or ( apositionNonUTF8 > li_length ) Then
   Begin
    Result:='';
    Exit;
   end;

  {$IFDEF FPC}
  AChar:=@astring[1];
  PosChar:=AChar;
  EndChar:=@astring[li_length];
  li_length:=0;
  while AChar<=EndChar do
    begin
      li_charlength := UTF8CharacterLength ( AChar );
      if ab_strict // strict calculate total lenght
      and (AChar-PosChar+li_charlength>aLengthNonUTF8) Then
        Break;
      inc (li_length);
      // adapt position behore increasing pchar
      if apositionNonUTF8 = li_length Then
       Begin
         li_pos:=AChar-@astring[1]+1;
         PosChar:=AChar;
       end;
      inc (Achar,li_charlength);
      // not strict : visible lenght
      if not ab_strict and (li_length -apositionNonUTF8 + 1 = aLengthNonUTF8) Then
        Break;
    end;
  Result:=copy(astring,li_pos,AChar-@astring[1]);
  {$ELSE}
  Result:=copy(astring,apositionNonUTF8,aLengthNonUTF8);
  {$ENDIF}
end;
function fs_GetBeginingOfString ( const as_text, as_endingstring: string): string;
var AChar :PChar;
    EndChar : PChar;
begin
  Result:='';
  if as_text = '' Then
   Exit;
  AChar:=@as_text[1];
  EndChar:=@as_text[Length(as_text)];
  while AChar<=EndChar do
    begin
      if pos(AChar^,as_endingstring)>0
       Then Exit
       Else AppendStr(Result,AChar^);
      inc (Achar);
    end;
end;

// function fs_TextWithoutAccent
// text with no special caracters
procedure p_FormatText(var Chaine:String ; const amft_Mode :TModeFormatText; const ab_NoAccents:Boolean = False );
begin
  if not ab_NoAccents and ( amft_Mode = mftNone ) Then
     Exit;
  if (Chaine = '')
   Then
    Exit;
  if ab_NoAccents // conversion of accents
  {$IFDEF FPC}
    Then Chaine := AnsiToUtf8(fs_ReplaceWithTable (StringReplace( StringReplace(Chaine,#195,'',[rfReplaceAll,rfIgnoreCase]),#194,'',[rfReplaceAll,rfIgnoreCase]),SansAccents));
  {$ELSE}
    Then Chaine := AnsiToUtf8(fs_ReplaceAccents(StringReplace( StringReplace(Chaine,#195,'',[rfReplaceAll,rfIgnoreCase]),#194,'',[rfReplaceAll,rfIgnoreCase])));
  {$ENDIF}
  case amft_Mode of
  {$IFDEF FPC}
   mftUpper : Chaine := UTF8UpperCase (Chaine,gs_Lang);
   mftLower : Chaine := UTF8LowerCase (Chaine,gs_Lang);
  {$ELSE}
   mftUpper : Chaine := UpperCase (Chaine);
   mftLower : Chaine := LowerCase (Chaine);
  {$ENDIF}
   mftFirstIsMaj:
    Begin
      if Length(Chaine) > 1
      Then p_PutFirstCharOfWordsInMaj(Chaine,'')
      else Chaine := {$IFDEF FPC}UTF8UpperCase (Chaine,gs_Lang){$ELSE}UpperCase (Chaine){$ENDIF};
    end;
   mftFirstCharOfWordsIsMaj:
      p_PutFirstCharOfWordsInMaj(Chaine);
  end;
end;
function fs_FormatText(const Chaine:String ; const amft_Mode :TModeFormatText; const ab_NoAccents:Boolean = False ):String;
Begin
  Result:=Chaine;
  p_FormatText(Result,amft_Mode,ab_NoAccents);
end;

function fs_EraseSpecialChars( const aText: string): string;
var li_i : Longint ;
Begin
  Result := '';
  for li_i := 1 to length ( aText ) do
    if  fb_isFileChar ( aText [ li_i ])
     Then Result := Result + aText [ li_i ]
     Else Result := Result + '_';

End;


// function TextToFileName
// creating file name
function fs_TextToFileName(const Chaine:String; const ab_NoAccents :Boolean = True ):AnsiString;
begin
  Result:=chaine;
  if Result = '' Then
    Exit;
  Result := StringReplace( StringReplace(Chaine,#195,'',[rfReplaceAll,rfIgnoreCase]),#194,'',[rfReplaceAll,rfIgnoreCase]);
  Result:=fs_EraseSpecialChars(fs_ReplaceWithTable (Result,SansAccents));
end;

function fs_EraseChar(const AChaine:String; const ACharToErase : Char ):String;
var AChar :PChar;
    EndChar : PChar;
begin
  Result:='';
  if AChaine = '' Then
    Exit;
  AChar:=@AChaine[1];
  EndChar:=@AChaine[Length(AChaine)];
  while AChar<=EndChar do
    begin
      if AChar^ <> ACharToErase Then AppendStr( Result, AChar^ ); // if not a correct char so ''
      inc (Achar);
    end;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction ramenant une liste de string en supprimant le séparateur
///////////////////////////////////////////////////////////////////////////////
function fb_stringConstruitListe( const aTexte: string ; var aa_Result : TUArray ):Boolean;
var AChar :PAnsiChar;
    EndChar : PAnsiChar;
    AInt:^Byte;
    li_texte : Pointer ;
    lw_Char : Word;
    procedure p_add;
    Begin
      SetLength(aa_Result,high(aa_Result)+2);
      aa_Result [ High(aa_Result)] [0] := {$IFNDEF FPC}Integer{$ENDIF} ( li_texte ) - {$IFNDEF FPC}Integer{$ENDIF}(@aTexte[1]) + 1;
      aa_Result [ High(aa_Result)] [1] := lw_Char;
      aa_Result [ High(aa_Result)] [2] := 0;
    end;

begin
  Result := False;
  if aTexte = '' Then
    Exit;
  AChar:=@aTexte[1];
  li_texte:=@aTexte[1];
  lw_Char := 0;
  EndChar:=@aTexte[Length(aTexte)];
  repeat
      if not ( AChar^ in ['-',',','''','"',';','/',' ','(',')'] )
       Then lw_Char := AChar - li_texte+1
       Else
       Begin
         AInt := Pointer ( AChar );
         if AInt^ in [194,195] Then //  on utf8 the char 195 is accent
           Begin
             inc (Achar);
             lw_Char := AChar - li_texte+1;
           End
          else
           Begin
            if ( li_texte < AChar ) and ( lw_Char > 0 ) Then
              Begin
                p_add;
                aa_Result [ High(aa_Result)] [2] := ord(AChar^);
                Result := True;
                lw_Char := 0;
                li_texte := AChar+1;
              end;
           end;
       End ;
      inc (Achar);
  until ( AChar>EndChar );
  if ( li_texte < AChar ) and ( lw_Char > 0 ) Then
    p_add;
end;
function fs_ReplaceEndOfLines ( const ALines : TStrings; const as_replace : String = '\n' ): String;
var li_i : Integer;
Begin
  Result := '';
  for li_i := 0 to ALines.Count-1 do
    AppendStr(Result,ALines [ li_i ] + as_replace );
end;

// function fs_GetLabelCaption
// Getting label caption from name
// Name of caption
function fs_GetLabelCaption ( const as_Name : String ):WideString;
Begin
   Result := fs_GetStringValue ( gstl_Labels, as_Name );
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonction_string );
{$ENDIF}
end.

