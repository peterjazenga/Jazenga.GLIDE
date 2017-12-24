unit fonctions_variant;

interface

{$I ..\DLCompilers.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}
{
2004-08-27
Création de l'unité par Matthieu Giroux
}
uses Variants,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  Classes,
  fonctions_string,
  fonctions_proprietes;

{$IFDEF VERSIONS}
const
    gVer_fonctions_variant : T_Version = ( Component : 'Gestion des variants' ; FileUnit : 'fonctions_variant' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fonctions de traduction et de formatage des tableaux de variants ( Variables pouvant changer de type ).' ;
                        			                 BugsStory : 'Version 1.0.2.0 : Fonctions fi_CountArrayVariant et fi_CountArrayVarOption (non testée).' + #13#10
                        			                	        	+'Version 1.0.1.0 : Fonctions avec valeurs de retour correctes.' + #13#10
                        			                	        	+ 'Version 1.0.0.0 : Toutes les fonctions sont OK.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 0 ; Release : 2 ; Build : 0 );

{$ENDIF}

type

  tset_OctetOptions = set of Byte;
procedure CopyArrayVariantToArrayString ( const aav_Valeur : Variant; var Result: tt_ValeurStrings);
function fb_SettedOriginalList(const at_Liste: tt_TableauxVarOption ; const ab_TestBool : Boolean ; const ai_OptionLimitDown : Integer ): Boolean;
function fb_VariantsEqual ( const avar_Compare1, avar_Compare2 : Variant ): Boolean ; overload;
function fb_VariantsEqual ( const avar_Compare1 : Variant; const ats_Compare2 : tt_ValeurStrings ): Boolean ; overload;
function fb_StringsEqual ( const ats_Compare1, ats_Compare2 : tt_ValeurStrings ): Boolean ; overload;
function fb_StringsEqual ( const ats_Compare1: tt_ValeurStrings ; const ats_Compare2 : tt_TableauxStrings ; const ali_position : Integer ): Boolean ; overload;
// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListArray(const alst_liste: tt_TableauxStrings;
  const ats_ATrouver: tt_ValeurStrings ; const ab_VarIsNull : Boolean ): Integer;overload;
function fi_findInListVariant(const alst_liste: tt_TableauxStrings;
  const avar_ATrouver: Variant ; const ab_VarIsNull : Boolean ): Integer; overload;

function fi_findInListArray(const alst_liste: tt_ValeurStrings;
  const ats_ATrouver: tt_ValeurStrings ; const ab_VarIsNull : Boolean ): Integer; overload;
function fi_findInListVariant(const alst_liste: tt_ValeurStrings;
  const avar_ATrouver: Variant ; const ab_VarIsNull : Boolean ): Integer;overload;

function fi_CountArrayVariant   (const at_Array: tt_TableauxStrings) : integer ;
function fi_CountArrayVarOption (const at_Array: tt_TableauxVarOption; const ab_TestBool : Boolean ; const ai_Options : tset_OctetOptions ) : integer ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListVarBool(const alst_liste: tt_TableauxVarOption;
  const avar_ATrouver: Variant ; const ab_VarIsNull, ab_TestOption : Boolean; const ai_ValTest : tset_OctetOptions ): Integer;
function fi_findInListArrayBool(const alst_liste: tt_TableauxVarOption;
  const avar_ATrouver: tt_ValeurStrings ; const ab_VarIsNull, ab_TestOption : Boolean; const ai_ValTest : tset_OctetOptions  ): Integer;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure p_AddListe ( var at_Liste : tt_TableauxStrings ; const at_Valeur : tt_ValeurStrings );

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste : tt_TableauxStrings ; const at_Valeur : tt_ValeurStrings ; const ab_VerifExiste : Boolean  = True ): Integer ; overload ;
function fi_AddListe ( var at_Liste : tt_ValeurStrings ; const at_Valeur : tt_ValeurStrings ; const ab_VerifExiste : Boolean  = True ): Integer ; overload ;
function fi_AddListe ( var at_Liste1, at_Liste2 : tt_TableauxStrings ; const ai_Valeur : Integer ; const ab_VerifExiste : Boolean  = True ): Integer ; overload ;
function fi_AddListe ( var at_Liste : tt_TableauxStrings ; const as_Valeur : String ; const ab_VerifExiste : Boolean = True ):Integer; overload ;
function fi_AddListe ( var at_Liste : tt_TableauxVarOption ; const aas_Valeur : tt_ValeurStrings ; const ab_VerifExiste : Boolean = True ):Integer;overload;
function fi_AddListe ( var at_Liste : tt_TableauxVarOption ; const aas_Valeur : Variant ; const ab_VerifExiste : Boolean = True ):Integer;overload;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_EffaceListe ( var at_Liste : tt_TableauxStrings ; const avar_Valeur : Variant ) : Integer  ;
function fb_EffaceListe ( var at_Liste : tt_TableauxStrings ; const avar_Valeur : Variant ) : Boolean ;

// La liste de variants contient-elle des valeurs
// at_Liste : le tableau de variants
// résultat : True si valeurs non null
function fb_SettedList ( const at_Liste : tt_TableauxStrings ) : Boolean ;

// Supprime un item d'un tableau
// at_Liste  : Tableau
// alsi_Item : Item de la liste

implementation

uses SysUtils ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListArray(const alst_liste: tt_TableauxStrings;
  const ats_ATrouver: tt_ValeurStrings ; const ab_VarIsNull : Boolean ): Integer;
var li_i, li_j : Integer ;
    lb_continue : Boolean;
Begin
  Result := -1 ;
  if not ab_VarIsNull Then
   for li_i := 0 to high ( ats_ATrouver ) do
    if ( ats_ATrouver [ li_i ] = '' )
     Then
      Exit ;
    // Scrute la liste
  for li_j := 0 to high ( alst_liste ) do
   Begin
    lb_continue:=False;
    for li_i := 0 to high ( alst_liste [ li_j ] ) do
     // c'est un tableau qu'on a affecté ( pas encore mise en place )
     if fb_StringsEqual ( ats_ATrouver, alst_liste, li_i ) Then
        Begin
          lb_continue := True;
          Result := li_i ;
        End ;
     if not lb_continue Then
      Begin
        Result:=-1;
        Continue;
      end;
   end;
End ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListVariant(const alst_liste: tt_TableauxStrings;
  const avar_ATrouver: Variant ; const ab_VarIsNull : Boolean ): Integer;
 var li_i : Integer ;
Begin
  Result := -1 ;
  if VarIsArray(avar_ATrouver) Then
   Begin
     Result:=fi_findInListArray( alst_liste, tt_ValeurStrings (avar_ATrouver), ab_VarIsNull);
     Exit;
   end;
  if not ab_VarIsNull
  and ( avar_ATrouver = Null )
   Then
    Exit ;
    // Scrute la liste
  if high(alst_liste )>-1 Then
  for li_i := 0 to high ( alst_liste [ 0 ] ) do
  // c'est un tableau qu'on a affecté ( pas encore mise en place )
   if fb_VariantsEqual ( avar_ATrouver, alst_liste [ 0, li_i ]) Then
     Begin
       Result := li_i ;
       Break ;
     End ;
End ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListArray(const alst_liste: tt_ValeurStrings;
  const ats_ATrouver: tt_ValeurStrings ; const ab_VarIsNull : Boolean ): Integer;
var li_i, li_j : Integer ;
    lb_continue : Boolean;
Begin
  Result := -1 ;
  if not ab_VarIsNull Then
   for li_i := 0 to high ( ats_ATrouver ) do
    if ( ats_ATrouver [ li_i ] = '' )
     Then
      Exit ;
    // Scrute la liste
  for li_i := 0 to high ( alst_liste ) do
   // c'est un tableau qu'on a affecté ( pas encore mise en place )
   if fb_StringsEqual ( ats_ATrouver, alst_liste ) Then
      Begin
        Result := li_i ;
        Exit;
      End ;
End ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListVariant(const alst_liste: tt_ValeurStrings;
  const avar_ATrouver: Variant ; const ab_VarIsNull : Boolean ): Integer;
 var li_i : Integer ;
Begin
  Result := -1 ;
  if VarIsArray(avar_ATrouver) Then
   Begin
     Result:=fi_findInListArray(alst_liste, tt_valeurstrings (avar_ATrouver), ab_VarIsNull);
     Exit;
   end;
  if not ab_VarIsNull
  and ( avar_ATrouver = Null )
   Then
    Exit ;
    // Scrute la liste
  if high(alst_liste )>-1 Then
  for li_i := 0 to high ( alst_liste ) do
  // c'est un tableau qu'on a affecté ( pas encore mise en place )
   if fb_VariantsEqual ( avar_ATrouver, alst_liste [  li_i ]) Then
     Begin
       Result := li_i ;
       Exit ;
     End ;
End ;


function CopyArrayVariantToString ( const ats_Variant : Variant ): tt_ValeurStrings;
var li_i : Integer;
Begin
  if VarIsArray(ats_Variant) Then
   Begin
    SetLength(Result,Length(ats_Variant));
    for li_i := 0 to high ( Result ) do
      Result [li_i]    := ats_Variant [li_i];
   end
  Else
  Begin
   SetLength(Result,1);
   Result [0]    := ats_Variant;
  end
end;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListVarBool(const alst_liste: tt_TableauxVarOption;
  const avar_ATrouver: Variant ; const ab_VarIsNull, ab_TestOption : Boolean; const ai_ValTest : tset_OctetOptions ): Integer;
 var li_i : Integer ;
Begin
  Result := -1 ;
    // Scrute la liste
  for li_i := 0 to high ( alst_liste ) do
    if ( ab_TestOption and ( alst_liste [ li_i ].i_Option in ai_ValTest ))
    or not ab_TestOption Then
    // c'est un tableau qu'on a affecté ( pas encore mise en place )
     if (( avar_ATrouver = Null ) and not ab_VarIsNull ) or fb_VariantsEqual ( avar_ATrouver, alst_liste [ li_i ].as_Key ) Then
       Begin

         Result := li_i ;
         Break ;
       End ;
End ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListArrayBool(const alst_liste: tt_TableauxVarOption;
  const avar_ATrouver: tt_ValeurStrings ; const ab_VarIsNull, ab_TestOption : Boolean; const ai_ValTest : tset_OctetOptions ): Integer;
 var li_i, li_j : Integer ;
     lb_continue : Boolean;
Begin
  Result := -1 ;
    // Scrute la liste
  for li_i := 0 to high ( alst_liste ) do
    Begin
     lb_continue := False;
     for li_j := 0 to high ( alst_liste ) do
      if ( ab_TestOption and ( alst_liste [ li_j ].i_Option in ai_ValTest ))
      or not ab_TestOption Then
      // c'est un tableau qu'on a affecté ( pas encore mise en place )
       if (( avar_ATrouver [ li_i ] = Null ) and not ab_VarIsNull ) or fb_VariantsEqual ( avar_ATrouver, alst_liste [ li_j ].as_Key [li_i] ) Then
         Begin
           lb_continue := True;
           Result:=li_j;
           Break ;
         End ;
      if not lb_continue then
       Continue;
    end;
  if not lb_continue Then
   Result:=-1;
End ;

function fb_VariantsEqual ( const avar_Compare1, avar_Compare2 : Variant ): Boolean ;
var li_j : Integer ;
    ls_Compare1, ls_Compare2 : String ;
    function fs_VariantToStr ( const avariant : Variant ) : String;
     Begin
      if VarIsNull(avariant)
        Then Result:=''
        Else Result:=avariant;

     end;

Begin
// c'est un tableau qu'on a affecté ( pas encore mise en place )
 Result := False ;
 if  (avar_Compare1 = Null )
  Then Exit;

 if VarIsArray ( avar_Compare1 )
  Then
   Begin
     // scrute le tableau
     for li_j := VarArrayLowBound ( avar_Compare2 , 1 ) to  VarArrayHighBound ( avar_Compare2 , 1 ) do
       if avar_Compare2[ li_j ] <> avar_Compare1 [ li_j ]
        Then
         Break
        Else
         if li_j = VarArrayHighBound ( avar_Compare2 , 1 )
          Then
           Begin
             Result := True ;
             Break ;
           End ;
   End
  Else
   Begin
    ls_Compare1:=fs_VariantToStr ( avar_Compare1 );
    ls_Compare2:=fs_VariantToStr ( avar_Compare2 );
    if ls_Compare2 = ls_Compare1
      Then
       Begin
         // On a trouvé la valeur
         Result := True ;
       End ;
   end;
End ;

function fb_VariantsEqual ( const avar_Compare1 : Variant; const ats_Compare2 : tt_ValeurStrings ): Boolean ;
var li_j : Integer ;
    ls_Compare1, ls_Compare2 : String ;
Begin
 // c'est un tableau qu'on a affecté ( pas encore mise en place )
  Result := False ;
  if VarIsArray ( avar_Compare1 )
   Then
    Begin
      // scrute le tableau
      for li_j := low ( ats_Compare2 ) to  high ( ats_Compare2 ) do
        if ats_Compare2[ li_j ] <> VarToStr(avar_Compare1 [ li_j ])
         Then
          Break
         Else
          if li_j = high ( ats_Compare2 )
           Then
            Begin
              Result := True ;
              Break ;
            End ;
    End
   Else
   // Gestion du null
    If avar_Compare1 = Null
     Then
      Begin
        if VarToStr(avar_Compare1) = ats_Compare2 [ 0 ]
         Then
          Begin
            // Variable null trouvée
            Result := True ;
          End ;
      End
     Else
      // Sinon on compare des valeurs qui ne doivent pas être null
      if ats_Compare2 [ 0 ] > ''
       Then
        Begin
          // Comparaison
          ls_Compare1 := avar_Compare1 ;
          if ls_Compare1 = ats_Compare2 [ 0 ]
           Then
            Begin
              // On a trouvé la valeur
              Result := True ;
            End ;
        End ;
End ;

function fb_StringsEqual ( const ats_Compare1, ats_Compare2 : tt_ValeurStrings ): Boolean ;
var li_j : Integer ;
    ls_Compare1, ls_Compare2 : String ;
Begin
 // c'est un tableau qu'on a affecté ( pas encore mise en place )
  Result := False ;
  // scrute le tableau
  for li_j := low ( ats_Compare2 ) to  high ( ats_Compare2 ) do
    if ats_Compare2[ li_j ] <> VarToStr(ats_Compare1 [ li_j ])
     Then
      Break
     Else
      if li_j = high ( ats_Compare2 )
       Then
        Begin
          Result := True ;
          Break ;
        End ;
End ;

function fb_StringsEqual ( const ats_Compare1: tt_ValeurStrings ; const ats_Compare2 : tt_TableauxStrings ; const ali_position : Integer ): Boolean ; overload;
var li_j : Integer ;
    ls_Compare1, ls_Compare2 : String ;
Begin
 // c'est un tableau qu'on a affecté ( pas encore mise en place )
  Result := False ;
  // scrute le tableau
  for li_j := low ( ats_Compare1 ) to  high ( ats_Compare1 ) do
    if ats_Compare1[ li_j ] <> ats_Compare2 [ li_j, ali_position ]
     Then
      Break
     Else
      if li_j = high ( ats_Compare2 )
       Then
        Begin
          Result := True ;
          Break ;
        End ;
End ;


// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure p_AddListe ( var at_Liste : tt_TableauxStrings ; const at_Valeur : tt_ValeurStrings );
begin
  fi_AddListe ( at_Liste, at_Valeur, True );
End ;
// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste1, at_Liste2 : tt_TableauxStrings ; const ai_Valeur : Integer ; const ab_VerifExiste : Boolean  = True ): Integer ; overload ;
var li_i : Integer;
    lt_Valeur: tt_ValeurStrings;
Begin
  Result := -1 ;
  SetLength(lt_Valeur,high(at_Liste1)+1);
  for li_i := 0 to high(lt_Valeur) do
    lt_Valeur [ li_i ] := at_Liste2 [ li_i, ai_valeur ];
   if  ab_VerifExiste
   and ( fi_findInListVariant ( at_Liste1, lt_Valeur, False ) <> -1 )
    Then
     Exit ;
   Result := fi_AddListe ( at_Liste1, lt_Valeur, ab_VerifExiste );
End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste : tt_TableauxStrings ; const at_Valeur : tt_ValeurStrings ; const ab_VerifExiste : Boolean  = True ):Integer;
var li_i : Integer;
    lt_Valeur: tt_ValeurStrings;
Begin
  Result := -1 ;
   if  ab_VerifExiste
   and ( fi_findInListArray ( at_Liste, at_Valeur, False ) <> -1 )
    Then
     Exit ;

   Result := fi_findInListArray ( at_Liste, lt_Valeur, True );
   If Result = -1
    Then
     for li_i := 0 to high ( at_Valeur ) do
       Begin
        // On ajoute dans les clés d'exclusion
        if high ( at_Liste ) < High(at_Valeur) Then
          SetLength ( at_Liste, high ( at_Valeur )+1);
        SetLength ( at_Liste [ li_i ], high ( at_Liste [ li_i ] ) + 2 );


        at_Liste [ li_i , high ( at_Liste  [ li_i ] ) ] := at_Valeur [ li_i ] ;
        Result := high ( at_Liste [ li_i ] );
       End
      Else
        at_Liste [ li_i , Result ] := at_Valeur [ li_i ] ;

End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste : tt_ValeurStrings ; const at_Valeur : tt_ValeurStrings ; const ab_VerifExiste : Boolean  = True ):Integer;
var li_i : Integer;
    lt_Valeur: tt_ValeurStrings;
Begin
  Result := -1 ;
   if  ab_VerifExiste
   and ( fi_findInListArray ( at_Liste, at_Valeur, False ) <> -1 )
    Then
     Exit ;

   Result := fi_findInListArray ( at_Liste, lt_Valeur, True );
   If Result = -1
    Then
     for li_i := 0 to high ( at_Valeur ) do
       Begin
        // On ajoute dans les clés d'exclusion
        if high ( at_Liste ) < High(at_Valeur) Then
          SetLength ( at_Liste, high ( at_Valeur )+1);
        SetLength ( at_Liste, high ( at_Liste ) + 2 );


        at_Liste [ high ( at_Liste ) ] := at_Valeur [ li_i ] ;
        Result := high ( at_Liste );
       End
      Else
        at_Liste [ Result ] := at_Valeur [ li_i ] ;

End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste : tt_TableauxStrings ; const as_Valeur : String ; const ab_VerifExiste : Boolean = True ):Integer;
var li_i, li_j : Integer;
Begin
   if  ab_VerifExiste
   and ( fi_findInListVariant ( at_Liste, as_Valeur, False ) <> -1 )
    Then
      Begin
        Result := -1 ;
        Exit ;
      End ;
   Result := fi_findInListVariant ( at_Liste, Null, True );
   If Result = -1
      Then
       Begin
         if high ( at_liste ) = -1 Then
          SetLength(at_liste,1);
        // On ajoute dans les clés d'exclusion
        SetLength ( at_Liste [0], high ( at_Liste [0] ) + 2 );


        at_Liste [ 0, high ( at_Liste ) ] := as_Valeur ;
        Result := high ( at_Liste [0] );
       End
      Else
        at_Liste [ 0, Result ] := as_Valeur ;

End ;


// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste : tt_TableauxVarOption ; const aas_Valeur : tt_ValeurStrings ; const ab_VerifExiste : Boolean  = True ):Integer;
var li_i : Integer;
Begin
   if  ab_VerifExiste Then
    Begin
      if ( fi_findInListArrayBool ( tt_TableauxVarOption( at_Liste ), aas_Valeur, False, False, [] ) <> -1 ) Then
       Begin
        Result := -1 ;
        Exit ;
       end;
    end;
   Result := fi_findInListVarBool ( at_Liste, Null, True, False, [] );
   If Result = -1
    Then
     Begin
       // On ajoute dans les clés d'exclusion
       SetLength ( at_Liste , high ( at_Liste ) + 2 );
       SetLength(at_Liste [ high ( at_Liste ) ].as_Key, high(aas_Valeur)+1);
       for li_i := 0 to high(aas_Valeur) do
         at_Liste [ high ( at_Liste ) ].as_Key [li_i] := aas_Valeur [ li_i ] ;
       Result := high ( at_Liste );
     End
    Else
      at_Liste [ Result ].as_Key [li_i]:= aas_Valeur [ li_i ] ;

End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AddListe ( var at_Liste : tt_TableauxVarOption ; const aas_Valeur : Variant ; const ab_VerifExiste : Boolean  = True ):Integer;
Begin
  Result:= fi_AddListe(at_Liste,CopyArrayVariantToString (aas_Valeur),ab_VerifExiste);
End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure CopyArrayVariantToArrayString ( const aav_Valeur : Variant; var Result: tt_ValeurStrings);
var li_i : Integer;
Begin
  if VarIsArray(aav_Valeur) then
   Begin
     SetLength(Result,VarArrayHighBound(aav_Valeur, 1));
     for li_i := 0 to high ( Result ) do
       Result[li_i]:=VarToStr(aav_Valeur[li_i]);
   end
  Else
   Begin
    SetLength(Result,1);
    Result[0]:=VarToStr(aav_Valeur);
   end;
End ;


function fb_EffaceListe ( var at_Liste : tt_TableauxStrings ; const avar_Valeur : Variant ) : Boolean ;
Begin
  Result := fi_EffaceListe ( at_Liste, avar_Valeur ) <> -1 ;
End ;

function fi_EffaceListe ( var at_Liste : tt_TableauxStrings ; const avar_Valeur : Variant ) : Integer ;
Begin
  Result := fi_findInListVariant ( at_Liste, avar_Valeur, False );
  If Result <> -1 Then
    Begin
      at_Liste [ Result ] := Null ;
    End ;
End ;

// La liste de variants contient-elle des valeurs
// at_Liste : le tableau de variants
// résultat : True si valeurs non null
function fb_SettedList ( const at_Liste : tt_TableauxStrings ) : Boolean ;
var li_i : integer ;
Begin
 Result := False ;
  // Il peut y avoir des clés à null
  for li_i := 0 to  high ( at_Liste [0] ) do
   // La liste a-t-elle une clé affectée
   if at_Liste [0] [ li_i ] > '' Then
     Begin
       Result := True ;
       Break ;
     End ;
End ;

// La liste de variants contient-elle des valeurs
// at_Liste : le tableau de variants
// résultat : True si valeurs non null
function fb_SettedOriginalList ( const at_Liste : tt_TableauxVarOption ; const ab_TestBool : Boolean ; const ai_OptionLimitDown : Integer ) : Boolean ;
var li_i, li_j : integer ;
    lb_continue : Boolean;
Begin
 Result := False ;
  // Il peut y avoir des clés à null
 for li_i := 0 to  high ( at_Liste ) do
  Begin
    for li_j := 0 to  high ( at_Liste [ li_i ].as_Key ) do
     Begin
       // La liste a-t-elle une clé affectée ?
       if (not ab_TestBool and ( at_Liste [ li_i ].as_Key [ li_j ] > '' ))
       or ( ab_TestBool and ( at_Liste [ li_i ].i_Option >= ai_OptionLimitDown ) and ( at_Liste [ li_i ].as_Key [ li_j ] > '' ))
        Then Result := True
        Else
         Begin
           Result := False ;
           Break;
         End;
     end;
    if Result Then Break;
  end;
End ;

// Compte les variants non null dans La liste de variants
// at_Array : le tableau de variants
// résultat : Nombre de variants non null
function fi_CountArrayVariant (const at_Array: tt_TableauxStrings) : integer ;
var li_i : integer ;
Begin
  Result := 0 ;
  for li_i := 0 to high ( at_Array ) do
    if at_Array [ li_i ] <> Null Then
      inc ( Result );
End ;

// Compte les variants non null dans La liste de variants
// at_Array : le tableau de variants
// ab_TestBool : Test supplémentaire
// ai_Options  : Options valide à tester
// résultat : Nombre de variants non null
function fi_CountArrayVarOption (const at_Array: tt_TableauxVarOption; const ab_TestBool : Boolean ; const ai_Options : tset_OctetOptions ) : integer ;
var li_i : integer ;
Begin
  Result := 0 ;
  for li_i := 0 to high ( at_Array ) do
    if at_Array [ li_i ].as_Key [ 0 ] > '' Then
      if ab_TestBool Then
        Begin
          if at_Array [ li_i ].i_Option in ai_Options Then
            inc ( Result );
        End
      Else
        inc ( Result );
End ;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_variant );
{$ENDIF}
end.

