unit fonctions_numedit;

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}


uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
     SysUtils,Controls,
     Math,
     StdCtrls,
     Classes,
     Forms,
     DB;

const CST_EXTOPERATION = 'TExtOperation';
      CST_EXTCOPERATION = 'TExtCOperation';
      CST_EXTMOPERATION = 'TExtMOperation';
type
    TNumRounded = (nrNone,nrErase,nrMiddle);

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
function DBUseRightToLeftAlignment(AControl: TControl; AField: TField): Boolean;
procedure p_editGridKeyPress ( const aobj_Sender : Tobject ; var ach_Key : Char ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean ; const ai_SelStart, ai_cursorpos : Integer ; const as_Texte , as_SelTexte: String ; const ab_Virgule : Boolean );
procedure p_editKeyUp ( var aext_Value : Extended; var ach_Key : Word ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean ; var as_Texte     : String  );
function fext_CalculateNumber ( const AValue : Extended ; const FNumRounded : TNumRounded ; const aby_NbApVirgule : Byte ): Extended;
function fs_InitZeroText ( const aby_NbAfComma : Byte ):String;

type

  {$ifdef FPC}
  {$ifdef FPC_HAS_TYPE_EXTENDED}
    TAFloat = Extended;
  {$else}
    {$ifdef FPC_HAS_TYPE_DOUBLE}
       TAFloat = Double;
    {$else}
    {$ifdef FPC_HAS_TYPE_SINGLE}
      TAFloat = Single;
    {$else}
      TAFloat = Float;
    {$endif FPC_HAS_TYPE_SINGLE}
    {$endif FPC_HAS_TYPE_DOUBLE}
   {$endif FPC_HAS_TYPE_EXTENDED}
   {$else}
   TAFloat = Extended;
   {$endif FPC}
const
  {$ifdef FPC}
  {$ifdef FPC_HAS_TYPE_EXTENDED}
    TMaxFloat = MaxExtended;
  {$else}
    {$ifdef FPC_HAS_TYPE_DOUBLE}
       TMaxFloat = MaxDouble;
    {$else}
    {$ifdef FPC_HAS_TYPE_SINGLE}
      TMaxFloat = MaxSingle;
    {$else}
      TMaxFloat = MaxFloat;
    {$endif FPC_HAS_TYPE_SINGLE}
    {$endif FPC_HAS_TYPE_DOUBLE}
  {$endif FPC_HAS_TYPE_EXTENDED}
  {$else}
    TMaxFloat = MaxExtended;
  {$endif FPC}
{$IFDEF VERSIONS}
  gVer_fonctions_numedit : T_Version = ( Component : 'Gestion partagée des nombres' ; FileUnit : 'fonctions_numedit' ;
                        		 Owner : 'Matthieu Giroux' ;
                        		 Comment : 'Gestion partagée du formatage des nombres' ;
                        		 BugsStory : 'Version 1.1.1.0 : DBUseRightToLeftAlignment.'
                        			   + 'Version 1.1.0.1 : TNumRounded type not tested.'
                        			   + 'Version 1.1.0.0 : Passage en Jedi 3.'
                        			   + 'Version 1.0.0.0 : Fonctions partagées.';
                        		 UnitType : 1 ;
                        		 Major : 1 ; Minor : 1 ; Release : 1 ; Build : 0 );

{$ENDIF}
implementation

uses fonctions_string,DBCtrls;

function fs_InitZeroText ( const aby_NbAfComma : Byte ):String;
Begin
  Result := '0';
  if aby_NbAfComma > 0 Then
    Result := Result + {$IFDEF FORMATSETTING}FormatSettings.{$ENDIF}DecimalSeparator + fs_RepeteChar('0',aby_NbAfComma);
end;

// A ne pas utiliser
// Evènement sur touche enlevée d'un dbedit et d'une grille
// Paramètres : pour créer l'évènement
procedure p_editKeyUp ( var aext_Value : Extended; var ach_Key : Word ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean ; var as_Texte     : String  );
var lli_Position : Longint ;
    lb_Reformate : Boolean ;
Begin
  // Zone d'éditon :
  // Rien alors on met un 0
  lb_Reformate := False ;
  {$IFDEF FORMATSETTING}
  with FormatSettings do
   Begin
   {$ENDIF}
  if  ( AnsiPos ( DecimalSeparator, as_Texte ) > 0 )
  and ( AnsiPos ( DecimalSeparator, as_Texte ) < length ( as_Texte ) - aby_NbApVirgule ) then
    Begin
      as_Texte := copy ( as_Texte, 1, AnsiPos ( DecimalSeparator, as_Texte ) + aby_NbApVirgule );
      lb_Reformate := True ;
    End ;
  // si il y a un séparateur de milliers et le texte est reformaté automatiquement
  // La saisie est alors différente
  if  ( AnsiPos ( ThousandSeparator, as_Texte ) > 0 )
  or lb_Reformate  Then
    try
      aext_Value := StrToFloat ( fs_RemplaceEspace ( as_Texte, '' ));
    except
      as_Texte := fs_InitZeroText ( aby_NbApVirgule );
    End ;
  {$IFDEF FORMATSETTING}
   End;
  {$ENDIF}
End ;
// A ne utiliser si on surcharge l'évènement onkeydown d'une grille
// action sur touche enlevée d'une grille
// Dévalide la suppression et l'insertion
// Paramètres : pour créer l'évènement
procedure p_editGridKeyPress ( const aobj_Sender : Tobject ; var ach_Key : Char ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean; const ai_SelStart, ai_cursorpos : Integer ; const as_Texte , as_SelTexte : String ; const ab_Virgule : Boolean );
var lby_Signe   : Byte ;
    li_i, li_lengthText : Integer;
Begin
  if (( AnsiPos ( '+'             , as_Texte ) > 0 ) or ( AnsiPos ( '-'             , as_Texte ) > 0 )) Then
    lby_Signe := 1
  Else
    lby_Signe := 0 ;
  if ( ai_CursorPos = 0 ) and ( AnsiPos ( '-', as_Texte ) > 0 ) then
    Begin
      ach_key := #0;
      Exit;
    End ;
  // gestion du + et du -
  if  ( ach_Key in [ '-','+']) Then
    Begin
      if (( ai_SelStart > 0 ) or ( AnsiPos ( '-', as_Texte ) > 0 ) or ( AnsiPos ( '+', as_Texte ) > 0 )) and not (( AnsiPos ( '-', as_SelTexte ) > 0 ) or ( AnsiPos ( '+', as_SelTexte ) > 0 )) then
        Begin
          ach_key := #0;
          Exit;
        End ;
      if ( ach_Key = '-' )
      and not ab_Negatif Then
        Begin
          ach_key := #0;
          Exit;
        End ;
    End ;
  // gestion de la bonne virgule à afficher
  {$IFDEF FORMATSETTING}
  with FormatSettings do
   Begin
   {$ENDIF}
  if ( ach_Key in ['.',',']) Then
    ach_Key := DecimalSeparator ;
  if ( ach_Key = DecimalSeparator )
  and ( pos ( DecimalSeparator, as_texte ) > 0)
  and ( pos ( DecimalSeparator, as_SelTexte ) = 0)
   then
     Begin
       ach_key := #0;
       Exit;
     end;
   li_lengthText:=length(as_texte);
  // Touches valides : il n'y en a pas d'autres
  if not ( ach_Key in [#8, #13, '0'..'9','-','+',DecimalSeparator]) then
    Begin
      ach_key := #0;
      Exit;
    End
  Else
    // On veut taper un nombre
    if not ( ach_Key in [#8, #13,DecimalSeparator,'-','+'] )
    and ( length ( as_SelTexte ) <= 0 ) Then
      // Gestion du nombre de chiffres après la virgule
      if  ( AnsiPos ( DecimalSeparator, as_Texte ) > 0 )
      and ( li_lengthText >= AnsiPos ( DecimalSeparator, as_Texte ) + aby_NbApVirgule)
      and ( ai_SelStart = li_lengthText)
      and ( ai_SelStart > AnsiPos ( DecimalSeparator, as_Texte ))
      and ( AnsiPos ( DecimalSeparator, as_SelTexte ) = 0 ) Then
        Begin
          ach_key := #0;
          Exit;
        End
      Else
      // Gestion du nombre de chiffres avant la virgule
        if  ((     ( AnsiPos ( DecimalSeparator, as_Texte ) > 0 )
              and ( AnsiPos ( DecimalSeparator, as_Texte ) - lby_Signe > aby_NbAvVirgule )
              and ( ai_SelStart < AnsiPos ( DecimalSeparator, as_Texte )))
        or  (     ( AnsiPos ( DecimalSeparator, as_Texte ) <= 0 )
              and ( li_lengthText - lby_Signe >= aby_NbAvVirgule )))
        and ( AnsiPos ( DecimalSeparator, as_SelTexte ) = 0 ) Then
          Begin
            ach_key := #0;
            Exit;
          End ;

  if ach_key in ['0'..'9'] Then
    Begin
      li_i := AnsiPos ( DecimalSeparator, as_Texte );
      if ( li_i > 0 ) Then
        Begin
         if li_lengthText - li_i > aby_NbApVirgule Then
           Begin
             ach_key := #0;
             Exit;
           end;
        End;
    end;
  // Gestion d'un champ sans virgule
  if  ( ach_Key = DecimalSeparator)
  and ((( AnsiPos(DecimalSeparator, as_Texte ) > 0 ) and ( AnsiPos ( DecimalSeparator, as_SelTexte ) = 0 )) or ( not ab_Virgule )) then
    ach_Key := #0 ;
  {$IFDEF FORMATSETTING}
   End;
  {$ENDIF}
End ;

function fext_CalculateNumber ( const AValue : Extended ; const FNumRounded : TNumRounded ; const aby_NbApVirgule : Byte ): Extended;
Begin
  if ( AValue >= TMaxFloat / Power ( 10, aby_NbApVirgule )) Then
    Begin
     Result := AValue ;
     Exit;
    end;
  if ( aby_NbApVirgule > 0 )
   Then
    case FNumRounded of
      nrMiddle : Result := Round ( AValue * Power ( 10, aby_NbApVirgule )) / Power ( 10, aby_NbApVirgule );
      nrErase  : Result := Trunc ( AValue * Power ( 10, aby_NbApVirgule )) / Power ( 10, aby_NbApVirgule );
      Else
        Result := AValue ;
    End
   else
   case FNumRounded of
     nrMiddle : Result := Round ( AValue );
     nrErase  : Result := Trunc ( AValue );
     Else
       Result := AValue ;
   End;
End;

{ AField is needed because TDBLookupComboBox, for its combobox, uses FListField
  for its alignment characteristics not FField }
function DBUseRightToLeftAlignment(AControl: TControl; AField: TField): Boolean;
var
  AAlignment: TAlignment;
begin
  if Assigned(AField) then
    AAlignment := AField.Alignment
  else
    AAlignment := taLeftJustify;
  Result := (SysLocale.MiddleEast) and (AControl.BiDiMode = bdRightToLeft)
{$IFDEF DELPHI}  and (OkToChangeFieldAlignment(AField, AAlignment)){$ENDIF};
end;

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
begin
  case Alignment of
    taLeftJustify:  Alignment := taRightJustify;
    taRightJustify: Alignment := taLeftJustify;
  end;
end;


initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_numedit );
{$ENDIF}
finalization
end.
