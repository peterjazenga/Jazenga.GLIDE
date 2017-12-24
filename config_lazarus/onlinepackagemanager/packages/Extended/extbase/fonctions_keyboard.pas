unit fonctions_keyboard;
// Unité de la Version 2 du projet FormMain
// La version 1 TFormMain n'est pas sa fenêtre parente

// Le module crée des propriété servant à la gestion du fichier INI
// Il gère la déconnexion
// Il gère la gestion des touches majuscules et numlock
// Il gère les forms enfants
// créé par Matthieu Giroux en décembre 2007

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,,
{$ENDIF}
  SysUtils;

{$IFDEF VERSIONS}
  const
    gVer_fonctions_keyboard: T_Version = (  Component : 'Gestion de Fenêtres' ;
                                       FileUnit : 'fonctions_forms' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Fiche principale deuxième version.' ;
                                       BugsStory : '1.0.0.0 : keyboard functions.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 0 ; Release : 1 ; Build : 1 );

{$ENDIF}

// Récupère le code déjà tapé d'une toouche à partir du buffer virtuelle et valide ou non la touche
// Entrée : Numéro de touche
function fb_GetKeyState(aby_Key: Integer): Boolean;
// Modifie la touche
// Entrée : Numéro de touche
procedure p_SetKeyState( var at_Buffer : TKeyboardState; const aby_Key: Integer; const ab_TurnOn: Boolean);


implementation


{ fonctions }

// Modifie la touche
// Entrée : Numéro de touche
procedure p_SetKeyState( var at_Buffer : TKeyboardState; const aby_Key: Integer; const ab_TurnOn: Boolean);
begin
  // Si windows non nt
  {$IFDEF DELPHI}
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then // Win95/98/ME
    begin
      at_Buffer[aby_Key] := Ord(ab_TurnOn);
      SetKeyboardState(at_Buffer);
    end
  // Si windows nt
  else if (fb_GetKeyState(aby_Key) <> ab_TurnOn) then // Procédure spécialisée
    begin
      keybd_event(aby_Key, $45, KEYEVENTF_EXTENDEDKEY, 0); // simulate aby_Key press
      keybd_event(aby_Key, $45, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0); // simulate aby_Key release
    end;
  {$ENDIF}
end;

// Récupère le code déjà tapé d'une toouche à partir du buffer virtuelle et valide ou non la touche
// Entrée : Numéro de touche
function fb_GetKeyState(aby_Key: Integer): Boolean;
{$IFDEF DELPHI}
var lt_TempBuffer: TKeyboardState;
{$ENDIF}
begin
  // Si Windows non NT
  {$IFDEF DELPHI}
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then // Win95/98/ME
    begin
      GetKeyboardState ( lt_TempBuffer );
      // Le buffer stocke 2 valeurs dans un mot(entier), on récupère la bonne valeur
      Result := lt_TempBuffer [ aby_Key ] and 1 <> 0;
    end
  // Si Windows NT
  else
    // Le buffer ne fonctionne pas bien avec NT la propriété GetGeyState stocke
    // 2 valeurs dans un mot(entier), on récupère la bonne valeur
    Result := GetKeyState(aby_Key) and 1 <> 0;
  {$ELSE}
   Result := False;
  {$ENDIF}
end;


initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_keyboard );
{$ENDIF}
end.
