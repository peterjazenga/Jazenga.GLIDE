unit fonctions_vtree;
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
       VirtualTrees;

{$IFDEF VERSIONS}
const
 gVer_fonctions_vtree : T_Version = ( Component : 'Virtual Tree management' ; FileUnit : 'fonctions_vtree' ;
                             			           Owner : 'Matthieu Giroux' ;
                             			           Comment : 'Virtual Tree'' Functions.' ;
                             			           BugsStory : 'Version 1.0.0.1 : Testing getnodelevel.'+#10
                                                                     + 'Version 1.0.0.0 : Working.';
                             			           UnitType : 1 ;
                             			           Major : 1 ; Minor : 0 ; Release : 0 ; Build : 1 );
{$ENDIF}

function GetNodeLevel ( Node: PVirtualNode ; const RootNode: PVirtualNode):Integer;


implementation

{ Standard Functions }

function GetNodeLevel ( Node: PVirtualNode; const RootNode: PVirtualNode):Integer;
Begin
  Result := 1 ;
  while Node <> RootNode do
   Begin
     Node := Node^.Parent;
     inc ( Result );
   end;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_vtree );
{$ENDIF}
end.
