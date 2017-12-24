unit fonctions_version;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses Classes, SysUtils ;

type T_Version = Record
                    Component ,
                    FileUnit      ,
                    Owner    		,
                    Comment     : String ;
                    BugsStory   : WideString ;
                    UnitType,
                    Major ,
                    Minor ,
                    Release ,
                    Build   : Integer ;
                  End ;

var gt_Versioning   : array of T_Version ;

const
    gVer_fonctions_version : T_Version = ( Component : 'Gestion des versions' ;
               			                 FileUnit : 'fonctions_version' ;
               			                 Owner : 'Matthieu Giroux' ;
               			                 Comment : 'Utilisé pour gérer la version d''une composante' ;
               			                 BugsStory : 'Gestion de version...';
               			                 UnitType : 1 ;
               			                 Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
      CST_TYPE_UNITE_APPLI    = 5 ;
      CST_TYPE_UNITE_FONCTIONS    = 1 ;
      CST_TYPE_UNITE_FICHE    = 2 ;
      CST_TYPE_UNITE_COMPOSANT    = 3 ;


function  fs_VersionToText  ( const aver_Version : T_Version ) : String ;
procedure p_AboutComponent  ( const aver_Version : T_Version );
Procedure p_ConcatVersion   ( const aver_Version : T_Version );
function  fb_AfficheApropos ( const ab_Commentaires     : Boolean ; const as_NomAppli, as_Version : WideString   ): Boolean ;

implementation

uses Messages, Dialogs, Controls
{$IFDEF VERSIONS}
   , U_About
{$ENDIF}
   , Forms, Graphics ;

function fs_VersionToText ( const aver_Version : T_Version ) : String ;

Begin
  Result := IntToStr ( aver_Version.Major ) + '.' + IntToStr ( aver_Version.Minor ) + '.' + IntToStr ( aver_Version.Release ) + '.' + IntToStr ( aver_Version.Build );
End ;

Procedure p_ConcatVersion ( const aver_Version : T_Version );

Begin
  setLength ( gt_Versioning, high ( gt_Versioning ) + 2 );
  with gt_Versioning [ high ( gt_Versioning )] do
    Begin
      Component   := aver_Version.Component ;
      FileUnit    := aver_Version.FileUnit ;
      Owner       := aver_Version.Owner ;
      Comment     := aver_Version.Comment ;
      BugsStory   := aver_Version.BugsStory ;
      UnitType    := aver_Version.UnitType ;
      Major       := aver_Version.Major ;
      Minor       := aver_Version.Minor ;
      Release     := aver_Version.Release ;
      Build       := aver_Version.Build ;
    end;
End ;

procedure p_AboutComponent ( const aver_Version : T_Version );

Begin
  ShowMessage ( 'GIROUX 2006' + #13#10 + #13#10 + #13#10
                + 'Composante : ' + aver_Version.Component + #13#10 + #13#10
                + 'Version    : ' + fs_VersionToText ( aver_Version ) + #13#10 + #13#10
                + 'Unité      : ' + aver_Version.FileUnit );
End ;
function fb_AfficheApropos ( const ab_Commentaires     : Boolean   ; const as_NomAppli, as_Version : WideString ): Boolean ;
Begin
  Result := False ;
{$IFDEF VIRTUALTREES}
  if not assigned ( gic_F_AboutIcon )
   Then
    gic_F_AboutIcon := TIcon.Create ;

  Application.CreateForm(TF_About, F_About);
  F_About.gs_NomApli := as_NomAppli ;
  F_About.gs_Version := as_Version ;
  F_About.gb_ShowComments := ab_Commentaires ;
  F_About.lb_Giroux.BringtoFront ;
  if F_About.ShowModal<> mrOk then Result := True;
  F_About.Destroy;
{$ENDIF}
End ;

initialization
  p_ConcatVersion ( gVer_fonctions_version );
finalization
  Finalize ( gt_Versioning );
end.
