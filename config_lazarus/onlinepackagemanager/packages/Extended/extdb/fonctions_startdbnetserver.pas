unit fonctions_startdbnetserver;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface

uses
  Classes, SysUtils,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  DB;

{$IFDEF VERSIONS}
const
      gver_fonctions_dbnetserver : T_Version = ( Component : 'DB NET Server package.' ;
                                         FileUnit : 'fonctions_dbnetserver';
                        		 Owner : 'Matthieu Giroux' ;
                        		 Comment : 'Just add the package.' ;
                        		 BugsStory   : 'Version 1.0.0.0 : ZEOS Version.'  ;
                        		 UnitType : 1 ;
                        		 Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

implementation

uses
    StdCtrls,
    ZeosDataServer,
    ZConnection,
    fonctions_dbcomponents;



function fb_OpenDatabase  ( const AConnection  : TComponent ;
                            const ab_Open : Boolean ;
                            const ab_showError : Boolean    ):Boolean;
begin
  with AConnection as TZeosDataServer do
    Begin
     try
      Active := ab_open;
     Except
     end;
     Result:=Active;
    end;
End;



initialization
 ge_OnOpenOrCloseDatabase  := TOnOpenCloseDatabase({$IFNDEF FPC}@{$ENDIF}fb_OpenDatabase);
 {$IFDEF VERSIONS}
 p_ConcatVersion ( gVer_fonctions_dbnetserver );
 {$ENDIF}
end.

