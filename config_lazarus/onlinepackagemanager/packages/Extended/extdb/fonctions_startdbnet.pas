unit fonctions_startdbnet;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface

uses
  Classes,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  DB;

{$IFDEF VERSIONS}
const
      gver_fonctions_dbnet : T_Version = ( Component : 'DB NET Connect package.' ;
                                         FileUnit : 'fonctions_dbnet' ;
                        		 Owner : 'Matthieu Giroux' ;
                        		 Comment : 'Just add the package.' ;
                        		 BugsStory   : 'Version 1.0.0.0 : DB Net Version.'  ;
                        		 UnitType : 1 ;
                        		 Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

implementation

uses
    OnLineQuery,
    NetConnection,
    fonctions_dbcomponents;



procedure p_ExecuteOnLineQuery ( const adat_Dataset : TComponent  );
Begin
  if ( adat_Dataset is TClientDataset ) Then
     with ( adat_Dataset as TClientDataset ) do
       OnlineConn.Buffer.ExecSQL(adat_Dataset, SQL.Text);
End ;

initialization
 ge_OnExecuteQuery:=TOnExecuteQuery(p_ExecuteOnLineQuery);
 {$IFDEF VERSIONS}
 p_ConcatVersion ( gVer_fonctions_dbnet );
 {$ENDIF}
end.

