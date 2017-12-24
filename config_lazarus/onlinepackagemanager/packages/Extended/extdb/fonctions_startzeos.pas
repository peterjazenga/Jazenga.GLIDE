unit fonctions_startzeos;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface

uses
  Classes, SysUtils,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  ZConnection,
  DB;

{$IFDEF VERSIONS}
const
      gver_fonctions_zeos : T_Version = ( Component : 'ZEOS Connect package.' ;
                                         FileUnit : 'fonctions_zeos' ;
                        		 Owner : 'Matthieu Giroux' ;
                        		 Comment : 'Just add the package.' ;
                        		 BugsStory   : 'Version 1.0.0.0 : ZEOS Version.'  ;
                        		 UnitType : 1 ;
                        		 Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

implementation

uses
    ZDataset,
    ZSqlProcessor,
    ZAbstractRODataset,
    StdCtrls,
{$IFDEF FPC}
    LazFileUtils,
{$ENDIF}
    fonctions_system, fonctions_db, fonctions_file,
    fonctions_proprietes,
    ZCompatibility,
    ZDbcIntfs,
    Types,
    fonctions_dbobjects, ZClasses;


procedure p_ExecuteZEOSQuery ( const adat_Dataset : TComponent  );
Begin
  if ( adat_Dataset is TZAbstractRODataset ) Then
    ( adat_Dataset as TZAbstractRODataset ).ExecSQL ;
    (*
  {$ELSE}
  if ( adat_Dataset is TQuery ) Then
    Begin
    ( adat_Dataset as TQuery ).ExecSQL;
  {$ENDIF}
  {$IFDEF EADO}
    End
  else if ( adat_Dataset is TADOQuery ) Then
    Begin
    ( adat_Dataset as TADOQuery ).ExecSQL
  {$ENDIF}
  {$IFDEF ZEOS}
    End
  else
  {$ENDIF}
  {$IFDEF IBX}
    End
  else if ( adat_Dataset is TIBQuery ) Then
    Begin
    ( adat_Dataset as TIBQuery ).ExecSQL
  {$ENDIF}
  {$IFDEF EDBEXPRESS}
    End
  else if ( adat_Dataset is TSQLQuery ) Then
    Begin
    ( adat_Dataset as TSQLQuery ).ExecSQL ;
  {$ENDIF}
    End;              *)
End ;

procedure p_setZEOSConnectionOnCreation ( const cbx_Protocol, ch_ServerConnect, ed_Base, ed_Host, ed_Password, ed_User, ed_Catalog, ed_Collation: TComponent );
  var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  with fobj_getComponentObjectProperty(cbx_Protocol,'Items')as TStrings do
   for I := 0 to Drivers.Count - 1 do
    begin
      Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
      for J := Low(Protocols) to High(Protocols) do
        Append(Protocols[J]);
    End;
End;

procedure p_ExecuteSQLCommandServer ( const AConnection : TComponent; const as_SQL :{$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF}  );
var lsp_sqlcommand : TZSQLProcessor;
Begin
  lsp_sqlcommand:=TZSQLProcessor.Create(nil);
  try
    lsp_sqlcommand.Connection:=AConnection as TZConnection;
    lsp_sqlcommand.Script.Text:=as_SQL;
    lsp_sqlcommand.Execute;
  finally
    lsp_sqlcommand.Destroy;
  end;
End ;

function fb_OpenDatabase  ( const AConnection  : TComponent ;
                            const ab_Open : Boolean ;
                            const ab_showError : Boolean    ):Boolean;
begin
  with AConnection as TZConnection do
   Begin
    try
     connected := ab_open;
    Except
    end;
    Result:=Connected;
   end;
End;


initialization
 ge_OnExecuteScriptServer:=TOnExecuteScriptServer({$IFNDEF FPC}@{$ENDIF}p_ExecuteSQLCommandServer);
 ge_OnExecuteQuery:=TOnExecuteQuery({$IFNDEF FPC}@{$ENDIF}p_ExecuteZEOSQuery);
 ge_OnOpenOrCloseDatabase :=TOnOpenCloseDatabase({$IFNDEF FPC}@{$ENDIF}fb_OpenDatabase);
 ge_SetConnectComponentsOnCreate := TSetConnectComponents({$IFNDEF FPC}@{$ENDIF}p_setZEOSConnectionOnCreation);
 {$IFDEF VERSIONS}
 p_ConcatVersion ( gVer_fonctions_zeos );
 {$ENDIF}
end.

