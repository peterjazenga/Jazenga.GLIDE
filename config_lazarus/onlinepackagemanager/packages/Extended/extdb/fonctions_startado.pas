unit fonctions_startado;

interface

uses ADODB,AdoEdit

procedure p_ReadADOCommonIni ( const acco_ConnAcces, acco_Conn: TComponent; const amif_Init : TIniFile );
function fb_WriteADOCommonIni ( const acco_Conn,acco_ConnAcces : TComponent ; var amif_Init : TIniFile ; const as_NomConnexion: string ) : Boolean ;

implementation

procedure p_ReadADOCommonIni ( const acco_ConnAcces, acco_Conn: TComponent; const amif_Init : TIniFile );
Begin
  if  ( acco_Conn is TADOConnection ) Then
    Begin
      gb_ApplicationAsynchrone := amif_Init.ReadBool(INISEC_PAR, GS_MODE_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
      gb_ConnexionAsynchrone := amif_Init.ReadBool(INISEC_PAR, GS_MODE_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
      gi_IniDatasourceAsynchroneEnregistrementsACharger := amif_Init.ReadInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_NB_ENREGISTREMENTS, CST_ASYNCHRONE_NB_ENREGISTREMENTS);
      gi_IniDatasourceAsynchroneTimeOut                 := amif_Init.ReadInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_TIMEOUT, CST_ASYNCHRONE_TIMEOUT_DEFAUT);
      gb_IniDirectAccessOnServer := amif_Init.ReadBool   (INISEC_PAR, GS_ACCES_DIRECT_SERVEUR, gb_IniDirectAccessOnServer );
      gb_IniADOsetKeySet := amif_Init.ReadBool   (INISEC_PAR, GS_Set_KEYSET, gb_IniADOsetKeySet );
      if gb_ConnexionAsynchrone Then
        Begin
          if ( acco_ConnAcces is TADOConnection ) Then
             ( acco_ConnAcces as TADOConnection ).ConnectOptions := coAsyncConnect ;
          ( acco_Conn      as TADOConnection ).ConnectOptions := coAsyncConnect ;
        End ;
  End ;
End;

function fb_WriteADOCommonIni ( const acco_Conn,acco_ConnAcces : TComponent ; var amif_Init : TIniFile ; const as_NomConnexion: string ) : Boolean ;
var lt_Arg  : Array [0..0] of String ;
Begin
  Result := False;
  if ( acco_Conn is TADOConnection ) Then
    try
      if not amif_Init.SectionExists(INISEC_PAR) Then
        begin
          p_SetComponentProperty ( acco_Conn, 'ConnectionString', '' );
          // Mise à jour des paramètre
          amif_Init.WriteString (INISEC_PAR, INISEC_CON, CST_MACHINE);
          amif_Init.WriteString (INISEC_PAR, GS_AIDE, GS_CHEMIN_AIDE);

          amif_Init.WriteInteger(INISEC_PAR, GS_CONNECTION_TIMEOUT, CST_CONNECTION_TIMEOUT_DEFAUT);
          amif_Init.WriteBool   (INISEC_PAR, GS_ACCES_DIRECT_SERVEUR, gb_IniDirectAccessOnServer );
          amif_Init.WriteBool   (INISEC_PAR, GS_Set_KEYSET, gb_IniADOSetKeyset );
          amif_Init.WriteInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_NB_ENREGISTREMENTS, CST_ASYNCHRONE_NB_ENREGISTREMENTS);
          amif_Init.WriteBool   (INISEC_PAR, GS_MODE_CONNEXION_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
          amif_Init.WriteBool   (INISEC_PAR, GS_MODE_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
          amif_Init.WriteInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_TIMEOUT, CST_ASYNCHRONE_TIMEOUT_DEFAUT);
          // Ouverture de la fenêtre de dialogue de connexion
          EditConnectionString(acco_Conn);
          amif_Init.WriteString (INISEC_PAR, INIPAR_ACCESS, ( acco_Conn as TADOConnection ).ConnectionString);
        end
          else
            Begin
              ( acco_Conn as TADOConnection ).ConnectionString := amif_Init.Readstring(INISEC_PAR, INIPAR_ACCESS, '');
              if assigned ( acco_ConnAcces ) Then
                ( acco_ConnAcces as TADOConnection ).ConnectionTimeout := amif_Init.ReadInteger(INISEC_PAR, GS_CONNECTION_TIMEOUT, CST_CONNECTION_TIMEOUT_DEFAUT);
            End;
        Result := True;
    except
      On E : Exception do
       Begin
         lt_Arg [ 0 ] := fs_PathCommonIni ( as_NomConnexion, False );
         MessageDlg(fs_RemplaceMsg ( GS_INI_FILE_CANT_WRITE, lt_Arg ),mtError,[mbOK],0);
       end;
    End;
end;


end.
