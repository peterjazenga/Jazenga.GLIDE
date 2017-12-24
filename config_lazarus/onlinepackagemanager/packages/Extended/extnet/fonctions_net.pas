unit fonctions_net;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF VERSIONS}
   fonctions_version,
  {$ENDIF}
  SysUtils,
  IniFiles,
  fonctions_system;

type  TAVersionInfo =  array [ 0..3 ] of word;

var   gr_ExeVersion   : TAVersionInfo;

const INI_FILE_UPDATE = 'UPDATE';
      INI_FILE_UPDATE_FILE_SIZE = 'FileSize';
      INI_FILE_UPDATE_FILE_SIZE_UNCOMPRESSED = 'SizeUncompressed';
      INI_FILE_UPDATE_VERSION      = 'Version';
      INI_FILE_UPDATE_EXE_VERSION  = 'VersionExe';
      INI_FILE_UPDATE_BASE_VERSION = 'VersionBase';
      INI_FILE_UPDATE_DATE         = 'Date';
      INI_FILE_UPDATE_MD5          = 'md5';
      INI_FILE_UPDATE_FILE_NAME    = 'FileName';
      INI_NEAR_EXE_VERSION         = 'Version' ;

{$IFDEF VERSIONS}
    gVer_fonctions_net : T_Version = ( Component : 'Functions TNetUpdate' ;
                                               FileUnit : 'fonctions_net' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Net File Download.' ;
                                               BugsStory : '0.9.9.0 : Adding desktops.'+#13#10
                                                         + '0.9.0.0 : Updating ok.';
                                               UnitType : 1 ;
                                               Major : 0 ; Minor : 9 ; Release : 9 ; Build : 0 );


{$ENDIF}

type TLCLType = ( lDefault, lKDE, lAndroid, lCarbon, lCocoa );

function ftl_GetLCLCType: TLCLType;
function fs_GetLCLCType (  const aLCLType : TLCLType ):String;
function fs_GetIniFileNameUpdate ( const aat_ArchitectureType : TArchitectureType; const apt_PackageType : TPackageType ; const aProcType : TProcessorType ; const aLCLType : TLCLType ; const as_BeginIni : String ):String;
function fs_GetFileNameUpdate ( const aat_ArchitectureType : TArchitectureType; const apt_PackageType : TPackageType ; const aProcType : TProcessorType ; const aLCLType : TLCLType ; const as_BeginFile : String ):String;
function IniVersionExe ( const AIniFile : TIniFile ):TAVersionInfo;
function fi_BaseVersionToInt ( const as_versionBase : String ): Integer;
function fi64_VersionExeInt64( as_versionExe : String ):Int64; overload;
function fi64_VersionExeInt64:Int64; overload;
function fs_VersionExe:String;
function fs_GetFullArchitecture ( const aat_ArchitectureType : TArchitectureType; const aProcType : TProcessorType ):String;


implementation

uses fonctions_string,
{$IFDEF FPC}
     LazFileUtils,
{$ENDIF}
     fonctions_ini;

function fs_GetFullArchitecture ( const aat_ArchitectureType : TArchitectureType; const aProcType : TProcessorType ):String;
begin
  Result := CST_ProcessorTypeString [ aProcType ];
  case aat_ArchitectureType of
    at64 :  AppendStr ( Result, '64' );
  End;
End;
function ftl_GetLCLCType: TLCLType;
begin
 Result := lDefault;
 {$IFDEF LCLCARBON}
 Result := lCarbon ;
 {$ELSE}
 {$IFDEF LCLCOCOA}
 Result := lCocoa ;
 {$ELSE}
 {$IFDEF LCLQT}
 Result := lKDE ;
 {$ELSE}
 {$IFDEF LCLANDROID}
 Result := lAndroid ;
 {$ENDIF}
 {$ENDIF}
 {$ENDIF}
 {$ENDIF}
End;
function fs_GetLCLCType (  const aLCLType : TLCLType ):String;
begin
  case aLCLType of
    lKDE : Result := '-K' ;
    lAndroid : Result := '-A' ;
    lCarbon : Result := '-X' ;
    lCocoa : Result := '-I' ;
    else
      Result := '';
  end;
End;
function fs_GetIniFileNameUpdate ( const aat_ArchitectureType : TArchitectureType; const apt_PackageType : TPackageType ; const aProcType : TProcessorType ; const aLCLType : TLCLType; const as_BeginIni : String ):String;
begin
  Result := as_BeginIni+fs_GetLCLCType ( aLCLType )+ fs_FormatText( CST_PackageTypeString [ apt_PackageType ], mftFirstIsMaj ) + fs_GetFullArchitecture ( aat_ArchitectureType, aProcType ) +  CST_EXTENSION_INI ;
End;

function fs_GetFileNameUpdate ( const aat_ArchitectureType : TArchitectureType; const apt_PackageType : TPackageType ; const aProcType : TProcessorType ; const aLCLType : TLCLType ; const as_BeginFile : String ):String;
begin
  Result := as_BeginFile+fs_GetLCLCType ( aLCLType )+ fs_GetFullArchitecture ( aat_ArchitectureType, aProcType ) + '.' + CST_PackageTypeString [ apt_PackageType ];
End;

function IniVersionExe ( const AIniFile : TIniFile ):TAVersionInfo;
Begin
 with AIniFile do
   Begin
     Result [ 0 ]:=ReadInteger(INI_NEAR_EXE_VERSION,'Major',0);
     Result [ 1 ]:=ReadInteger(INI_NEAR_EXE_VERSION,'Minor',0);
     Result [ 2 ]:=ReadInteger(INI_NEAR_EXE_VERSION,'Revision',0);
     Result [ 3 ]:=ReadInteger(INI_NEAR_EXE_VERSION,'Build',0);
     gr_ExeVersion:=Result;
   End;
end;

function fi_BaseVersionToInt ( const as_versionBase : String ): Integer;
Begin
 Result := StrToInt(copy(as_versionBase,1,pos('.',as_versionBase)-1))shl 16
            +StrToInt(copy(as_versionBase,pos('.',as_versionBase)+1,5));
end;
function fi64_VersionExeInt64:Int64;
begin
  // Matthieu Pas de version, c'est sur DELPHI
//    GetFileBuildInfo(NomFic,V1,V2,V3,V4);
  //result:=(int64(gr_ExeVersion [ 0 ]) shl 48);
  result:=int64(gr_ExeVersion [ 3 ])or(int64(gr_ExeVersion [ 2 ]) shl 16)or(int64(gr_ExeVersion [ 1 ]) shl 32)or(int64(gr_ExeVersion [ 0 ]) shl 48);
end;

function fs_VersionExe:String;
begin
  // Matthieu Pas de version, c'est sur DELPHI
//    GetFileBuildInfo(NomFic,V1,V2,V3,V4);
  result:=IntToStr(gr_ExeVersion [ 0 ])+'.'+IntToStr(gr_ExeVersion [ 1 ])+'.'+IntToStr(gr_ExeVersion [ 2 ])+'.'+IntToStr(gr_ExeVersion [ 3 ]);
end;

function fi64_VersionExeInt64( as_versionExe : String ):Int64;
begin
  Result := int64(StrToInt(copy(as_versionExe,1,pos('.',as_versionExe)-1)))shl 48;
  as_versionExe := copy ( as_versionExe, pos('.',as_versionExe)+1, Length(as_versionExe));
  Result := Result or (int64(StrToInt(copy(as_versionExe,1,pos('.',as_versionExe)-1)))shl 32);
  as_versionExe := copy ( as_versionExe, pos('.',as_versionExe)+1, Length(as_versionExe));
  Result := Result or (int64(StrToInt(copy(as_versionExe,1,pos('.',as_versionExe)-1)))shl 16);
  Result := Result or (int64(StrToInt(copy(as_versionExe,pos('.',as_versionExe)+1, Length(as_versionExe)))));
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_net  );
{$ENDIF}
end.

