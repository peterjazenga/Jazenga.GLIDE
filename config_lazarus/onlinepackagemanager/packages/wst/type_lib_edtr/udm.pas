unit udm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, IniFiles,
  {$IFDEF WST_IDE}LazConfigStorage, BaseIDEIntf, LCLProc,{$ENDIF WST_IDE}
  pascal_parser_intf, wst_consts;

const
  SECTION_OPTIONS = 'Options';
  CASE_SENSITIVE  = 'CaseSensitive';
  STRING_MAPPING  = 'StringMapping';
{$IFNDEF WST_IDE}
  sLAST_PATH = 'LastPath';
{$ENDIF WST_IDE}
{$IFDEF WST_IDE}
  sCONFIG_FILE_NAME = 'wstsettings.xml';
{$ENDIF WST_IDE}

type

  { TDM }

  TDM = class(TDataModule)
    IM: TImageList;
  private
{$IFNDEF WST_IDE}
    FOptions : TMemIniFile;
    function GetOtions: TCustomIniFile;
{$ENDIF WST_IDE}
{$IFDEF WST_IDE}
    procedure SaveOptionsToIDEStore();
    procedure LoadOptionsFromIDEStore();
{$ENDIF WST_IDE}
  private
    FXsdStringMaping : TXSDStringMaping;
    FCaseSensitive : Boolean;
  private
    procedure LoadOptions(AStore : TCustomIniFile);
    procedure SaveOptions(AStore : TCustomIniFile);
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy();override;
{$IFNDEF WST_IDE}
    property Options : TCustomIniFile read GetOtions;
{$ENDIF WST_IDE}
    property XsdStringMaping : TXSDStringMaping read FXsdStringMaping write FXsdStringMaping default xsmUnicodeString;
    property CaseSensitive : Boolean read FCaseSensitive write FCaseSensitive default CASE_SENSITIVE_DEFAULT;
  end; 

var
  DM: TDM;

implementation

{$R *.lfm}

{ TDM }

{$IFNDEF WST_IDE}
function TDM.GetOtions: TCustomIniFile;
begin
  Result := FOptions;
end;
{$ENDIF WST_IDE}

{$IFDEF WST_IDE}
procedure TDM.SaveOptionsToIDEStore();
var
  store : TConfigStorage;
begin
  try
    store := GetIDEConfigStorage(sCONFIG_FILE_NAME,True);
    try
      store.SetDeleteValue('Options/Version',WST_VERSION_INTEGER,0);
      store.SetDeleteValue('Options/'+CASE_SENSITIVE,Self.CaseSensitive,CASE_SENSITIVE_DEFAULT);
      store.SetDeleteValue('Options/'+STRING_MAPPING,Ord(Self.XsdStringMaping),Ord(xsmUnicodeString));
    finally
      store.Free();
    end;
  except
    on e : Exception do begin
      DebugLn([Format('Saving %s failed: ',[sCONFIG_FILE_NAME]),e.Message]);
    end;
  end;
end;

procedure TDM.LoadOptionsFromIDEStore();
var
  store : TConfigStorage;
  i : Integer;
begin
  try
    store := GetIDEConfigStorage(sCONFIG_FILE_NAME,True);
    try
      Self.CaseSensitive := store.GetValue('Options/'+CASE_SENSITIVE,Self.CaseSensitive);
      i := store.GetValue('Options/'+STRING_MAPPING,Ord(Self.XsdStringMaping));
      if (i >= Ord(Low(TXSDStringMaping))) and (i <= Ord(High(TXSDStringMaping))) then
        Self.XsdStringMaping := TXSDStringMaping(i);
    finally
      store.Free();
    end;
  except
    on e : Exception do begin
      DebugLn([Format('Loading %s failed: ',[sCONFIG_FILE_NAME]),e.Message]);
    end;
  end;
end;
{$ENDIF WST_IDE}

procedure TDM.LoadOptions(AStore: TCustomIniFile);
var
  i : Integer;
begin
  i := AStore.ReadInteger(SECTION_OPTIONS,STRING_MAPPING,Ord(XsdStringMaping));
  if (i >= Ord(Low(TXSDStringMaping))) and (i <= Ord(High(TXSDStringMaping))) then
    XsdStringMaping := TXSDStringMaping(i);
  CaseSensitive := AStore.ReadBool(SECTION_OPTIONS,CASE_SENSITIVE,CaseSensitive);
end;

procedure TDM.SaveOptions(AStore: TCustomIniFile);
begin
  AStore.WriteInteger(SECTION_OPTIONS,STRING_MAPPING,Ord(XsdStringMaping));
  AStore.WriteBool(SECTION_OPTIONS,CASE_SENSITIVE,CaseSensitive);
end;

constructor TDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXsdStringMaping := xsmUnicodeString;
  FCaseSensitive := CASE_SENSITIVE_DEFAULT;
{$IFNDEF WST_IDE}
  FOptions := TMemIniFile.Create(ChangeFileExt(GetAppConfigFile(False),'.ini'));
  LoadOptions(FOptions);
{$ENDIF WST_IDE}
{$IFDEF WST_IDE}
  LoadOptionsFromIDEStore();
{$ENDIF WST_IDE}
end;

destructor TDM.Destroy();
begin
{$IFDEF WST_IDE}
  SaveOptionsToIDEStore();
{$ENDIF WST_IDE}
{$IFNDEF WST_IDE}
  if ( FOptions <> nil ) then begin
    SaveOptions(FOptions);
    if not DirectoryExists(FOptions.FileName) then
      ForceDirectories(ExtractFileDir(FOptions.FileName));
    FOptions.UpdateFile();
  end;
  FOptions.Free();
{$ENDIF WST_IDE}
  inherited Destroy();
end;

end.

