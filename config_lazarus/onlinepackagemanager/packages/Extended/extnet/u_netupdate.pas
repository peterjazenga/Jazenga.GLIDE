unit u_netupdate;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}
// Uncomment if cannot compile lazarus. After compile lazarus to finish to set as comment
{.$UNDEF MD5}


interface

uses
  Classes, SysUtils,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
{$IFDEF LNET}
  lNetComponents,
  Lnet, lhttp,
{$ELSE}
  IdComponent,
  IdTCPConnection,
  IdHTTP,
{$ENDIF}
  {$IFDEF FPC}
  lazutf8classes,
  {$ENDIF}
  Controls,
  IniFiles,
  ComCtrls;

const
  INI_FILE_UPDATE = 'UPDATE';
  INI_FILE_UPDATE_SIZE = 'Size';
  INI_FILE_UPDATE_EXE_VERSION = 'VersionExe';
  INI_FILE_UPDATE_BASE_VERSION = 'VersionBase';
  INI_FILE_UPDATE_DATE = 'Date';
  INI_FILE_UPDATE_MD5 = 'md5';
  INI_FILE_UPDATE_FILE_NAME = 'FileName';

{$IFDEF VERSIONS}
  gVer_netupdate: T_Version = (Component: 'Composant TNetUpdate';
    FileUnit: 'U_NetUpdate';
    Owner: 'Matthieu Giroux';
    Comment: 'Net File Download.';
    BugsStory: '1.0.1.1 : Testing ini.'+#10+
               '1.0.1.0 : For Indy.'+#10+
               '1.0.0.0 : Tested ok.'+#10+
               '0.9.0.0 : Updating ok.';
    UnitType: 3;
    Major: 1; Minor: 0;
    Release: 1; Build: 1);


{$ENDIF}
type
  TUpdateStep = (usNone, usIni, usPage, usFile);
  TStateUpdate = (suNeedVerify, suNeedUpdate, suDownloading, suUpdated);
  TErrorMessageEvent = procedure(const Sender: TObject;
    const ErrorCode: integer; const ErrorMessage: string) of object;
  TProgressEvent = procedure(const Sender: TObject; const Uploaded: integer) of object;
  TMD5Event = procedure(const Sender: TObject; const MD5OK: Boolean) of object;
  TProgressInit = procedure(const Sender: TObject; const Total: integer) of object;
  TDownloadingEvent = procedure(const Sender: TObject; const Step: TUpdateStep) of object;
  TDownloadedEvent = procedure(const Sender: TObject;
    const TheFile: string; const TheStep: TUpdateStep) of object;

  { TNetUpdate }

  TNetUpdate = class(TComponent)
  private
    // Parent propriétaire des évènements liés au lien de données
    gs_VersionExeUpdate, gs_VersionBaseUpdate, gs_Date, gs_MD5Ini,
    gs_URL, gs_File, gs_EndFile, gs_FilePage, gs_md5File, gs_Ini,
    gs_UpdateDir: string;
    gsu_UpdateState: TStateUpdate;
    gus_UpdateStep: TUpdateStep;
    gst_Stream: TStream;
    {$IFDEF LNET}
    gs_Buffer: string;
    {$ENDIF}
    gpb_Progress: TProgressBar;
    gc_Component: {$IFDEF LNET} TLComponent{$ELSE}TIdTCPConnection{$ENDIF};
    ge_ErrorEvent: TErrorMessageEvent;
    ge_ProgressEvent: TProgressEvent;
    ge_Downloaded: TDownloadedEvent;
    ge_CreateIni: TNotifyEvent;
    ge_DownloadedPage, ge_DownloadedFile : TMD5Event;
    ge_Downloading : TDownloadingEvent;
    ge_IniRead: TProgressInit;
    gini_inifile: TIniFile;
    gi_Weight: longword;
    gb_Buffered,gb_Messages: boolean;
    procedure p_SetComponent(const AValue: {$IFDEF LNET} TLComponent{$ELSE}TIdTCPConnection{$ENDIF});
    procedure SetUpdateDir ( const AValue : String );
  protected
    {$IFDEF LNET}
    function HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: PChar;
      ASize: integer): integer; virtual;
    procedure HTTPClientDoneInput(ASocket: TLHTTPClientSocket); virtual;
    procedure HTTPClientError(const msg: string; aSocket: TLSocket); virtual;
    {$ELSE}
    procedure IdWork(ASender:TObject;AWorkMode:TWorkMode;
      ASize:{$IFDEF INDY10}Integer{$ELSE}Int64{$ENDIF}); virtual;
    {$ENDIF}
    procedure SetMD5; virtual;
    procedure GetURL(const as_URL, as_LocalDir, as_FileName, as_endFile: string;
      const aus_Step: TUpdateStep = usNone); virtual;
    function CanDownloadIni: boolean; virtual;
    function CanDownloadPage: boolean; virtual;
    function CanDownloadFile: boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnError(const ai_error: integer; const as_Message: string); virtual;
    function GetOptionalFilename: String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateIniPage; virtual;
    procedure Update; virtual;
    procedure VerifyUpdate; virtual;
    procedure VerifyIni(const as_file: string); virtual;
    procedure AfterUpdate(const as_file: string;
      const ab_StatusOK: boolean); virtual;
    property IniFile: TIniFile read gini_inifile;
    property UpdateWeight: longword read gi_Weight default 0;
    property UpdateState: TStateUpdate read gsu_UpdateState default suNeedVerify;
    property UpdateStep: TUpdateStep read gus_UpdateStep default usNone;

    property MD5File : string read gs_md5File;
    property MD5Ini  : string read gs_MD5Ini;
    property VersionExeUpdate: string read gs_VersionExeUpdate;
    property VersionBaseUpdate: string read gs_VersionBaseUpdate;

    property Buffer : {$IFDEF LNET}String read gs_Buffer{$ELSE}TStream read gst_Stream{$ENDIF};

  published
    property FileIni: string read gs_Ini write gs_Ini;
    property FileUpdate: string read gs_File write gs_File;
    property FileAddAtEnd: string read gs_endFile write gs_endFile;
    property FilePage: string read gs_FilePage write gs_FilePage;
    property UpdateDir: string read gs_UpdateDir write SetUpdateDir;
    property URLBase: string read gs_URL write gs_URL;
    property Progress: TProgressBar read gpb_Progress write gpb_Progress;
    property OnErrorMessage: TErrorMessageEvent read ge_ErrorEvent write ge_ErrorEvent;
    property OnIniCreate: TNotifyEvent read ge_CreateIni write ge_CreateIni;
    property OnIniRead: TProgressInit read ge_IniRead write ge_IniRead;
    property OnProgress: TProgressEvent read ge_ProgressEvent write ge_ProgressEvent;
    property OnPageDownloaded: TMD5Event read ge_DownloadedPage write ge_DownloadedPage;
    property OnDownloading: TDownloadingEvent read ge_Downloading write ge_Downloading;
    property OnDownloaded: TDownloadedEvent read ge_Downloaded write ge_Downloaded;
    property OnFileDownloaded: TMD5Event read ge_DownloadedFile write ge_DownloadedFile;
    property {$IFDEF LNET} LNetComponent: TLComponent{$ELSE}IdComponent: TIdTCPConnection{$ENDIF} read gc_Component write p_SetComponent;
    property Buffered: boolean read gb_Buffered write gb_Buffered default False;
    property Messages: boolean read gb_Messages write gb_Messages default True;
  end;

var
  gb_IsUpdating: boolean = False;

implementation

uses fonctions_string,
  {$IFDEF FPC}
  unite_messages,
  LazFileUtils,
  {$ELSE}
  unite_messages_delphi,
  fonctions_system,
  {$ENDIF}
  {$IFDEF LNET}
  lHTTPUtil,
  {$ENDIF}
  fonctions_dialogs,
  {$IFDEF MD5}
  md5api,
  {$ENDIF}
  Forms;


{ TNetUpdate }

// init
constructor TNetUpdate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  gpb_Progress := nil;
  gc_Component := nil;
  ge_Downloaded := nil;
  ge_Downloading := nil;
  ge_DownloadedPage := nil;
  ge_DownloadedFile := nil;
  ge_ErrorEvent := nil;
  ge_ProgressEvent := nil;
  ge_IniRead := nil;
  gst_Stream := nil;
  gini_inifile := nil;
  gi_Weight := 0;
  gsu_UpdateState := suNeedVerify;
  gus_UpdateStep := usNone;
  gb_Buffered := False;
  gb_Messages := True;
end;

// auto delete update files
destructor TNetUpdate.Destroy;
begin
  if (gs_UpdateDir > '') and (gs_FilePage > '') and
    FileExistsUTF8(gs_UpdateDir + gs_FilePage) then
    DeleteFileUTF8(gs_UpdateDir + gs_FilePage);

  inherited Destroy;
end;

// MD5 from file
procedure TNetUpdate.SetMD5;
begin
  {$IFDEF MD5}
  {$IFDEF LNET}
  if gb_Buffered and ( gs_Buffer > '' ) Then
    Begin
      gs_md5File := MD5DataToStr(MD5DataFromString(gs_Buffer));
    End
  else
  {$ENDIF}
  if not gb_Buffered and FileExistsUTF8(gs_UpdateDir + gs_File) then
   begin
    // Matthieu : comparing files
    gs_md5File := MD5FromFile(gs_UpdateDir + gs_File);
   end
  else
  {$ENDIF}
    gs_md5File := '';

end;

// if file verify if there isa file
// return file name
function TNetUpdate.GetOptionalFilename:String;
Begin
  if not gb_Buffered Then
   Begin
    Result := (gst_Stream as {$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF} ).FileName;
    if not FileExistsUTF8(Result)
    and gb_Messages  Then
      Begin
       MyMessageDlg(gs_Error_Cannot_load_not_downloaded_file);
       Exit;
      end;
   end
  Else Result := '';
end;

{$IFDEF LNET}

// lnet input end
procedure TNetUpdate.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
var
  gb_ok : Boolean ;
  ls_File : String;
begin
  try
    gb_IsUpdating := False;
    gb_ok := (LNetComponent as TLHTTPClient).Response.Status = hsOK;
    ASocket.Disconnect;
    Screen.Cursor := crDefault;
    ls_File := GetOptionalFilename;
  finally
    FreeAndNil(gst_Stream);
  End;
  AfterUpdate(ls_File, gb_ok );
end;

// lnet error
procedure TNetUpdate.HTTPClientError(const msg: string; aSocket: TLSocket);
var
  sMessage: string;
  iError: integer;
begin
  FreeAndNil(gst_Stream);
  gb_IsUpdating := False;
  Screen.Cursor := crDefault;
  iError := 0;
  if LNetComponent is TLHTTPClientComponent then
    with LNetComponent as TLHTTPClientComponent do
      case Response.Status of
        hsForbidden, hsNotAllowed, hsNotFound:
        begin
          sMessage := gs_Error_Forbidden_Access;
          iError := 403;
        end;
        hsBadRequest:
        begin
          iError := 400;
          smessage := gs_Error_Bad_request;
        end;
        hsMovedPermanently:
        begin
          sMessage := gs_Error_File_is_not_on_the_web_site;
          iError := 301;
        end;
        hsRequestTooLong:
        begin
          sMessage := gs_Error_timeout_problem;
          iError := 408;
        end;
        hsPreconditionFailed:
        begin
          iError := 502;
          sMessage := gs_Error_Bad_Gateway;
        end
        else
        begin
          sMessage := gs_Error_Bad_Web_Connection;
          iError := 4491;
        end;
      end;
  sMessage := msg + CST_ENDOFLINE + sMessage;
  OnError(iError, sMessage);
end;

// http input
function TNetUpdate.HTTPClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: PChar; ASize: integer): integer;
{$ELSE}
procedure TNetUpdate.IdWork(ASender: TObject; AWorkMode: TWorkMode;
  ASize: {$IFDEF INDY10}Integer{$ELSE}Int64{$ENDIF});
{$ENDIF}
var
  OldLength: integer;
begin
  {$IFDEF LNET}
  if gi_Weight = 0 Then
   Begin
    if gc_Component is TLHTTPClient Then
     gi_Weight:=( gc_Component as TLHTTPClient ).ContentLength;
    {$ELSE}
     // to do
    {$ENDIF}
    if gi_Weight = 0 Then
     gi_Weight:=100000; // do not return here
    if assigned ( gpb_Progress ) Then
      gpb_Progress.Max := gi_Weight ;
  {$IFDEF LNET}
   End;
  {$ENDIF}

  if ASize <= 0 then
    Exit; // nothing to do
  try
    {$IFDEF LNET}
    if gb_Buffered then
    begin
      oldLength := Length(gs_Buffer);
      setlength(gs_Buffer, oldLength + ASize);
      move(ABuffer^, gs_Buffer[oldLength + 1], ASize);
    end
    else
      gst_Stream.WriteBuffer(ABuffer^, ASize);
    Result := aSize; // tell the http buffer we read it all
    {$ENDIF}
    if gpb_Progress <> nil then
    with gpb_Progress do
    begin
      Position := Position + ASize;
      
    end;
    if Assigned(ge_ProgressEvent) then
      ge_ProgressEvent(Self, Asize);

  except
    on e: Exception do
    begin
      if Assigned(ge_ErrorEvent) then
        if gb_Buffered then
          ge_ErrorEvent(Self, 1, e.Message)
        else
          ge_ErrorEvent(Self, 1, fs_RemplaceMsg(GS_ECRITURE_IMPOSSIBLE, [(gst_Stream as {$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF}).Filename]));
      Abort;
    end;
  end;

end;

// unlink design of component
procedure TNetUpdate.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) or (csDestroying in ComponentState) then
    Exit;

  // Suppression d'un composant lnet
  if Assigned(gc_Component) and (AComponent = gc_Component) then
    p_SetComponent ( nil );
  // Suppression d'un composant de progression
  if Assigned(Progress) and (AComponent = Progress) then
    Progress := nil;
end;

// Update ini before updating file with ini
procedure TNetUpdate.UpdateIniPage;
begin
  if CanDownloadIni then
    GetURL(gs_URL, gs_UpdateDir, gs_Ini, '', usIni)
  else if CanDownloadPage then
    GetURL(gs_URL, gs_UpdateDir, gs_FilePage, gs_EndFile, usPage);
end;

// update file with optional ini
procedure TNetUpdate.Update;
begin
  SetMD5;
  if CanDownloadFile then
    if ( gs_MD5Ini > '' ) and ( gs_MD5Ini = gs_md5File ) Then
      Begin
       if Assigned(ge_DownloadedFile) Then
         ge_DownloadedFile ( Self, True );
      end
     Else
      GetURL(gs_URL, gs_UpdateDir, gs_File, gs_EndFile, usFile);
end;

// can we download file ?
function TNetUpdate.CanDownloadFile: boolean;
begin
  if (gs_File = '') or (gs_URL = '') or (gs_UpdateDir = '') then
    Result := False
  else
    Result := True;
end;

// can we download ini ?
function TNetUpdate.CanDownloadIni: boolean;
begin
  if (gs_Ini = '') or (gs_URL = '') or (gs_UpdateDir = '') then
    Result := False
  else
    Result := True;
end;

// can we download page ?
function TNetUpdate.CanDownloadPage: boolean;
begin
  if (gs_FilePage = '') or (gs_URL = '') or (gs_UpdateDir = '') then
    Result := False
  else
    Result := True;
end;

// autoset external net component
procedure TNetUpdate.p_SetComponent(const AValue: {$IFDEF LNET} TLComponent{$ELSE}TIdTCPConnection{$ENDIF});
begin
  gc_Component := AValue;
  if Assigned(gc_Component) {$IFDEF LNET}and (gc_Component is TLHTTPClientComponent){$ENDIF}
   then
   {$IFDEF LNET}
    if not ( csDesigning in ComponentState ) Then
    with gc_Component as TLHTTPClientComponent do
    begin
      OnDoneInput := HTTPClientDoneInput;
      OnInput     := HTTPClientInput;
      OnError     := HTTPClientError;
    end;
   {$ELSE}
     gc_Component.OnWork:=IdWork;
   {$ENDIF}
end;

// update dir set
procedure TNetUpdate.SetUpdateDir(const AValue: String);
begin
  gs_UpdateDir:=AValue;
  if gs_UpdateDir>'' Then
    gs_UpdateDir:=IncludeTrailingPathDelimiter(gs_UpdateDir);
end;

// download ini page or file
procedure TNetUpdate.GetURL(const as_URL, as_LocalDir, as_FileName, as_endFile: string;
  const aus_Step: TUpdateStep = usNone);
var
{$IFDEF LNET}
  aHost, aURI: string;
  aPort: word;
{$ELSE}
  aFile: string;
{$ENDIF}
begin
  if not DirectoryExistsUTF8(gs_UpdateDir) then
   Begin
    ForceDirectoriesUTF8(gs_UpdateDir);
    if not DirectoryExistsUTF8(gs_UpdateDir)
    and gb_Messages Then
      Begin
        MyMessageDlg( fs_RemplaceMsg(gs_Error_Cannot_Write_on,[gs_UpdateDir]));
        Exit;
      end;

   end;

  gus_UpdateStep := aus_Step;
  IncludeTrailingPathDelimiter(as_LocalDir);
  if gpb_Progress <> nil then
   Begin
     gpb_Progress.Position := 0;
     gpb_Progress.Max := gi_Weight;
   End;
  {$IFDEF LNET}
  DecomposeURL(as_URL + as_FileName, aHost, aURI, aPort);
  if gc_Component is TLHTTPClientComponent then
    with gc_Component as TLHTTPClientComponent do
    begin
      Screen.Cursor:=crHourGlass;
      Host := aHost;
      URI := aURI;
      Port := aPort;
  {  if Connect // does not work but should be better
     Then
      Begin
       Disconnect;}
      if gb_Buffered then
        gs_Buffer := ''
      else
      begin
        gst_Stream.Free;
        if FileExistsUTF8(as_LocalDir + as_FileName) then
          DeleteFileUTF8(as_LocalDir + as_FileName);
        gst_Stream := TFileStreamUTF8.Create(as_LocalDir + as_FileName + as_EndFile, fmCreate);
        if Assigned(ge_Downloading) Then
          ge_Downloading ( Self, aus_Step );
      end;
      SendRequest;
  {$ELSE}
  if gc_Component is TIdHTTP then
    with gc_Component as TIdHTTP do
    begin
      if gb_Buffered
        Then gst_Stream:=TMemoryStream.Create
        Else gst_Stream:={$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF}.Create(as_LocalDir+as_FileName,fmCreate);
      try
        (gc_Component as TIdHTTP).Get(as_URL+as_FileName,gst_Stream);
        aFile := GetOptionalFilename;
      finally
        FreeAndNil(gst_Stream);
      end;
    if ResponseCode = 200 Then
      Begin
        AfterUpdate(aFile,True);
      End
     Else
       OnError(ResponseCode, ResponseText);
  {$ENDIF}
    end;
end;

// verify befor downloading
procedure TNetUpdate.VerifyUpdate;
var
  ls_filePath: string;
begin
  if not CanDownloadFile then
    Exit;
  if not gb_Buffered Then
    Begin
      ls_filePath := IncludeTrailingPathDelimiter(gs_UpdateDir) + gs_File;
      if FileExistsUTF8(ls_filePath) then
        if MyMessageDlg(GS_ConfirmCaption,
          gs_Confirm_File_is_unavailable_Do_i_erase_it_to_update_it, mmtConfirmation, mmbYesNo, 0) =
          mrNo then
          Exit
        else
          DeleteFileUTF8(ls_filePath);

    end;
  gb_IsUpdating := True;
  doShowWorking(gs_Please_Wait + CST_ENDOFLINE + fs_RemplaceMsg(
    gs_Downloading_in_progress, [gs_URL + gs_File]));
  GetURL(gs_URL, gs_UpdateDir, gs_File, gs_EndFile);
end;

// ini loading after downloading
procedure TNetUpdate.VerifyIni(const as_file: string);
var
  ls_File : String;
begin
  gini_inifile := TIniFile.Create(as_file);

  if gini_inifile.SectionExists(INI_FILE_UPDATE) then
    try
      if Assigned(ge_CreateIni) then
        ge_CreateIni(Self);
      gi_Weight := gini_inifile.ReadInteger(INI_FILE_UPDATE, INI_FILE_UPDATE_SIZE, 0); // 0 is impossible
      gs_VersionExeUpdate := gini_inifile.ReadString(
        INI_FILE_UPDATE, INI_FILE_UPDATE_EXE_VERSION, '0');
      gs_Date := gini_inifile.ReadString(INI_FILE_UPDATE, INI_FILE_UPDATE_DATE, '0');
      gs_VersionBaseUpdate := gini_inifile.ReadString(INI_FILE_UPDATE,
        INI_FILE_UPDATE_BASE_VERSION, '0');
      gs_MD5Ini := gini_inifile.ReadString(INI_FILE_UPDATE, INI_FILE_UPDATE_MD5, '');
      ls_File:= gini_inifile.ReadString(INI_FILE_UPDATE, INI_FILE_UPDATE_FILE_NAME, '');
      if ls_File > '' Then
        gs_File:=ls_File;

      if Assigned(gpb_Progress) then
        gpb_Progress.Max := gi_Weight;

      DeleteFileUTF8(gs_UpdateDir + gs_Ini);

      if Assigned(ge_IniRead) then
        ge_IniRead(Self,gi_Weight);

    finally
      FreeAndNil(gini_inifile);
    end;
  if FilePage > '' then
    GetURL(gs_URL, gs_UpdateDir, gs_FilePage, gs_EndFile, usPage);
end;

// dispatch after downloading
procedure TNetUpdate.AfterUpdate(const as_file: string;
  const ab_StatusOK: boolean);
var gb_MD5OK : Boolean;
begin
  Screen.Cursor:=crDefault;
  if Assigned(ge_Downloaded) then
    ge_Downloaded(Self, as_file, gus_UpdateStep);
  if (FileSizeUtf8(as_file) > 0) and ab_StatusOK then
    case gus_UpdateStep of
      usIni: VerifyIni(as_file);
      usFile,usPage:
        Begin
          SetMD5;
          gb_MD5OK := gs_MD5Ini = gs_md5File;
          if not gb_MD5OK
            and (gs_MD5Ini > '')
            and (gs_md5File > '')
            and FileExistsUTF8(gs_UpdateDir+gs_File) Then
             DeleteFileUTF8(gs_UpdateDir+gs_File);
          if gus_UpdateStep = usPage Then
           Begin
             if assigned ( ge_DownloadedPage )
              Then ge_DownloadedPage ( Self, gb_MD5OK );
           End
          else
            if assigned ( ge_DownloadedFile )
              Then ge_DownloadedFile ( Self, gb_MD5OK );
        End;
    end;
end;

// optional error event
procedure TNetUpdate.OnError(const ai_error: integer; const as_Message: string);
begin
  if Assigned(ge_ErrorEvent) then
    ge_ErrorEvent(Self, ai_error, as_Message);
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion(gVer_netupdate);
{$ENDIF}
end.
