{ rxlogin unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxlogin;

{$I RX.INC}

interface

uses LResources, LCLType, LCLIntf, SysUtils, LMessages, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TUpdateCaption = (ucNoChange, ucAppTitle, ucFormCaption);

  TRxLoginOption = (rloCustomSelect, rloMoreBtn, rloHelpBtn);

  TRxLoginOptions = set of TRxLoginOption;

  TRxLoginStorageParam = (rlsUserName, rlsTop, rlsLeft, rlsDetailStatus,
    rlsDetailItem);
  TRxLoginStorageParams = set of TRxLoginStorageParam;

  TRxLoginEvent = procedure(Sender: TObject; const UserName, Password: string;
    var AllowLogin: Boolean) of object;
  TCheckUnlockEvent = function(const Password: string): Boolean of object;
  TUnlockAppEvent = procedure(Sender: TObject; const UserName,
    Password: string; var AllowUnlock: Boolean) of object;

  TRxLoginForm = class;

{ TRxCustomLogin }

  TRxCustomLogin = class(TComponent)
  private
    FActive: Boolean;
    FAttemptNumber: Integer;
    FDetailItem: integer;
    FDetailItems: TStrings;
    FLoggedUser: string;
    FMaxPasswordLen: Integer;
    FAllowEmpty: Boolean;
    FLoginOptions: TRxLoginOptions;
    FShowDetails: boolean;
    FStorageParams: TRxLoginStorageParams;
    FUpdateCaption: TUpdateCaption;
    FIniFileName: string;
    FUseRegistry: Boolean;
    FLocked: Boolean;
    FUnlockDlgShowing: Boolean;
    FSaveOnRestore: TNotifyEvent;
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FOnUnlock: TCheckUnlockEvent;
    FOnUnlockApp: TUnlockAppEvent;
    FOnIconDblClick: TNotifyEvent;
    function GetIniFileName: string;
    procedure SetDetailItems(const AValue: TStrings);
    procedure SetLoginOptions(const AValue: TRxLoginOptions);
    procedure SetShowDetails(const AValue: boolean);
    function UnlockHook(var Message: TLMessage): Boolean;
  protected
    function CheckUnlock(const UserName, Password: string): Boolean; dynamic;
    function CreateLoginForm(UnlockMode: Boolean): TRxLoginForm; virtual;
    procedure DoAfterLogin; dynamic;
    procedure DoBeforeLogin; dynamic;
    procedure DoIconDblCLick(Sender: TObject); dynamic;
    function DoLogin(var UserName: string): Boolean; virtual; abstract;
    function DoUnlockDialog: Boolean; virtual;
    procedure SetLoggedUser(const Value: string);
    procedure DoUpdateCaption;
    procedure UnlockOkClick(Sender: TObject);
    property Active: Boolean read FActive write FActive default True;
    property AllowEmptyPassword: Boolean read FAllowEmpty write FAllowEmpty default True;
    property AttemptNumber: Integer read FAttemptNumber write FAttemptNumber default 3;
    property IniFileName: string read GetIniFileName write FIniFileName;
    property MaxPasswordLen: Integer read FMaxPasswordLen write FMaxPasswordLen default 0;
    property UpdateCaption: TUpdateCaption read FUpdateCaption write FUpdateCaption default ucNoChange;
    property UseRegistry: Boolean read FUseRegistry write FUseRegistry default False;
    property ShowDetails: boolean read FShowDetails write SetShowDetails;
    property StorageParams:TRxLoginStorageParams read FStorageParams write FStorageParams default [rlsUserName];
    property DetailItems:TStrings read FDetailItems write SetDetailItems;
    property DetailItem:integer read FDetailItem write FDetailItem;
    property LoginOptions:TRxLoginOptions read FLoginOptions write SetLoginOptions default [rloCustomSelect, rloMoreBtn, rloHelpBtn];

    property AfterLogin: TNotifyEvent read FAfterLogin write FAfterLogin;
    property BeforeLogin: TNotifyEvent read FBeforeLogin write FBeforeLogin;
    property OnUnlock: TCheckUnlockEvent read FOnUnlock write FOnUnlock; { obsolete }
    property OnUnlockApp: TUnlockAppEvent read FOnUnlockApp write FOnUnlockApp;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Login: Boolean; virtual;
    procedure TerminateApplication;
    procedure Lock;
    property LoggedUser: string read FLoggedUser;
  end;

{ TRxLoginDialog }

  TRxLoginDialog = class(TRxCustomLogin)
  private
    FOnCheckUser: TRxLoginEvent;
    FUserName:string;
    FFormTop:integer;
    FFormLeft:integer;
    procedure OkButtonClick(Sender: TObject);
    procedure WriteParams;
    procedure LoadParams;
  protected
    function DoCheckUser(const UserName, Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
  published
    property Active;
    property AttemptNumber;
    property IniFileName;
    property DetailItems;
    property DetailItem;
    property MaxPasswordLen;
    property UpdateCaption;
    property UseRegistry;
    property ShowDetails;
    property LoginOptions;
    property StorageParams;
    property OnCheckUser: TRxLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

{ TRxLoginForm }

  TRxLoginForm = class(TForm)
    AppIcon: TImage;
    btnHelp: TBitBtn;
    btnMore: TBitBtn;
    btnCancel: TBitBtn;
    KeyImage: TImage;
    HintLabel: TLabel;
    btnOK: TBitBtn;
    UserNameLabel: TLabel;
    PasswordLabel: TLabel;
    UserNameEdit: TEdit;
    PasswordEdit: TEdit;
    AppTitleLabel: TLabel;
    DataBaseLabel: TLabel;
    CustomCombo: TComboBox;
    procedure btnMoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectDatabase: Boolean;
    FUnlockMode: Boolean;
    FAttempt: Integer;
    FOnFormShow: TNotifyEvent;
    FOnOkClick: TNotifyEvent;
    function GetShowDetailParams: boolean;
    procedure SetLoginOptions(const AValue: TRxLoginOptions);
    procedure SetShowDetailParams(const AValue: boolean);
  public
    { Public declarations }
    AttemptNumber: Integer;
    property Attempt: Integer read FAttempt;
    property SelectDatabase: Boolean read FSelectDatabase write FSelectDatabase;
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnOkClick: TNotifyEvent read FOnOkClick write FOnOkClick;
    property ShowDetailParams:boolean read GetShowDetailParams write SetShowDetailParams;
    property LoginOptions:TRxLoginOptions write SetLoginOptions;
  end;

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
  FormShowEvent, OkClickEvent: TNotifyEvent): TRxLoginForm;

implementation

uses
  Registry, IniFiles, RxAppUtils, RxDConst, rxVclUtils, RxConst;

const
  keyLoginSection                = 'Login Dialog';
  keyLastLoginUserName           = 'Last Logged User';
  keyLastLoginFormTop            = 'Last Logged Form Top';
  keyLastLoginFormLeft           = 'Last Logged Form Left';
  keyLastLoginFormDetailStatus   = 'Last Logged Detail Status';
  keyLastLoginFormDetailSelected = 'Last Logged Selected Detail';


function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
  FormShowEvent, OkClickEvent: TNotifyEvent): TRxLoginForm;
begin
  Result := TRxLoginForm.Create(Application);
  with Result do
  begin
    FSelectDatabase := ASelectDatabase;
    FUnlockMode := UnlockMode;
    if FUnlockMode then
    begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else
    begin
      FormStyle := fsStayOnTop;
    end;
    OnFormShow := FormShowEvent;
    OnOkClick := OkClickEvent;
  end;
end;

{ TRxCustomLogin }

constructor TRxCustomLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDetailItems:=TStringList.Create;
  FActive := True;
  FAttemptNumber := 3;
  FAllowEmpty := True;
  FUseRegistry := False;
  FStorageParams:=[rlsUserName];
  FLoginOptions:=[rloCustomSelect, rloMoreBtn, rloHelpBtn];
end;

destructor TRxCustomLogin.Destroy;
begin
  if FLocked then
  begin
//    Application.UnhookMainWindow(UnlockHook);
    FLocked := False;
  end;
  FreeAndNil(FDetailItems);
  inherited Destroy;
end;

function TRxCustomLogin.GetIniFileName: string;
begin
  Result := FIniFileName;
  if (Result = '') and not (csDesigning in ComponentState) then
  begin
    if UseRegistry then
      Result := GetDefaultIniRegKey
    else
      Result := GetDefaultIniName;
  end;
end;

procedure TRxCustomLogin.SetDetailItems(const AValue: TStrings);
begin
  if Assigned(AValue) then
    FDetailItems.Assign(AValue);
end;

procedure TRxCustomLogin.SetLoginOptions(const AValue: TRxLoginOptions);
begin
  if FLoginOptions=AValue then exit;
  FLoginOptions:=AValue;
end;

procedure TRxCustomLogin.SetShowDetails(const AValue: boolean);
begin
  if FShowDetails=AValue then exit;
  FShowDetails:=AValue;
end;

procedure TRxCustomLogin.SetLoggedUser(const Value: string);
begin
  FLoggedUser := Value;
end;

procedure TRxCustomLogin.DoAfterLogin;
begin
  if Assigned(FAfterLogin) then FAfterLogin(Self);
end;

procedure TRxCustomLogin.DoBeforeLogin;
begin
  if Assigned(FBeforeLogin) then FBeforeLogin(Self);
end;

procedure TRxCustomLogin.DoIconDblCLick(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then FOnIconDblClick(Self);
end;

procedure TRxCustomLogin.DoUpdateCaption;
var
  F: TForm;
begin
  F := Application.MainForm;
  if (F = nil) and (Owner is TForm) then F := Owner as TForm;
  if (F <> nil) and (LoggedUser <> '') then
    case UpdateCaption of
      ucAppTitle:
        F.Caption := Format('%s (%s)', [Application.Title, LoggedUser]);
      ucFormCaption:
        begin
          F.Caption := Format('%s (%s)', [F.Caption, LoggedUser]);
          UpdateCaption := ucNoChange;
        end;
    end;
end;

function TRxCustomLogin.Login: Boolean;
var
  LoginName: string;
begin
  LoginName := EmptyStr;
  DoBeforeLogin;
  Result := DoLogin(LoginName);
  if Result then
  begin
    SetLoggedUser(LoginName);
    DoUpdateCaption;
    DoAfterLogin;
  end;
end;

procedure TRxCustomLogin.Lock;
begin
//  FSaveOnRestore := Application.OnRestore;
  Application.Minimize;
//  Application.HookMainWindow(UnlockHook);
  FLocked := True;
end;

procedure TRxCustomLogin.TerminateApplication;
begin
  with Application do
  begin
    ShowMainForm := False;
{    if Application.Handle <> 0 then
      ShowOwnedPopups(Handle, False);}
    Terminate;
  end;
  CallTerminateProcs;
  Halt(10);
end;

procedure TRxCustomLogin.UnlockOkClick(Sender: TObject);
var
  Ok: Boolean;
begin
  with TRxLoginForm(Sender) do begin
    Ok := False;
    try
      Ok := CheckUnlock(UserNameEdit.Text, PasswordEdit.Text);
    except
      Application.HandleException(Self);
    end;
    if Ok then ModalResult := mrOk
    else ModalResult := mrCancel;
  end;
end;

function TRxCustomLogin.CheckUnlock(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnUnlockApp) then
    FOnUnlockApp(Self, UserName, Password, Result)
  else if Assigned(FOnUnlock) then
    Result := FOnUnlock(Password);
end;

function TRxCustomLogin.CreateLoginForm(UnlockMode: Boolean): TRxLoginForm;
begin
  Result := TRxLoginForm.Create(Application);
  with Result do
  begin
    FUnlockMode := UnlockMode;
    if FUnlockMode then
    begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else
      FormStyle := fsStayOnTop;
    if Assigned(Self.FOnIconDblClick) then
    begin
      with AppIcon do
      begin
        OnDblClick := @DoIconDblClick;
        Cursor := crHandPoint;
      end;
      with KeyImage do
      begin
        OnDblClick := @DoIconDblClick;
        Cursor := crHandPoint;
      end;
    end;
    PasswordEdit.MaxLength := FMaxPasswordLen;
    AttemptNumber := Self.AttemptNumber;
  end;
end;

function TRxCustomLogin.DoUnlockDialog: Boolean;
begin
  with CreateLoginForm(True) do
  try
    OnFormShow := nil;
    OnOkClick := @UnlockOkClick;
    with UserNameEdit do
    begin
      Text := LoggedUser;
      ReadOnly := True;
      Font.Color := clGrayText;
    end;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

function TRxCustomLogin.UnlockHook(var Message: TLMessage): Boolean;

  function DoUnlock: Boolean;
  var
    Popup: HWnd;
  begin
(*    with Application do
      if IsWindowVisible(Application.Handle) and IsWindowEnabled(Handle) then
{$IFDEF WIN32}
        SetForegroundWindow(Handle);
{$ELSE}
        BringWindowToTop(Handle);
{$ENDIF}
    if FUnlockDlgShowing then begin
      Popup := GetLastActivePopup(Application.Handle);
      if (Popup <> 0) and IsWindowVisible(Popup) and
        (WindowClassName(Popup) = TRxLoginForm.ClassName) then
      begin
{$IFDEF WIN32}
        SetForegroundWindow(Popup);
{$ELSE}
        BringWindowToTop(Popup);
{$ENDIF}
      end;  //*)
      Result := False;
(*      Exit;
    end;
    FUnlockDlgShowing := True;
    try
      Result := DoUnlockDialog;
    finally
      FUnlockDlgShowing := False;
    end;
    if Result then begin
      Application.UnhookMainWindow(UnlockHook);
      FLocked := False;
    end;*)
  end;

begin
  Result := False;
  if not FLocked then Exit;
  with Message do begin
    case Msg of
{      LM_QUERYOPEN:
        begin
          UnlockHook := not DoUnlock;
        end;}
      LM_SHOWWINDOW:
        if Bool(WParam) then begin
          UnlockHook := not DoUnlock;
        end;
      LM_SYSCOMMAND:
        if (WParam and $FFF0 = SC_RESTORE)
{          or (WParam and $FFF0 = SC_ZOOM) }then
        begin
          UnlockHook := not DoUnlock;
        end;
    end;
  end;
end;

{ TRxLoginDialog }

procedure TRxLoginDialog.Loaded;
var
  FLoading: Boolean;
begin
  FLoading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and FLoading then
  begin
    if Active and not Login then
      TerminateApplication;
  end;
end;

procedure TRxLoginDialog.OkButtonClick(Sender: TObject);
var
  SC: Boolean;
begin
  with TRxLoginForm(Sender) do
  begin
{$IFDEF WIN32}
    SC := GetCurrentThreadID = MainThreadID;
{$ELSE}
    SC := True;
{$ENDIF}
    try
      if SC then
        Screen.Cursor := crHourGlass;
      try
        if DoCheckUser(UserNameEdit.Text, PasswordEdit.Text) then
          ModalResult := mrOk
        else
          ModalResult := mrNone;
      finally
        if SC then Screen.Cursor := crDefault;
      end;
    except
      Application.HandleException(Self);
    end;
  end;
end;

function TRxLoginDialog.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TRxLoginDialog.WriteParams;
var
  Ini: TObject;
begin
  try
    if UseRegistry then Ini := TRegIniFile.Create(IniFileName)
    else Ini := TIniFile.Create(IniFileName);
    try
      if rlsUserName in FStorageParams then
        IniWriteString(Ini, keyLoginSection, keyLastLoginUserName, FUserName);
      if rlsTop in FStorageParams then
        IniWriteInteger(Ini, keyLoginSection, keyLastLoginFormTop, FFormTop);
      if rlsLeft in FStorageParams then
        IniWriteInteger(Ini, keyLoginSection, keyLastLoginFormLeft, FFormLeft);
      if rlsDetailStatus in FStorageParams then
        IniWriteInteger(Ini, keyLoginSection, keyLastLoginFormDetailStatus, ord(FShowDetails));
      if rlsDetailItem in FStorageParams then
        IniWriteInteger(Ini, keyLoginSection, keyLastLoginFormDetailSelected, FDetailItem);
    finally
      Ini.Free;
    end;
  except
  end;
end;

procedure TRxLoginDialog.LoadParams;
var
  Ini: TObject;
begin
  try
    if UseRegistry then
    begin
      Ini := TRegIniFile.Create(IniFileName);
      TRegIniFile(Ini).Access := KEY_READ;
    end
    else
      Ini := TIniFile.Create(IniFileName);
    try
      if rlsUserName in FStorageParams then
        FUserName:=IniReadString(Ini, keyLoginSection, keyLastLoginUserName, FUserName);
      if rlsTop in FStorageParams then
        FFormTop:=IniReadInteger(Ini, keyLoginSection, keyLastLoginFormTop, FFormTop);
      if rlsLeft in FStorageParams then
        FFormLeft:=IniReadInteger(Ini, keyLoginSection, keyLastLoginFormLeft, FFormLeft);
      if rlsDetailStatus in FStorageParams then
        FShowDetails:=IniReadInteger(Ini, keyLoginSection, keyLastLoginFormDetailStatus, ord(FShowDetails))=1;
      if rlsDetailItem in FStorageParams then
        FDetailItem:=IniReadInteger(Ini, keyLoginSection, keyLastLoginFormDetailSelected, FDetailItem);
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TRxLoginDialog.DoLogin(var UserName: string): Boolean;
var
  LoginForm:TRxLoginForm;
begin
  try
    LoginForm:=CreateLoginForm(False);
    try
      FUserName:=UserName;
      LoginForm.OnOkClick := @Self.OkButtonClick;
      LoadParams;
      LoginForm.LoginOptions:=FLoginOptions;

      if rlsUserName in StorageParams then
        LoginForm.UserNameEdit.Text := FUserName;
      if rlsTop in StorageParams then
        LoginForm.Top:=FFormTop;
      if rlsLeft in StorageParams then
        LoginForm.Left:=FFormLeft;

      if rloCustomSelect in LoginOptions then
      begin
        LoginForm.CustomCombo.Items.Assign(DetailItems);
        if (FDetailItem>=0) and (FDetailItem<DetailItems.Count) then
          LoginForm.CustomCombo.ItemIndex:=FDetailItem;
      end;

      LoginForm.ShowDetailParams:=ShowDetails;

      Result := (LoginForm.ShowModal = mrOk);
      if Result then
      begin
        if rlsTop in StorageParams then
          FFormTop:=LoginForm.Top;
        if rlsLeft in StorageParams then
          FFormLeft:=LoginForm.Left;

        if rloCustomSelect in LoginOptions then
          FDetailItem:=LoginForm.CustomCombo.ItemIndex;

        ShowDetails:=LoginForm.ShowDetailParams;
        UserName := LoginForm.UserNameEdit.Text;
        FUserName:=UserName;
        WriteParams;
      end;
    finally
      LoginForm.Free;
    end;
  except
    Application.HandleException(Self);
    Result := False;
  end;
end;

{ TRxLoginForm }

procedure TRxLoginForm.FormCreate(Sender: TObject);
begin
  Icon.Assign(Application.Icon);
//  if Icon.Empty then Icon.Handle := LoadIcon(0, IDI_APPLICATION);
  AppIcon.Picture.Assign(Icon);
  AppTitleLabel.Caption := Format(SAppTitleLabel, [Application.Title]);
  PasswordLabel.Caption := SPasswordLabel;
  UserNameLabel.Caption := SUserNameLabel;
end;

procedure TRxLoginForm.btnMoreClick(Sender: TObject);
begin
  ShowDetailParams:=not ShowDetailParams;
end;

procedure TRxLoginForm.btnOKClick(Sender: TObject);
begin
  Inc(FAttempt);
  if Assigned(FOnOkClick) then FOnOkClick(Self)
  else ModalResult := mrOk;
  if (ModalResult <> mrOk) and (FAttempt >= AttemptNumber) then
    ModalResult := mrCancel;
end;

procedure TRxLoginForm.FormShow(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if FSelectDatabase then
  begin
    ClientHeight := CustomCombo.Top + PasswordEdit.Top - UserNameEdit.Top;
    S := SDatabaseName;
    I := Pos(':', S);
    if I = 0 then I := Length(S);
    DataBaseLabel.Caption := '&' + Copy(S, 1, I);
  end
  else
  begin
    DataBaseLabel.Visible := False;
    CustomCombo.Visible := False;
    btnMore.Visible := False;
  end;

  SetShowDetailParams(ShowDetailParams);

  if not FUnlockMode then
  begin
    HintLabel.Caption := SHintLabel;
    Caption := SRegistration;
  end
  else
  begin
    HintLabel.Caption := SUnlockHint;
    Caption := SUnlockCaption;
  end;

  if (UserNameEdit.Text = EmptyStr) and not FUnlockMode then
    ActiveControl := UserNameEdit
  else 
    ActiveControl := PasswordEdit;
  if Assigned(FOnFormShow) then FOnFormShow(Self);
  FAttempt := 0;
end;

procedure TRxLoginForm.SetShowDetailParams(const AValue: boolean);
begin
  DataBaseLabel.Visible:=AValue;
  CustomCombo.Visible:=AValue;
  if AValue then
  begin
    btnMore.Caption:=SMore2;
    btnCancel.AnchorSideTop.Control:=CustomCombo;
    Height := CustomCombo.Top + CustomCombo.Height + btnCancel.Height + 12;
  end
  else
  begin
    btnMore.Caption:=SMore1;
    btnCancel.AnchorSideTop.Control:=PasswordEdit;
    Height := PasswordEdit.Top + PasswordEdit.Height + btnCancel.Height + 12;
  end;
end;

function TRxLoginForm.GetShowDetailParams: boolean;
begin
  Result:=CustomCombo.Visible;
end;

procedure TRxLoginForm.SetLoginOptions(const AValue: TRxLoginOptions);
begin
  btnHelp.Visible:=rloHelpBtn in AValue;
  if not btnHelp.Visible then
  begin
    btnCancel.AnchorSideLeft.Side:=asrBottom;
    btnCancel.AnchorSideLeft.Control:=Self;
  end;

  btnMore.Visible:=rloMoreBtn in AValue;
  FSelectDatabase:=rloCustomSelect in AValue;
end;

initialization
 {$I rxlogin.lrs}
end.
