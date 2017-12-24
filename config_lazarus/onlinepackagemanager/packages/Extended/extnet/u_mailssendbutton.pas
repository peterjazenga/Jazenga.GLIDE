unit u_mailssendbutton;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, forms,
  {$IFDEF FPC}
   JvXPButtons,
  {$ELSE}
  JvXPButtons,
  {$ENDIF}
  {$IFDEF VERSIONS}
     fonctions_version,
  {$ENDIF}
  Controls, u_buttons_defs, DB, DbCtrls,
  u_framework_dbcomponents;

const
    CST_MAILS_SEND_WIDTH = 64 ;
    CST_MAILS_SEND_WIDTH_SPACE = 4 ;
{$IFDEF VERSIONS}
    gVer_ExtSendMails : T_Version = ( Component : 'Button that send mails.' ;
                                       FileUnit : 'u_SendMailsbutton' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Button that send mails with Datasource' ;
                                       BugsStory : '0.8.0.0 : Not Finished.';
                                       UnitType : 3 ;
                                       Major : 0 ; Minor : 8 ; Release : 0 ; Build : 0 );
{$ENDIF}

{ TExtSendMails }
type
  TExtSendMails = class ( TFWButton, IFWButton )
     private
      FDatalink : TFieldDataLink;
      FFieldMail: String;
      FFormSource : Integer;
      FTableResult,
      FFormRegisteredName,
      FFieldSurname,
      FFieldName,
      FFilter : String;
      FOldCloseAction : TCloseEvent;
      FOnCreateSend,
      FAfterSent,
      FOnSend : TNotifyEvent;
      procedure LoadBitmap;
     protected
      FFormModal : TCustomForm ;
      FOK : Boolean;
      procedure SetFormEvents; virtual;
      procedure CreateForm(const aico_Icon: TIcon); virtual;
      function  CloseForm : Boolean; virtual;
      function  AfterModalHidden : Integer; virtual;
      function fs_GetFieldMail: String;
      function fds_GetDatasource: TDataSource;
      procedure p_SetDatasource ( const Avalue : TDataSource);
      procedure p_setFieldMail ( const AValue : String );
     public
      constructor Create ( AOwner : TComponent ) ; override;
      destructor Destroy; override;
      function  Execute ( const aBmp_Icon : TBitmap = nil ):Integer; virtual;
      procedure CreateFormWithIcon(const aBmp_Icon: TBitmap); virtual;
      procedure Click; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
     published
      procedure OnGridDblClick ( Sender : TObject ); virtual;
      procedure OnCloseModalForm ( Sender: TObject; var AAction: TCloseAction ); virtual;
      property FormSource : Integer read FFormSource write FFormSource default 0;
      property FormRegisteredName : String read FFormRegisteredName write FFormRegisteredName;
      property FieldMail : String read fs_GetFieldMail write p_setFieldMail;
      property FieldName : String read FFieldName write FFieldName;
      property FieldSurname : String read FFieldSurname write FFieldSurname;
      property TableResult : String read FTableResult write FTableResult;
      property Filter : String read FFilter write FFilter;
      property Datasource : TDatasource read fds_GetDatasource write p_SetDatasource;
      property OnSend : TNotifyEvent read FOnSend write FOnSend ;
      property OnCreateResult: TNotifyEvent read FOnCreateSend write FOnCreateSend ;
      property AfterSent : TNotifyEvent read FAfterSent write FAfterSent;
      property Width default CST_MAILS_SEND_WIDTH;
    End;
    TExtSendMailsClass = class of TExtSendMails;

var gefc_SendMailsAutoCreated : TExtSendMailsClass = TExtSendMails;
function ffc_CreateSendMailsButton(const acom_owner : TComponent):TExtSendMails;

implementation

uses fonctions_images, U_FormMainIni, fonctions_proprietes,
     unite_messages, fonctions_forms,
     fonctions_dbobjects, fonctions_dbcomponents, u_sendmail;

function ffc_CreateSendMailsButton(const acom_owner : TComponent):TExtSendMails;
Begin
  Result:=gefc_SendMailsAutoCreated.Create(acom_owner);
end;

{ TExtSendMails }

constructor TExtSendMails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := GS_Send_Mail_Caption;
  ControlStyle := ControlStyle - [csSetCaption];
  FFormSource := 0 ;
  FOnSend := Nil;
  Width := CST_MAILS_SEND_WIDTH;
  FDatalink:=TFieldDataLink.Create;
end;

destructor TExtSendMails.Destroy;
begin
  inherited Destroy;
  FDatalink.Destroy;
end;

function TExtSendMails.AfterModalHidden : Integer;
begin
  Result := mrCancel;
  if assigned ( FAfterSent ) Then
   FAfterSent ( Self );
  FFormModal.Free;
end;

function TExtSendMails.fs_GetFieldMail: String;
begin
  Result:=FDatalink.FieldName;
end;

function TExtSendMails.fds_GetDatasource: TDataSource;
begin
  Result := FDatalink.DataSource;
end;

procedure TExtSendMails.p_SetDatasource(const Avalue: TDataSource);
begin
  FDatalink.DataSource:=Avalue;

end;

procedure TExtSendMails.p_setFieldMail(const AValue: String);
begin
  FDatalink.FieldName := AValue;

end;

function TExtSendMails.Execute(const aBmp_Icon: TBitmap) : Integer;
var lst_OldFilter : String;
    lb_OldFiltered : Boolean;
begin
  Result := -1 ;
  FOK := False;
  if not CloseForm Then
   Exit;
  FFormModal := nil;
  CreateFormWithIcon ( aBmp_Icon );
  if assigned ( FFormModal ) Then
   Begin
       Begin
        p_SetComponentBoolProperty ( FFormModal, 'AutoSize', True );
        FFormModal.Hide;
        p_SetComponentProperty     ( FFormModal, 'Position', poMainFormCenter );
        if ( FFilter <> '' ) then
          with Datasource do
           Begin
             lst_OldFilter  := fs_getComponentProperty(DataSet, CST_DATASET_FILTER);
             lb_OldFiltered := fb_getComponentBoolProperty(DataSet, CST_DATASET_FILTERED);
             p_SetComponentProperty ( DataSet, CST_DATASET_FILTER, FFilter );
             p_SetComponentBoolProperty ( DataSet, CST_DATASET_FILTERED, True );
           end;
        SetFormEvents;
        FFormModal.ShowModal;
        if ( FFilter <> '' ) Then
        with Datasource do
            Begin
              p_SetComponentProperty ( DataSet, CST_DATASET_FILTER, lst_OldFilter );
              p_SetComponentBoolProperty ( DataSet, CST_DATASET_FILTERED, lb_OldFiltered );
            end;
        Result := AfterModalHidden;
       end;
   end;

end;

procedure  TExtSendMails.SetFormEvents;
var
    lmet_Event: TMethod;
Begin
  lmet_Event.Data := Self;
  lmet_Event.Code := MethodAddress('OnCloseModalForm');
  FOldCloseAction := TCloseEvent ( fmet_getComponentMethodProperty (  FFormModal, CST_FORM_ONCLOSE ));
  p_SetComponentMethodProperty ( FFormModal, CST_FORM_ONCLOSE, lmet_Event );
  lmet_Event.Code := MethodAddress('OnGridDblClick');
  p_SetComponentObjectProperty(FFormModal, 'MailButton', Self );
end;

procedure TExtSendMails.CreateForm(const aico_Icon: TIcon);
var lfs_newFormStyle : TFormStyle ;
begin
  FFormModal := nil;
  lfs_newFormStyle := fsMDIChild;

  if FFormRegisteredName <> '' Then
    Begin
      if  ( Application.MainForm is TF_FormMainIni )
       Then
    end ;
  if FFormModal = nil Then
    FFormModal := TF_SendMails.Create(Self);

  if assigned ( FOnCreateSend ) Then
    FOnCreateSend ( FFormModal );
end;

function TExtSendMails.CloseForm : Boolean;
var lfor_FormToClose : TCustomForm;
begin
  Result := True;
  lfor_FormToClose := nil;
  if  ( Application.MainForm is TF_FormMainIni )
   Then
    lfor_FormToClose := ffor_FindForm ( FFormRegisteredName );
  if ( lfor_FormToClose = nil ) Then
    Exit;
  if ( fsModal in lfor_FormToClose.FormState ) Then
    Result := False
   Else
    lfor_FormToClose.Free;
end;

procedure TExtSendMails.CreateFormWithIcon(
  const aBmp_Icon: TBitmap);
var lico_Icon : TIcon ;
begin
  lico_Icon := Nil ;
  if assigned ( abmp_Icon ) then
    p_BitmapVersIco(abmp_Icon, lico_Icon);
  CreateForm ( lico_Icon );
  if assigned ( lico_Icon ) Then
    with lico_Icon do
      if Handle <> 0 Then
        Begin
          ReleaseHandle ;
          Handle := 0 ;
          Free ;
        End ;
end;

procedure TExtSendMails.Click;
begin
  if assigned ( OnClick ) Then
   inherited Click
  Else
    Begin
      Execute;
    end;
end;

procedure TExtSendMails.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove Then Exit;
  if AComponent = Datasource then
   Datasource:=nil;
end;

procedure TExtSendMails.OnGridDblClick(Sender: TObject);
begin
  FOK := True;
  FFormModal.Close ;
end;

procedure TExtSendMails.OnCloseModalForm(Sender: TObject;
  var AAction: TCloseAction);
begin
  if assigned ( FOldCloseAction ) Then
    FOldCloseAction ( Sender, AAction );
  AAction:=caHide;
end;

procedure TExtSendMails.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_ExtSendMails, Self);
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_ExtSendMails  );
{$ENDIF}
end.

