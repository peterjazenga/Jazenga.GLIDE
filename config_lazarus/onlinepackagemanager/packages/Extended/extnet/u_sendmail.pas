unit u_sendmail;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFnDEF FPC}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdAntiFreezeBase, IdAntiFreeze, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdMessageClient, IdSMTP, StdCtrls,
  U_FormAdapt, u_framework_dbcomponents, u_framework_components,
  u_buttons_appli, u_buttons_defs, JvXPCheckCtrls,
  IdAttachmentMemory, u_mailssendbutton,
  IdMessage, ComCtrls, ExtCtrls, Buttons, JvXPButtons,
  Grids, ValEdit, JvExExtCtrls, U_OnFormInfoIni;

type

  { TF_SendMails }

  TF_SendMails = class(TF_FormAdapt)
    annule: TFWCancel;
    bt_civilite: TJvXPButton;
    con: TJvXPButton;
    decon: TJvXPButton;
    destinataire: TFWEdit;
    ed_MotPasse: TFWEdit;
    ed_serveur: TFWEdit;
    ed_Utilisateur: TFWEdit;
    env: TJvXPButton;
    expediteur: TFWEdit;
    Label1: TFWLabel;
    Label2: TFWLabel;
    Label3: TFWLabel;
    Label4: TFWLabel;
    Label5: TFWLabel;
    Label7: TFWLabel;
    lb_Utilisateur: TFWLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    rep: TJvXPButton;
    smtp: TIdSMTP;
    IdAntiFreeze1: TIdAntiFreeze;
    mess: TIdMessage;
    GroupBox1: TGroupBox;
    progresse: TProgressBar;
    dialog: TOpenDialog;
    status: TStatusBar;
    pa_Droite: TPanel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    piece: TFWMemo;
    pa_Message: TPanel;
    sujet: TFWEdit;
    texte: TFWMemo;
    pa_ParamMessage: TPanel;
    JvSplitter1: TSplitter;
    SvgFormInfoIni: TOnFormInfoIni;
    ch_Canton: TJvXPCheckBox;

    procedure envClick(Sender: TObject);
    procedure conClick(Sender: TObject);
    procedure deconClick(Sender: TObject);
    procedure expediteurChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupBox3Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure pa_ParamMessageClick(Sender: TObject);
    procedure repClick(Sender: TObject);
    procedure annuleClick(Sender: TObject);
    procedure sujetChange(Sender: TObject);
  private
    { Déclarations privées }
    FMailButton : TExtSendMails;
  public
    { Déclarations publiques }
    property MailButton : TExtSendMails read FMailButton write FMailButton;
  end;

var
  F_SendMails: TF_SendMails;
   pj : integer;

implementation

{$R *.lfm}

uses DB ;

procedure TF_SendMails.envClick(Sender: TObject);
var
  a : integer;
  Mail,
  m,test : string;
  ls_LIste : String ;
  dat_Dataset : TDataset ;
  ds_Datasource : TDatasource ;

begin
try
  conClick ( Sender );
  m := '';
  if (expediteur.Text = '') or ((destinataire.Text = '')) then
  begin
  showmessage('remplir les champs destinataires et expéditeur.');
  end
  else
      if smtp.Connected then
      begin
       progresse.Position := 2 ;
       con.Enabled := false;
       decon.Enabled := false;
       env.Enabled := false;
       rep.Enabled := false;
       annule.Enabled := false;
       progresse.visible := true;

       test := inttostr(piece.lines.Count);

       progresse.Position := 3 ;

       if (piece.lines.Count > 0) then
         begin
           for a := 0 to piece.lines.Count - 1 do
             if ( trim ( piece.lines[a] ) <> '' ) Then
               begin
                 with TIdAttachmentMemory.Create(mess.MessageParts) do
                   Begin
                      DataString := piece.lines[a];
                   end;
               end;
         End ;

      mess.From.Text := expediteur.Text;
      mess.Subject := sujet.Text;
      mess.Body.Text := m;
      progresse.Position := 4 ;
     // if Liste.Checked then
        Begin
          if  not ch_Canton.Checked   Then
            Begin
              //dat_Dataset   := M_Donn.adoq_MailsMilitant ;
              //ds_Datasource := M_Donn.ds_MailsMilitant;
              //M_Donn.adoq_MailsMilitant.Open;

            End
          Else
            if  ch_Canton.Checked  Then
              Begin
                //dat_Dataset := M_Donn.ADOQ_MailsVillCant ;
                //ds_Datasource := M_Donn.ds_MailsVillCant;
                //M_Donn.ADOQ_MailsVillCant.Close ;
                //M_Donn.ADOQ_MailsVillCant.Parameters.ParamByName ( 'canton' ).Value := cb_Cantons.KeyValue ;
                //M_Donn.ADOQ_MailsVillCant.Parameters.ParamByName ( 'ville'  ).Value := cb_Villes .KeyValue ;
                //M_Donn.ADOQ_MailsVillCant.Open ;

              End
            Else
              if  ch_Canton.Checked   Then
                Begin
                  //ds_Datasource := M_Donn.ds_MailsCanton;
                  //dat_Dataset := M_Donn.ADOQ_MailsCanton ;
                  //M_Donn.ADOQ_MailsCanton.Close ;
                  //M_Donn.ADOQ_MailsCanton.Parameters.ParamByName ( 'canton' ).Value := cb_Cantons.KeyValue ;
                  //M_Donn.ADOQ_MailsCanton.Open ;

                End
               else
                  Begin
                    //dat_Dataset := M_Donn.ADOQ_MailsVille ;
                    //ds_Datasource := M_Donn.ds_MailsVille;
                    //M_Donn.ADOQ_MailsVille.Close ;
                    //M_Donn.ADOQ_MailsVille.Parameters.ParamByName ( 'ville'  ).Value := cb_Villes.KeyValue ;
                    //M_Donn.ADOQ_MailsVille.Open ;

                  End ;
          progresse.Position := 5 ;
          if not dat_Dataset.IsEmpty Then
             Begin
               dat_Dataset.First ;

               if assigned ( FMailButton.OnCreateResult )
                 then FMailButton.OnCreateResult ( Self );

               while not dat_Dataset.Eof do
                 Begin
                   Mail := dat_Dataset.FieldByName ( FMailButton.FieldMail ).AsString ;
                   if pos ( '@', Mail ) > 0
                    Then
                     Begin
                       ls_Liste := ls_LIste + Mail + ',' ;
                     End ;
                    dat_Dataset.Next ;
                 End ;
             End ;
        End ;
      progresse.Position := 6 ;
      if ( destinataire.Text <> '' ) Then
        ls_LIste := ls_LIste + destinataire.Text ;

      if pos ( '@', ls_LIste )=0 Then
        Begin
          ShowMessage ( 'Aucun destinataire' );
        End
       Else
        Begin
          progresse.Position := 7 ;
          mess.Recipients.EMailAddresses := ls_LIste ;
          smtp.Send(mess);

          if smtp.Connected then
          begin
             progresse.Position := 8 ;
             smtp.disconnect;
             progresse.visible := false;
             status.Panels[0].Text := 'Vous n''êtes pas connecté à un serveur' ;
             con.Enabled := true;
             decon.Enabled := true;
             env.Enabled := true;
             rep.Enabled := true;
             annule.Enabled := true;
          end;
        End;


end
else
    begin
    showmessage('Avant d''envoyer un mail il faut remplir un nom de serveur smtp correct.');
    end;
except
  on exc:Exception do
    ShowMessage ( exc.Message );
End ;
 progresse.visible := false;
 con.Enabled := true;
 decon.Enabled := true;
 env.Enabled := true;
 rep.Enabled := true;
 annule.Enabled := true;
end;


procedure TF_SendMails.conClick(Sender: TObject);
begin
progresse.Position := 0 ;
try
  if not smtp.Connected then
  begin
    if (ed_serveur.Text <> '') then
    begin
      con.Enabled := false;
      decon.Enabled := false;
      env.Enabled := false;
      rep.Enabled := false;
      annule.Enabled := false;
      smtp.Port := 25;
      smtp.Host := ed_serveur.Text;
      if ( ed_Utilisateur.Text <> '' ) Then
        Begin
          smtp.AuthType := satDefault ;
          smtp.Username := ed_Utilisateur.Text ;
          smtp.Password := ed_MotPasse.Text ;
        End
       Else
         smtp.AuthType := satNone ;

      smtp.Port := 25;
      smtp.Connect;
      status.Panels[0].Text := 'Vous etes connecté à un serveur' ;
      con.Enabled := true;
      decon.Enabled := true;
      env.Enabled := true;
      rep.Enabled := true;
      annule.Enabled := true;
    end;
  End ;
progresse.Position := 1 ;
except
  On Exc: exception do
    ShowMessage ( exc.message );
end;



end;

procedure TF_SendMails.deconClick(Sender: TObject);
begin
if smtp.Connected then
begin
smtp.disconnect;
status.Panels[0].Text := 'Vous n etes pas connecté à un serveur' ;
end
else
showmessage('t es deja déconnecté trou de cul');
end;

procedure TF_SendMails.expediteurChange(Sender: TObject);
begin

end;



procedure TF_SendMails.FormCreate(Sender: TObject);
begin
pj := 0;
progresse.visible := false;
status.Panels[0].Text := 'Vous n''êtes pas connecté à un serveur' ;
end;

procedure TF_SendMails.GroupBox3Click(Sender: TObject);
begin

end;

procedure TF_SendMails.Panel3Click(Sender: TObject);
begin

end;

procedure TF_SendMails.pa_ParamMessageClick(Sender: TObject);
begin

end;

procedure TF_SendMails.repClick(Sender: TObject);
begin
 dialog.Execute;
 piece.lines.add(dialog.FileName);
end;

procedure TF_SendMails.annuleClick(Sender: TObject);
begin
if  piece.lines.Count <> 0 then
begin
piece.Clear;
end
else
showmessage('On ne peut pas effacer une piece jointe qui n''existe pas.');
end;

procedure TF_SendMails.sujetChange(Sender: TObject);
begin

end;



end.
