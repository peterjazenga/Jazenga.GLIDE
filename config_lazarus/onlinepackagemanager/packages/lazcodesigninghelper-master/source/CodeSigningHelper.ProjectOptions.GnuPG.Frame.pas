{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(TODO : please fill in abstract here!)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-22  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.ProjectOptions.GnuPG.Frame;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   FileUtil,
   Forms,
   Controls,
   StdCtrls,
   EditBtn,
   PepiMK.Signing.Base,
   CodeSigningHelper.Options,
   CodeSigningHelper.ProjectOptions,
   IDEOptionsIntf;

type

   { TFrameCodeSigningProjectOptionsGnuPG }

   TFrameCodeSigningProjectOptionsGnuPG = class(TAbstractIDEOptionsEditor)
      cbCertificateOverride: TCheckBox;
      editCertificateHash: TEditButton;
      editCertificateSubjectSubstring: TEdit;
      groupKey: TGroupBox;
      rbSignByCertificateHash: TRadioButton;
      rbSignByCertificateSubstring: TRadioButton;
      procedure cbCertificateOverrideChange({%H-}Sender: TObject);
      procedure editCertificateHashButtonClick({%H-}Sender: TObject);
      procedure ControlChange({%H-}Sender: TObject);
   private
      { private declarations }
      FLoading: boolean;
      procedure Save;
   public
      { public declarations }
      procedure ApplyLocalizedTexts();
      function GetTitle: string; override;
      procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
      procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
      procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
      class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
   end;

implementation

{$R *.lfm}

uses
   CodeSigningHelper.Strings,
   CodeSigningHelper.Debug,
   CodeSigningHelper.Certificates.Form;

{ TFrameCodeSigningProjectOptionsGnuPG }

procedure TFrameCodeSigningProjectOptionsGnuPG.editCertificateHashButtonClick(Sender: TObject);
var
   s: string;
begin
   s := editCertificateHash.Text;
   if SelectGnuPGCertificate(s) then begin
      editCertificateHash.Text := s;
   end;
   Save;
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.ControlChange(Sender: TObject);
begin
   Save;
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.Save;
begin
   if not FLoading then begin
      WriteSettings(SupportedOptionsClass.GetInstance);
      TCodeSigningProjectOptions(SupportedOptionsClass.GetInstance).Write;
   end;
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.cbCertificateOverrideChange(Sender: TObject);
begin
   groupKey.Enabled := cbCertificateOverride.Checked;
   Save;
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.ApplyLocalizedTexts;
begin
   cbCertificateOverride.Caption := 'Use a project-specific signing key:';
   groupKey.Caption := rsCodeSigningOptionsSigningKey;
   rbSignByCertificateHash.Caption := rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash;
   rbSignByCertificateSubstring.Caption := rsCodeSigningOptionsSpecifiyKeyByPartOfSubject;
end;

function TFrameCodeSigningProjectOptionsGnuPG.GetTitle: string;
begin
   Result := 'GnuPG Signing';
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   CodeSigningLogInformation(Self, 'Setup');
   ApplyLocalizedTexts();
   ReadSettings(SupportedOptionsClass.GetInstance);
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.ReadSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningProjectOptions;
begin
   FLoading := True;
   try
      CodeSigningLogInformation(Self, 'ReadSettings');
      o := TCodeSigningProjectOptions(AOptions);
      o.Read;
      cbCertificateOverride.Checked := o.GnuPGCustom.UseSpecificCertificate;
      cbCertificateOverrideChange(nil);
      case o.GnuPGOptions.Certificate.Source of
         cscsStoreByHash: rbSignByCertificateHash.Checked := True;
         cscsStoreBySubstring: rbSignByCertificateSubstring.Checked := True;
         else
            rbSignByCertificateSubstring.Checked := True;
      end;
      rbSignByCertificateHash.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.GnuPGOptions.Certificate.Source = cscsStoreByHash, True)]);
      rbSignByCertificateSubstring.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.GnuPGOptions.Certificate.Source = cscsStoreBySubstring, True)]);

      editCertificateHash.Text := o.GnuPGOptions.Certificate.Hash;
      editCertificateHash.Hint := CodeSigningOptions.GnuPGOptions.Certificate.Hash;
      editCertificateSubjectSubstring.Text := o.GnuPGOptions.Certificate.SubString;
      editCertificateSubjectSubstring.Hint := CodeSigningOptions.GnuPGOptions.Certificate.SubString;
   finally
      FLoading := False;
   end;
end;

procedure TFrameCodeSigningProjectOptionsGnuPG.WriteSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningProjectOptions;
begin
   CodeSigningLogInformation(Self, 'WriteSettings');
   o := TCodeSigningProjectOptions(AOptions);
   o.GnuPGCustom.UseSpecificCertificate := cbCertificateOverride.Checked;
   if rbSignByCertificateHash.Checked then begin
      o.GnuPGOptions.Certificate.Source := cscsStoreByHash;
   end else begin
      o.GnuPGOptions.Certificate.Source := cscsStoreBySubstring;
   end;
   o.GnuPGOptions.Certificate.Hash := editCertificateHash.Text;
   o.GnuPGOptions.Certificate.SubString := editCertificateSubjectSubstring.Text;
   o.Write;
end;

class function TFrameCodeSigningProjectOptionsGnuPG.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningProjectOptions;
end;

initialization

   RegisterIDEOptionsEditor(GroupProject, TFrameCodeSigningProjectOptionsGnuPG, 802, NoParent, True);

end.
