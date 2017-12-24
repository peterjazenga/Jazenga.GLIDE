{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Lazarus IDE user interface for project-specific codesigning options.)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-18  pk  10m  Localization.
// 2017-05-18  pk  10m  Now shows global settings as hints.
// 2017-05-17  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.ProjectOptions.MicrosoftSignTool.Frame;

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
   PepiMK.Signing.MicrosoftSignTool,
   CodeSigningHelper.Options,
   CodeSigningHelper.ProjectOptions,
   IDEOptionsIntf;

type

   { TFrameCodeSigningProjectOptionsMicrosoftSignTool }

   TFrameCodeSigningProjectOptionsMicrosoftSignTool = class(TAbstractIDEOptionsEditor)
      cbAlgorithmSHA1: TCheckBox;
      cbAlgorithmSHA256: TCheckBox;
      cbAlgorithmsOverride: TCheckBox;
      cbPageHashing: TCheckBox;
      cbTimestampingOverride: TCheckBox;
      cbFlagsOverride: TCheckBox;
      cbTimeStampingServer: TComboBox;
      cbUseTimeStamping: TCheckBox;
      cbWindowsSystemComponentVerification: TCheckBox;
      cbXSignCert: TCheckBox;
      cbCertificateOverride: TCheckBox;
      editCertificateHash: TEditButton;
      editCertificateSubjectSubstring: TEdit;
      editFilenamePFX: TFileNameEdit;
      editXSignCert: TFileNameEdit;
      groupAlgorithms: TGroupBox;
      groupKey: TGroupBox;
      groupMoreOptions: TGroupBox;
      groupTimeStamping: TGroupBox;
      rbSignByCertificateHash: TRadioButton;
      rbSignByCertificatePFXFile: TRadioButton;
      rbSignByCertificateSubstring: TRadioButton;
      procedure cbAlgorithmsOverrideChange({%H-}Sender: TObject);
      procedure cbCertificateOverrideChange({%H-}Sender: TObject);
      procedure cbFlagsOverrideChange({%H-}Sender: TObject);
      procedure cbTimestampingOverrideChange({%H-}Sender: TObject);
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
      procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
      procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
      class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
   end;

implementation

uses
   Dialogs,
   IDEIntf,
   IDEExternToolIntf,
   CodeSigningHelper.Strings,
   CodeSigningHelper.Debug,
   CodeSigningHelper.Certificates.Form;

{$R *.lfm}

{ TFrameCodeSigningProjectOptionsMicrosoftSignTool }

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.cbCertificateOverrideChange(Sender: TObject);
begin
   groupKey.Enabled := cbCertificateOverride.Checked;
   Save;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.cbFlagsOverrideChange(Sender: TObject);
begin
   groupMoreOptions.Enabled := cbFlagsOverride.Checked;
   Save;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.cbAlgorithmsOverrideChange(Sender: TObject);
begin
   groupAlgorithms.Enabled := cbAlgorithmsOverride.Checked;
   Save;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.cbTimestampingOverrideChange(Sender: TObject);
begin
   groupTimeStamping.Enabled := cbTimestampingOverride.Checked;
   Save;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.editCertificateHashButtonClick(Sender: TObject);
var
   s: string;
begin
   s := editCertificateHash.Text;
   if SelectWindowsCertificate(s) then begin
      editCertificateHash.Text := s;
   end;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.ControlChange(Sender: TObject);
begin
   Save;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.Save;
begin
   if not FLoading then begin
      WriteSettings(SupportedOptionsClass.GetInstance);
      TCodeSigningProjectOptions(SupportedOptionsClass.GetInstance).Write;
   end;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.ApplyLocalizedTexts;
begin
   cbAlgorithmsOverride.Caption := 'Use project-specific algorithms:';
   groupAlgorithms.Caption := rsCodeSigningOptionsAlgorithms;
   cbAlgorithmSHA1.Caption := rsCodeSigningOptionsSignWithSHA1;
   cbAlgorithmSHA256.Caption := rsCodeSigningOptionsSignWithSHA256;

   cbCertificateOverride.Caption := 'Use a project-specific signing key:';
   groupKey.Caption := rsCodeSigningOptionsSigningKey;
   rbSignByCertificateHash.Caption := rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash;
   rbSignByCertificateSubstring.Caption := rsCodeSigningOptionsSpecifiyKeyByPartOfSubject;
   rbSignByCertificatePFXFile.Caption := rsCodeSigningOptionsSpecifiyKeyAndCertificateAsPfxFile;
   cbXSignCert.Caption := rsCodeSigningOptionsUseCrossSigningCertificate;

   cbTimestampingOverride.Caption := 'Use project-specific timestamping:';
   groupTimeStamping.Caption := rsCodeSigningOptionsTimestamping;
   cbUseTimeStamping.Caption := rsCodeSigningOptionsUseTimestamping;

   cbFlagsOverride.Caption := 'Use project-specific flags:';
   groupMoreOptions.Caption := rsCodeSigningOptionsMoreOptions;
   cbPageHashing.Caption := rsCodeSigningOptionsPageHashingPh;
   cbWindowsSystemComponentVerification.Caption := rsCodeSigningOptionsWindowsSystemComponentVerificationUw;
end;

function TFrameCodeSigningProjectOptionsMicrosoftSignTool.GetTitle: string;
begin
   Result := rsCodeSigningOptionsTitleMicrosoftSignTool;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   CodeSigningLogInformation(Self, 'Setup');
   ApplyLocalizedTexts();
   ReadSettings(TCodeSigningProjectOptions.GetInstance);
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.ReadSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningProjectOptions;
begin
   FLoading := True;
   try
      CodeSigningLogInformation(Self, 'ReadSettings');
      o := TCodeSigningProjectOptions(AOptions);
      o.Read;
      cbAlgorithmSHA1.Checked := o.MicrosoftSignToolOptions.SignWithSHA1;
      cbAlgorithmSHA1.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.SignWithSHA1, True)]);
      cbAlgorithmSHA256.Checked := o.MicrosoftSignToolOptions.SignWithSHA256;
      cbAlgorithmSHA256.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.SignWithSHA256, True)]);

      cbCertificateOverride.Checked := o.MicrosoftSignToolCustom.UseSpecificCertificate;
      cbCertificateOverrideChange(nil);
      case o.MicrosoftSignToolOptions.Certificate.Source of
         cscsStoreByHash: rbSignByCertificateHash.Checked := True;
         cscsFileAsPFX: rbSignByCertificatePFXFile.Checked := True;
         cscsStoreBySubstring: rbSignByCertificateSubstring.Checked := True;
      end;
      rbSignByCertificateHash.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.Certificate.Source = cscsStoreByHash, True)]);
      rbSignByCertificatePFXFile.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.Certificate.Source = cscsFileAsPFX, True)]);
      rbSignByCertificateSubstring.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.Certificate.Source = cscsStoreBySubstring, True)]);
      editCertificateHash.Text := o.MicrosoftSignToolOptions.Certificate.Hash;
      editCertificateHash.Hint := CodeSigningOptions.MicrosoftSignToolOptions.Certificate.Hash;
      editFilenamePFX.Text := UTF8Encode(o.MicrosoftSignToolOptions.Certificate.Filename);
      editFilenamePFX.Hint := UTF8Encode(CodeSigningOptions.MicrosoftSignToolOptions.Certificate.Filename);
      editCertificateSubjectSubstring.Text := o.MicrosoftSignToolOptions.Certificate.SubString;
      editCertificateSubjectSubstring.Hint := CodeSigningOptions.MicrosoftSignToolOptions.Certificate.SubString;
      cbXSignCert.Checked := o.MicrosoftSignToolOptions.CrossSigning.Active;
      cbXSignCert.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.CrossSigning.Active, True)]);
      editXSignCert.Text := UTF8Encode(o.MicrosoftSignToolOptions.CrossSigning.Filename);
      editXSignCert.Hint := UTF8Encode(CodeSigningOptions.MicrosoftSignToolOptions.CrossSigning.Filename);

      cbTimestampingOverride.Checked := o.MicrosoftSignToolCustom.UseSpecificTimestamping;
      cbTimestampingOverrideChange(nil);
      cbUseTimeStamping.Checked := o.MicrosoftSignToolOptions.Timestamping.Active;
      cbUseTimeStamping.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.Timestamping.Active, True)]);
      cbTimeStampingServer.Text := o.MicrosoftSignToolOptions.Timestamping.ServerRFC3161;
      cbTimeStampingServer.Hint := CodeSigningOptions.MicrosoftSignToolOptions.Timestamping.ServerRFC3161;

      cbFlagsOverride.Checked := o.MicrosoftSignToolCustom.UseSpecificFlags;
      cbFlagsOverrideChange(nil);
      cbWindowsSystemComponentVerification.Checked := o.MicrosoftSignToolOptions.Flags.UseWindowsSystemComponentVerification;
      cbWindowsSystemComponentVerification.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.MicrosoftSignToolOptions.Flags.UseWindowsSystemComponentVerification, True)]);
      case o.MicrosoftSignToolOptions.Flags.UsePageHashing of
         csphYes: cbPageHashing.State := cbChecked;
         csphNo: cbPageHashing.State := cbUnchecked;
         else
            cbPageHashing.State := cbGrayed;
      end;
   finally
      FLoading := False;
   end;
end;

procedure TFrameCodeSigningProjectOptionsMicrosoftSignTool.WriteSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningProjectOptions;
begin
   CodeSigningLogInformation(Self, 'WriteSettings');
   o := TCodeSigningProjectOptions(AOptions);
   o.MicrosoftSignToolCustom.UseSpecificCertificate := cbCertificateOverride.Checked;
   o.MicrosoftSignToolOptions.SignWithSHA1 := cbAlgorithmSHA1.Checked;
   o.MicrosoftSignToolOptions.SignWithSHA256 := cbAlgorithmSHA256.Checked;

   o.MicrosoftSignToolCustom.UseSpecificCertificate := cbCertificateOverride.Checked;
   if rbSignByCertificateHash.Checked then begin
      o.MicrosoftSignToolOptions.Certificate.Source := cscsStoreByHash;
   end else if rbSignByCertificatePFXFile.Checked then begin
      o.MicrosoftSignToolOptions.Certificate.Source := cscsFileAsPFX;
   end else begin
      o.MicrosoftSignToolOptions.Certificate.Source := cscsStoreBySubstring;
   end;
   o.MicrosoftSignToolOptions.Certificate.Hash := editCertificateHash.Text;
   o.MicrosoftSignToolOptions.Certificate.Filename := UTF8Decode(editFilenamePFX.Text);
   o.MicrosoftSignToolOptions.Certificate.SubString := editCertificateSubjectSubstring.Text;
   o.MicrosoftSignToolOptions.CrossSigning.Active := cbXSignCert.Checked;
   o.MicrosoftSignToolOptions.CrossSigning.Filename := UTF8Decode(editXSignCert.Text);

   o.MicrosoftSignToolCustom.UseSpecificTimestamping := cbTimestampingOverride.Checked;
   o.MicrosoftSignToolOptions.Timestamping.Active := cbUseTimeStamping.Checked;
   o.MicrosoftSignToolOptions.Timestamping.ServerRFC3161 := cbTimeStampingServer.Text;

   o.MicrosoftSignToolCustom.UseSpecificFlags := cbFlagsOverride.Checked;
   o.MicrosoftSignToolOptions.Flags.UseWindowsSystemComponentVerification := cbWindowsSystemComponentVerification.Checked;
   case cbPageHashing.State of
      cbChecked: o.MicrosoftSignToolOptions.Flags.UsePageHashing := csphYes;
      cbUnchecked: o.MicrosoftSignToolOptions.Flags.UsePageHashing := csphNo;
      else
         o.MicrosoftSignToolOptions.Flags.UsePageHashing := csphDefault;
   end;
   o.MicrosoftSignToolOptions.Certificate.Filename := UTF8Decode(editFilenamePFX.Text);
   o.Write;
end;

class function TFrameCodeSigningProjectOptionsMicrosoftSignTool.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningProjectOptions;
end;

initialization

   RegisterIDEOptionsEditor(GroupProject, TFrameCodeSigningProjectOptionsMicrosoftSignTool, 800);

end.
