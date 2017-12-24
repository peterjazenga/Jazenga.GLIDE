{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Lazarus IDE user interface for codesigning options.)

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
// 2017-05-18  pk   5m  Grouped code lines by TGroupBox associations.
// 2017-05-11  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Options.MicrosoftSignTool.Frame;

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
   ExtCtrls,
   EditBtn,
   PepiMK.Signing.Base,
   PepiMK.Signing.MicrosoftSignTool,
   IDEOptionsIntf;

type

   { TFrameCodeSigningOptionsMicrosoftSignTool }

   TFrameCodeSigningOptionsMicrosoftSignTool = class(TAbstractIDEOptionsEditor)
      cbTimeStampingServer: TComboBox;
      cbUseTimeStamping: TCheckBox;
      cbAutoSign: TCheckBox;
      cbPageHashing: TCheckBox;
      cbXSignCert: TCheckBox;
      cbWindowsSystemComponentVerification: TCheckBox;
      cbAlgorithmSHA1: TCheckBox;
      cbAlgorithmSHA256: TCheckBox;
      editCertificateSubjectSubstring: TEdit;
      editCertificateHash: TEditButton;
      editExecutable: TComboBox;
      editXSignCert: TFileNameEdit;
      editFilenamePFX: TFileNameEdit;
      groupAutomation: TGroupBox;
      groupAlgorithms: TGroupBox;
      groupMoreOptions: TGroupBox;
      groupKey: TGroupBox;
      groupExecutable: TGroupBox;
      groupTimeStamping: TGroupBox;
      rbSignByCertificateHash: TRadioButton;
      rbSignByCertificateSubstring: TRadioButton;
      rbSignByCertificatePFXFile: TRadioButton;
      procedure editCertificateHashButtonClick({%H-}Sender: TObject);

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
   CodeSigningHelper.Options,
   CodeSigningHelper.Strings,
   CodeSigningHelper.Debug,
   CodeSigningHelper.Certificates.Form;

{ TFrameCodeSigningOptionsMicrosoftSignTool }

procedure TFrameCodeSigningOptionsMicrosoftSignTool.editCertificateHashButtonClick(Sender: TObject);
var
   s: string;
begin
   s := editCertificateHash.Text;
   if SelectWindowsCertificate(s) then begin
      editCertificateHash.Text := s;
   end;
end;

procedure TFrameCodeSigningOptionsMicrosoftSignTool.ApplyLocalizedTexts;
begin
   groupAutomation.Caption := rsCodeSigningOptionsAutomation;
   cbAutoSign.Caption := rsCodeSigningOptionsSignOnSuccessfulCompileOrBuild;

   groupExecutable.Caption := rsCodeSigningOptionsSigningToolExecutable;

   groupAlgorithms.Caption := rsCodeSigningOptionsAlgorithms;
   cbAlgorithmSHA1.Caption := rsCodeSigningOptionsSignWithSHA1;
   cbAlgorithmSHA256.Caption := rsCodeSigningOptionsSignWithSHA256;

   groupKey.Caption := rsCodeSigningOptionsSigningKey;
   rbSignByCertificateHash.Caption := rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash;
   rbSignByCertificateSubstring.Caption := rsCodeSigningOptionsSpecifiyKeyByPartOfSubject;
   rbSignByCertificatePFXFile.Caption := rsCodeSigningOptionsSpecifiyKeyAndCertificateAsPfxFile;
   cbXSignCert.Caption := rsCodeSigningOptionsUseCrossSigningCertificate;

   groupTimeStamping.Caption := rsCodeSigningOptionsTimestamping;
   cbUseTimeStamping.Caption := rsCodeSigningOptionsUseTimestamping;

   groupMoreOptions.Caption := rsCodeSigningOptionsMoreOptions;
   cbPageHashing.Caption := rsCodeSigningOptionsPageHashingPh;
   cbWindowsSystemComponentVerification.Caption := rsCodeSigningOptionsWindowsSystemComponentVerificationUw;
end;

{ TFrameCodeSigningOptionsMicrosoftSignTool }

function TFrameCodeSigningOptionsMicrosoftSignTool.GetTitle: string;
begin
   Result := rsCodeSigningOptionsTitleMicrosoftSignTool;
end;

procedure TFrameCodeSigningOptionsMicrosoftSignTool.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   CodeSigningLogInformation(Self, 'Setup', true);
   ApplyLocalizedTexts();
end;

procedure TFrameCodeSigningOptionsMicrosoftSignTool.ReadSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningOptions;
begin
   CodeSigningLogInformation(Self, 'ReadSettings', true);
   o := TCodeSigningOptions(AOptions);
   cbAutoSign.Checked := o.MicrosoftSignToolOptions.AutoSign;

   editExecutable.Text := UTF8Encode(o.MicrosoftSignToolOptions.SignToolExecutable);

   cbAlgorithmSHA1.Checked := o.MicrosoftSignToolOptions.SignWithSHA1;
   cbAlgorithmSHA256.Checked := o.MicrosoftSignToolOptions.SignWithSHA256;

   case o.MicrosoftSignToolOptions.Certificate.Source of
      cscsStoreByHash: rbSignByCertificateHash.Checked := True;
      cscsFileAsPFX: rbSignByCertificatePFXFile.Checked := True;
      cscsStoreBySubstring: rbSignByCertificateSubstring.Checked := True;
   end;
   editCertificateHash.Text := o.MicrosoftSignToolOptions.Certificate.Hash;
   editFilenamePFX.Text := UTF8Encode(o.MicrosoftSignToolOptions.Certificate.Filename);
   editCertificateSubjectSubstring.Text := o.MicrosoftSignToolOptions.Certificate.SubString;
   cbXSignCert.Checked := o.MicrosoftSignToolOptions.CrossSigning.Active;
   editXSignCert.Text := UTF8Encode(o.MicrosoftSignToolOptions.CrossSigning.Filename);

   cbUseTimeStamping.Checked := o.MicrosoftSignToolOptions.Timestamping.Active;
   cbTimeStampingServer.Text := o.MicrosoftSignToolOptions.Timestamping.ServerRFC3161;

   cbWindowsSystemComponentVerification.Checked := o.MicrosoftSignToolOptions.Flags.UseWindowsSystemComponentVerification;
   case o.MicrosoftSignToolOptions.Flags.UsePageHashing of
      csphYes: cbPageHashing.State := cbChecked;
      csphNo: cbPageHashing.State := cbUnchecked;
      else
         cbPageHashing.State := cbGrayed;
   end;
end;

procedure TFrameCodeSigningOptionsMicrosoftSignTool.WriteSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningOptions;
begin
   CodeSigningLogInformation(Self, 'Write', true);
   o := TCodeSigningOptions(AOptions);
   o.MicrosoftSignToolOptions.AutoSign := cbAutoSign.Checked;

   o.MicrosoftSignToolOptions.SignToolExecutable := UTF8Decode(editExecutable.Text);

   o.MicrosoftSignToolOptions.SignWithSHA1 := cbAlgorithmSHA1.Checked;
   o.MicrosoftSignToolOptions.SignWithSHA256 := cbAlgorithmSHA256.Checked;

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

   o.MicrosoftSignToolOptions.Timestamping.Active := cbUseTimeStamping.Checked;
   o.MicrosoftSignToolOptions.Timestamping.ServerRFC3161 := cbTimeStampingServer.Text;

   o.MicrosoftSignToolOptions.Flags.UseWindowsSystemComponentVerification := cbWindowsSystemComponentVerification.Checked;
   case cbPageHashing.State of
      cbChecked: o.MicrosoftSignToolOptions.Flags.UsePageHashing := csphYes;
      cbUnchecked: o.MicrosoftSignToolOptions.Flags.UsePageHashing := csphNo;
      else
         o.MicrosoftSignToolOptions.Flags.UsePageHashing := csphDefault;
   end;
   o.MicrosoftSignToolOptions.Certificate.Filename := UTF8Decode(editFilenamePFX.Text);
end;

class function TFrameCodeSigningOptionsMicrosoftSignTool.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningOptions;
end;

initialization
   RegisterIDEOptionsEditor(CodeSigningOptionGroup, TFrameCodeSigningOptionsMicrosoftSignTool, 1, NoParent, True);
end.
