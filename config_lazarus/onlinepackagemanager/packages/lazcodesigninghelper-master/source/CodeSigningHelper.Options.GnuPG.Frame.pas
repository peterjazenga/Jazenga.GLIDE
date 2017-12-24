{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Lazarus IDE user interface for GnuPG signing options.)

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
// 2017-05-17  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Options.GnuPG.Frame;

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
   IDEOptionsIntf;

type

   { TFrameCodeSigningOptionsGnuPG }

   TFrameCodeSigningOptionsGnuPG = class(TAbstractIDEOptionsEditor)
      cbAutoSign: TCheckBox;
      cbCertificateOverride: TCheckBox;
      editCertificateHash: TEditButton;
      editCertificateSubjectSubstring: TEdit;
      editExecutable: TComboBox;
      groupAutomation: TGroupBox;
      groupExecutable: TGroupBox;
      groupKey: TGroupBox;
      rbSignByCertificateHash: TRadioButton;
      rbSignByCertificateSubstring: TRadioButton;
      procedure cbCertificateOverrideChange({%H-}Sender: TObject);
      procedure editCertificateHashButtonClick({%H-}Sender: TObject);
   private
      { private declarations }
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
   PepiMK.Signing.Base,
   CodeSigningHelper.Options,
   CodeSigningHelper.Strings,
   CodeSigningHelper.Certificates.Form;

{ TFrameCodeSigningOptionsGnuPG }

procedure TFrameCodeSigningOptionsGnuPG.cbCertificateOverrideChange(Sender: TObject);
begin
   groupKey.Enabled := cbCertificateOverride.Checked;
end;

procedure TFrameCodeSigningOptionsGnuPG.editCertificateHashButtonClick(Sender: TObject);
var
   s: string;
begin
   s := editCertificateHash.Text;
   if SelectGnuPGCertificate(s) then begin
      editCertificateHash.Text := s;
      rbSignByCertificateHash.Checked := true;
   end;
end;

procedure TFrameCodeSigningOptionsGnuPG.ApplyLocalizedTexts;
begin
   groupAutomation.Caption := rsCodeSigningOptionsAutomation;
   cbAutoSign.Caption := rsCodeSigningOptionsSignOnSuccessfulCompileOrBuild;

   groupExecutable.Caption := rsCodeSigningOptionsSigningToolExecutable;

   cbCertificateOverride.Caption := rsGnuPGSigningOptionsUseNonDefaultSigningKey;
   groupKey.Caption := rsCodeSigningOptionsSigningKey;
   rbSignByCertificateHash.Caption := rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash;
   rbSignByCertificateSubstring.Caption := rsCodeSigningOptionsSpecifiyKeyByPartOfSubject;
end;

function TFrameCodeSigningOptionsGnuPG.GetTitle: string;
begin
   Result := 'GnuPG Signing';
end;

procedure TFrameCodeSigningOptionsGnuPG.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   ApplyLocalizedTexts();
end;

procedure TFrameCodeSigningOptionsGnuPG.ReadSettings(AOptions: TAbstractIDEOptions);
begin
   cbAutoSign.Checked := CodeSigningOptions.GnuPGOptions.AutoSign;

   editExecutable.Text := UTF8Encode(CodeSigningOptions.GnuPGOptions.GnuPGExecutable);

   cbCertificateOverride.Checked := CodeSigningOptions.GnuPGOptions.UseCustomKey;
   cbCertificateOverrideChange(nil);
   case CodeSigningOptions.GnuPGOptions.Certificate.Source of
      cscsStoreByHash: rbSignByCertificateHash.Checked := True;
      cscsStoreBySubstring: rbSignByCertificateSubstring.Checked := True;
   end;
   editCertificateHash.Text := CodeSigningOptions.GnuPGOptions.Certificate.Hash;
   editCertificateSubjectSubstring.Text := CodeSigningOptions.GnuPGOptions.Certificate.SubString;
end;

procedure TFrameCodeSigningOptionsGnuPG.WriteSettings(AOptions: TAbstractIDEOptions);
begin
   CodeSigningOptions.GnuPGOptions.AutoSign := cbAutoSign.Checked;

   CodeSigningOptions.GnuPGOptions.GnuPGExecutable := UTF8Decode(editExecutable.Text);

   CodeSigningOptions.GnuPGOptions.UseCustomKey := cbCertificateOverride.Checked;
   if rbSignByCertificateHash.Checked then begin
      CodeSigningOptions.GnuPGOptions.Certificate.Source := cscsStoreByHash;
   end else begin
      CodeSigningOptions.GnuPGOptions.Certificate.Source := cscsStoreBySubstring;
   end;
   CodeSigningOptions.GnuPGOptions.Certificate.Hash := editCertificateHash.Text;
   CodeSigningOptions.GnuPGOptions.Certificate.SubString := editCertificateSubjectSubstring.Text;
end;

class function TFrameCodeSigningOptionsGnuPG.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningOptions;
end;

initialization
   RegisterIDEOptionsEditor(CodeSigningOptionGroup, TFrameCodeSigningOptionsGnuPG, 4, NoParent, true);
end.
