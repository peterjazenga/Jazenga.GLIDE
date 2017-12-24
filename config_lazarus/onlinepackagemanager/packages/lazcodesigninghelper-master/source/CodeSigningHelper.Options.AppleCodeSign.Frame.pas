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

unit CodeSigningHelper.Options.AppleCodeSign.Frame;

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
   IDEOptionsIntf;

type

   { TTFrameCodeSigningOptionsAppleCodeSign }

   TTFrameCodeSigningOptionsAppleCodeSign = class(TAbstractIDEOptionsEditor)
      cbAutoSign: TCheckBox;
      editCertificateHash: TEditButton;
      editCertificateSubjectSubstring: TEdit;
      editExecutable: TComboBox;
      groupAutomation: TGroupBox;
      groupExecutable: TGroupBox;
      groupKey: TGroupBox;
      rbSignByCertificateHash: TRadioButton;
      rbSignByCertificateSubstring: TRadioButton;
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
   CodeSigningHelper.Strings;

{ TTFrameCodeSigningOptionsAppleCodeSign }

procedure TTFrameCodeSigningOptionsAppleCodeSign.ApplyLocalizedTexts;
begin
   groupAutomation.Caption := rsCodeSigningOptionsAutomation;
   cbAutoSign.Caption := rsCodeSigningOptionsSignOnSuccessfulCompileOrBuild;

   groupExecutable.Caption := rsCodeSigningOptionsSigningToolExecutable;

   groupKey.Caption := rsCodeSigningOptionsSigningKey;
   rbSignByCertificateHash.Caption :=
      rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash;
   rbSignByCertificateSubstring.Caption := rsCodeSigningOptionsSpecifiyKeyByPartOfSubject;
end;

function TTFrameCodeSigningOptionsAppleCodeSign.GetTitle: string;
begin
   Result := rsCodeSigningOptionsTitleAppleCodeSign;
end;

procedure TTFrameCodeSigningOptionsAppleCodeSign.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   ApplyLocalizedTexts();
end;

procedure TTFrameCodeSigningOptionsAppleCodeSign.ReadSettings(AOptions: TAbstractIDEOptions);
begin
   cbAutoSign.Checked := CodeSigningOptions.AppleCodeSignOptions.AutoSign;

   editExecutable.Text := UTF8Encode(CodeSigningOptions.AppleCodeSignOptions.CodeSignExecutable);

   case CodeSigningOptions.AppleCodeSignOptions.Certificate.Source of
      cscsStoreByHash: rbSignByCertificateHash.Checked := True;
      cscsStoreBySubstring: rbSignByCertificateSubstring.Checked := True;
      else
         rbSignByCertificateSubstring.Checked := True;
   end;
   editCertificateHash.Text := CodeSigningOptions.AppleCodeSignOptions.Certificate.Hash;
   editCertificateSubjectSubstring.Text := CodeSigningOptions.AppleCodeSignOptions.Certificate.SubString;
end;

procedure TTFrameCodeSigningOptionsAppleCodeSign.WriteSettings(AOptions: TAbstractIDEOptions);
begin
   CodeSigningOptions.AppleCodeSignOptions.AutoSign := cbAutoSign.Checked;

   CodeSigningOptions.AppleCodeSignOptions.CodeSignExecutable := UTF8Decode(editExecutable.Text);

   if rbSignByCertificateHash.Checked then begin
      CodeSigningOptions.AppleCodeSignOptions.Certificate.Source := cscsStoreByHash;
   end else begin
      CodeSigningOptions.AppleCodeSignOptions.Certificate.Source := cscsStoreBySubstring;
   end;
   CodeSigningOptions.AppleCodeSignOptions.Certificate.Hash := editCertificateHash.Text;
   CodeSigningOptions.AppleCodeSignOptions.Certificate.SubString := editCertificateSubjectSubstring.Text;
end;

class function TTFrameCodeSigningOptionsAppleCodeSign.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningOptions;
end;

initialization
   RegisterIDEOptionsEditor(CodeSigningOptionGroup, TTFrameCodeSigningOptionsAppleCodeSign, 2, NoParent, True);
end.
