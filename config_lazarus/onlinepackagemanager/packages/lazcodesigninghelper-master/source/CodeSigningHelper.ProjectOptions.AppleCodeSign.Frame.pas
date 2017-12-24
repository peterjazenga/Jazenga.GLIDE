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

unit CodeSigningHelper.ProjectOptions.AppleCodeSign.Frame;

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

   { TFrameCodeSigningProjectOptionsAppleCodeSign }

   TFrameCodeSigningProjectOptionsAppleCodeSign = class(TAbstractIDEOptionsEditor)
      cbCertificateOverride: TCheckBox;
      editCertificateHash: TEditButton;
      editCertificateSubjectSubstring: TEdit;
      groupKey: TGroupBox;
      rbSignByCertificateHash: TRadioButton;
      rbSignByCertificateSubstring: TRadioButton;
      procedure cbCertificateOverrideChange({%H-}Sender: TObject);
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
   CodeSigningHelper.Debug,
   CodeSigningHelper.Strings;

{ TFrameCodeSigningProjectOptionsAppleCodeSign }

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.cbCertificateOverrideChange(Sender: TObject);
begin
   groupKey.Enabled := cbCertificateOverride.Checked;
end;

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.Save;
begin
   if not FLoading then begin
   WriteSettings(SupportedOptionsClass.GetInstance);
   TCodeSigningProjectOptions(SupportedOptionsClass.GetInstance).Write;
   end;
end;

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.ControlChange(Sender: TObject);
begin
   Save;
end;

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.ApplyLocalizedTexts;
begin
   cbCertificateOverride.Caption := 'Use a project-specific signing key:';
   groupKey.Caption := rsCodeSigningOptionsSigningKey;
   rbSignByCertificateHash.Caption := rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash;
   rbSignByCertificateSubstring.Caption := rsCodeSigningOptionsSpecifiyKeyByPartOfSubject;
end;

function TFrameCodeSigningProjectOptionsAppleCodeSign.GetTitle: string;
begin
   Result := rsCodeSigningOptionsTitleAppleCodeSign;
end;

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   CodeSigningLogInformation(Self, 'Setup');
   ApplyLocalizedTexts();
   ReadSettings(TCodeSigningProjectOptions.GetInstance);
end;

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.ReadSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningProjectOptions;
begin
   FLoading := True;
   try
      CodeSigningLogInformation(Self, 'ReadSettings');
      o := TCodeSigningProjectOptions(AOptions);
      cbCertificateOverride.Checked := o.AppleCodeSignCustom.UseSpecificCertificate;
      cbCertificateOverrideChange(nil);
      case o.AppleCodeSignOptions.Certificate.Source of
         cscsStoreByHash: rbSignByCertificateHash.Checked := True;
         cscsStoreBySubstring: rbSignByCertificateSubstring.Checked := True;
         else
            rbSignByCertificateSubstring.Checked := True;
      end;
      rbSignByCertificateHash.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.AppleCodeSignOptions.Certificate.Source = cscsStoreByHash, True)]);
      rbSignByCertificateSubstring.Hint := Format(rsCodeSigningOptionsBooleanHint, [BoolToStr(CodeSigningOptions.AppleCodeSignOptions.Certificate.Source = cscsStoreBySubstring, True)]);
      editCertificateHash.Text := o.AppleCodeSignOptions.Certificate.Hash;
      editCertificateHash.Hint := CodeSigningOptions.AppleCodeSignOptions.Certificate.Hash;
      editCertificateSubjectSubstring.Text := o.AppleCodeSignOptions.Certificate.SubString;
      editCertificateSubjectSubstring.Hint := CodeSigningOptions.AppleCodeSignOptions.Certificate.SubString;
   finally
      FLoading := False;
   end;
end;

procedure TFrameCodeSigningProjectOptionsAppleCodeSign.WriteSettings(AOptions: TAbstractIDEOptions);
var
   o: TCodeSigningProjectOptions;
begin
   CodeSigningLogInformation(Self, 'WriteSettings');
   o := TCodeSigningProjectOptions(AOptions);
   o.AppleCodeSignCustom.UseSpecificCertificate := cbCertificateOverride.Checked;
   if rbSignByCertificateHash.Checked then begin
      o.AppleCodeSignOptions.Certificate.Source := cscsStoreByHash;
   end else begin
      o.AppleCodeSignOptions.Certificate.Source := cscsStoreBySubstring;
   end;
   o.AppleCodeSignOptions.Certificate.Hash := editCertificateHash.Text;
   o.AppleCodeSignOptions.Certificate.SubString := editCertificateSubjectSubstring.Text;
   o.Write;
end;

class function TFrameCodeSigningProjectOptionsAppleCodeSign.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningProjectOptions;
end;

initialization

   RegisterIDEOptionsEditor(GroupProject, TFrameCodeSigningProjectOptionsAppleCodeSign, 801, NoParent, True);

end.
