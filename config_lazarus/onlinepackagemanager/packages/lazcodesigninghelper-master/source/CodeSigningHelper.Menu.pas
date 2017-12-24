{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Helper making codesigning available in the Lazarus IDE.)

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
// 2017-05-17  pk  20m  Updated to new PepiMK.Signing.* units.
// 2017-05-17  pk  10m  Restructured into submenus.
// 2017-05-11  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Menu;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   MenuIntf,
   MacroIntf,
   ProjectIntf,
   LazIDEIntf,
   IDEMsgIntf,
   IDEExternToolIntf,
   LazarusPackageIntf,
   IDEOptionsIntf,
   Forms,
   Controls,
   Dialogs,
   CodeSigningHelper.Options,
   CodeSigningHelper.ProjectOptions,
   CodeSigningHelper.Strings,
   PepiMK.Signing.Base,
   PepiMK.Signing.MicrosoftSignTool,
   PepiMK.Signing.AppleCodeSign,
   PepiMK.Signing.JavaKeyTool,
   PepiMK.Signing.GnuPG;

type

   { TCodeSigningHelper }

   TCodeSigningHelper = class
   private
      FOtherTargetDialog: TOpenDialog;
      FMenuSectionCodeSigning: TIDEMenuSection;
      FMenuItemCodeSign: TIDEMenuCommand;
      FMenuItemCodeSignOther: TIDEMenuCommand;
      FMenuItemCodeSignOtherGlobal: TIDEMenuCommand;
      FMenuItemCodeVerify: TIDEMenuCommand;
      FMenuItemCodeVerifyOther: TIDEMenuCommand;
      FMenuSectionGnuPGSigning: TIDEMenuSection;
      FMenuItemGPGSign: TIDEMenuCommand;
      FMenuItemGPGSignOther: TIDEMenuCommand;
      FMenuItemGPGSignOtherGlobal: TIDEMenuCommand;
      FMenuItemGPGVerify: TIDEMenuCommand;
      FMenuItemGPGVerifyOther: TIDEMenuCommand;
      procedure ProcessOutputToMessage(AResult: boolean; AOutcome: TCustomFileSignerResult; AView: string);
      procedure DoProjectBuildingFinished({%H-}ASender: TObject; ABuildSuccessful: boolean);
      procedure DoLazarusBuildingFinished({%H-}ASender: TObject; ABuildSuccessful: boolean);
      function DoProjectOpened({%H-}ASender: TObject; AProject: TLazProject): TModalResult;
      function DoProjectClose({%H-}ASender: TObject; AProject: TLazProject): TModalResult;
      function ExtractDescriptionURL(out AURL: string): boolean;
   private
      procedure DoCertificateSign({%H-}Sender: TObject);
      procedure DoCertificateSignOther({%H-}Sender: TObject);
      procedure DoCertificateSignOtherGlobal({%H-}Sender: TObject);
      procedure DoCertificateVerify({%H-}Sender: TObject);
      procedure DoCertificateVerifyOther({%H-}Sender: TObject);
      procedure DoGnuPGSign({%H-}Sender: TObject);
      procedure DoGnuPGSignOther({%H-}Sender: TObject);
      procedure DoGnuPGSignOtherGlobal({%H-}Sender: TObject);
      procedure DoGnuPGVerify({%H-}Sender: TObject);
      procedure DoGnuPGVerifyOther({%H-}Sender: TObject);
   protected
      procedure AssignOptionsToAppleCodeSign(var AFilename: string; ASigner: TAppleCodeSignSigner; AAllowProjectSpecificOptions: boolean = True; {%H-}ACustomDescription: string = '');
      procedure AssignOptionsToMicrosoftSignTool(var {%H-}AFilename: string; ASigner: TMicrosoftSignToolSigner; AAllowProjectSpecificOptions: boolean = True; {%H-}ACustomDescription: string = '');
      procedure AssignOptionsToJavaKeyTool(var {%H-}AFilename: string; ASigner: TJavaKeyToolSigner; AAllowProjectSpecificOptions: boolean = True);
      procedure PrintMicrosoftSignToolOptions(ASigner: TMicrosoftSignToolSigner);
      procedure PrintCertificate(ACertificate: TCustomFileSignerCertificate; AView: string);
   public
      constructor Create;
      destructor Destroy; override;
      procedure CreateMainMenuSubMenu();
      procedure AddHandlers();
      procedure RemoveHandlers();
      function GetTargetOS: string;
   public
      function CertificateSignExecutable(AFilename: string; AAllowProjectSpecificOptions: boolean = True; {%H-}ACustomDescription: string = ''): boolean;
      function CertificateVerifyExecutable(AFilename: string; {%H-}AAllowProjectSpecificOptions: boolean = True): boolean;
      function GnuPGSignFile(AFilename: string; AAllowProjectSpecificOptions: boolean = True): boolean;
      function GnuPGVerifyFile(AFilename: string; {%H-}AAllowProjectSpecificOptions: boolean = True): boolean;
   end;

implementation

uses
   RegExpr,
   {$IFDEF Darwin}
   BaseUnix,
   Unix,
   {$ENDIF Darwin}
   FileUtil,
   FileCtrl,
   CodeSigningHelper.Debug;

{ TCodeSigningHelper }

procedure TCodeSigningHelper.ProcessOutputToMessage(AResult: boolean; AOutcome: TCustomFileSignerResult; AView: string);
var
   i: integer;
begin
   if AResult then begin
      for i := 0 to Pred(AOutcome.Output.Count) do begin
         if Length(AOutcome.Output[i]) > 0 then begin
            IDEMessagesWindow.AddCustomMessage(mluNote, AOutcome.Output[i], '', 0, 0, AView);
         end;
      end;
      for i := 0 to Pred(AOutcome.Errors.Count) do begin
         if Length(AOutcome.Errors[i]) > 0 then begin
            IDEMessagesWindow.AddCustomMessage(mluError, AOutcome.Errors[i], '', 0, 0, AView);
         end;
      end;
   end else begin
      for i := 0 to Pred(AOutcome.Output.Count) do begin
         if Length(AOutcome.Output[i]) > 0 then begin
            IDEMessagesWindow.AddCustomMessage(mluNote, AOutcome.Output[i], '', 0, 0, AView);
         end;
      end;
      for i := 0 to Pred(AOutcome.Errors.Count) do begin
         if Length(AOutcome.Errors[i]) > 0 then begin
            IDEMessagesWindow.AddCustomMessage(mluError, AOutcome.Errors[i], '', 0, 0, AView);
         end;
      end;
   end;
end;

procedure TCodeSigningHelper.DoProjectBuildingFinished(ASender: TObject; ABuildSuccessful: boolean);
var
   s: string;
   sTargetOS: string;
   b: boolean;
begin
   s := '$(TargetFile)';
   sTargetOS := GetTargetOS;
   if IDEMacros.SubstituteMacros(s) then begin
      if ABuildSuccessful then begin
         b := false;
         if TMicrosoftSignToolSigner.SupportsLazarusTargetOS(sTargetOS) then begin
            b := CodeSigningOptions.MicrosoftSignToolOptions.AutoSign;
         end else if TAppleCodeSignSigner.SupportsLazarusTargetOS(sTargetOS) then begin
            b := CodeSigningOptions.AppleCodeSignOptions.AutoSign;
         end else if TJavaKeyToolSigner.SupportsLazarusTargetOS(sTargetOS) then begin
            b := CodeSigningOptions.JavaKeyToolOptions.AutoSign;
         end;
         if b then begin
            IDEMessagesWindow.AddCustomMessage(mluNone, rsCodeSigningStatusSigning, '', 0, 0, rsCodeSigningViewCodeSign);
            CertificateSignExecutable(s);
         end;
         if CodeSigningOptions.GnuPGOptions.AutoSign then begin
            IDEMessagesWindow.AddCustomMessage(mluNone, rsGnuPGSigningStatusSigning, '', 0, 0, rsGnuPGSigningViewSign);
            GnuPGSignFile(s);
         end;
      end else begin
         IDEMessagesWindow.AddCustomMessage(mluError, rsCodeSigningStatusBuildFailed);
      end;
   end else begin
      IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningErrorIDEMacrosSubstituteMacrosFailed, [s]));
   end;
end;

procedure TCodeSigningHelper.DoLazarusBuildingFinished(ASender: TObject; ABuildSuccessful: boolean);
begin
   if ABuildSuccessful then begin
      CertificateSignExecutable(ParamStr(0), False, Format('Lazarus IDE, compiled %s', [FormatDateTime('yyyy-mm-dd, hh:nn', Now)]));
   end;
end;

function TCodeSigningHelper.DoProjectOpened(ASender: TObject; AProject: TLazProject): TModalResult;
begin
   Result := mrOk;
   CodeSigningLogInformation(AProject, 'DoProjectOpened: ' + AProject.ProjectInfoFile);
   try
      TCodeSigningProjectOptions(TCodeSigningProjectOptions.GetInstance).Read;
   except
      on E: Exception do begin
         ShowMessage('TCodeSigningHelper.DoProjectOpened'#13#10 + Format(rsCodeSigningErrorDoProjectOpened, [AProject.MainFile.Filename]));
      end;
   end;
end;

function TCodeSigningHelper.DoProjectClose(ASender: TObject; AProject: TLazProject): TModalResult;
begin
   Result := mrOk;
   try
      TCodeSigningProjectOptions(TCodeSigningProjectOptions.GetInstance).Write;
   except
      on E: Exception do begin
         ShowMessage(Format(rsCodeSigningErrorDoProjectClose, [AProject.MainFile.Filename]));
      end;
   end;
end;

function TCodeSigningHelper.CertificateSignExecutable(AFilename: string; AAllowProjectSpecificOptions: boolean; ACustomDescription: string): boolean;
var
   fs: TCustomFileSigner;
   sFilename: string;
   sTargetOS: string;
begin
   sFilename := aFilename;
   sTargetOS := GetTargetOS;
   if TMicrosoftSignToolSigner.SupportsLazarusTargetOS(sTargetOS) then begin
      fs := TMicrosoftSignToolSigner.Create;
      AssignOptionsToMicrosoftSignTool(sFilename, TMicrosoftSignToolSigner(fs), AAllowProjectSpecificOptions, ACustomDescription);
      if not FileExists(CodeSigningOptions.MicrosoftSignToolOptions.SignToolExecutable) then begin
         IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningErrorMissingSigningExecutable, [CodeSigningOptions.MicrosoftSignToolOptions.SignToolExecutable]), '', 0, 0, rsCodeSigningViewCodeSign);
         Exit;
      end;
      PrintMicrosoftSignToolOptions(TMicrosoftSignToolSigner(fs));
   end else if TAppleCodeSignSigner.SupportsLazarusTargetOS(sTargetOS) then begin
      fs := TAppleCodeSignSigner.Create;
      AssignOptionsToAppleCodeSign(sFilename, TAppleCodeSignSigner(fs), AAllowProjectSpecificOptions, ACustomDescription);
   end else if TJavaKeyToolSigner.SupportsLazarusTargetOS(sTargetOS) then begin
      fs := TJavaKeyToolSigner.Create;
      AssignOptionsToJavaKeyTool(sFilename, TJavaKeyToolSigner(fs), AAllowProjectSpecificOptions);
   end;
   try
      Result := fs.SignFile(UTF8Decode(sFilename));
      IDEMessagesWindow.AddCustomMessage(mluVerbose, fs.Outcome.CommandLine, '', 0, 0, rsCodeSigningViewCodeSign);
      IDEMessagesWindow.AddCustomMessage(mluHint, Format(rsCodeSigningStatusErrorCode, [fs.Outcome.ExitCode]), '', 0, 0, rsCodeSigningViewCodeSign);
      if Result then begin
         IDEMessagesWindow.AddCustomMessage(mluImportant, Format(rsCodeSigningMessageSuccess, [sFilename]), '', 0, 0, rsCodeSigningViewCodeSign);
      end else begin
         IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningMessageError, [sFilename]), '', 0, 0, rsCodeSigningViewCodeSign);
      end;
      ProcessOutputToMessage(Result, fs.Outcome, rsCodeSigningViewCodeSign);
   finally
      fs.Free;
   end;
end;

function TCodeSigningHelper.CertificateVerifyExecutable(AFilename: string; AAllowProjectSpecificOptions: boolean): boolean;
var
   fs: TCustomFileSigner;
   sFilename: string;
begin
   fs := nil;
   {$IFDEF MSWindows}
   fs := TMicrosoftSignToolSigner.Create;
   fs.SigningExecutable := CodeSigningOptions.MicrosoftSignToolOptions.SignToolExecutable;
   {$ENDIF MSWindows}
   {$IFDEF Darwin}
   fs := TAppleCodeSignSigner.Create;
   fs.SigningExecutable := CodeSigningOptions.AppleCodeSignOptions.CodeSignExecutable;
   {$ENDIF Darwin}
   if not Assigned(fs) then begin
      Exit;
   end;
   try
      sFilename := aFilename;
      {$IFDEF MSWindows}
      if not FileExists(fs.SigningExecutable) then begin
         IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningErrorMissingSigningExecutable, [fs.SigningExecutable]), '', 0, 0, rsCodeSigningViewCodeVerify);
         Exit;
      end;
      {$ENDIF MSWindows}
      Result := fs.VerifyFile(UTF8Decode(sFilename));
      IDEMessagesWindow.AddCustomMessage(mluVerbose, fs.Outcome.CommandLine, '', 0, 0, rsCodeSigningViewCodeVerify);
      IDEMessagesWindow.AddCustomMessage(mluHint, Format(rsCodeSigningStatusErrorCode, [fs.Outcome.ExitCode]), '', 0, 0, rsCodeSigningViewCodeVerify);
      ProcessOutputToMessage(Result, fs.Outcome, rsCodeSigningViewCodeVerify);
   finally
      fs.Free;
   end;
end;

function TCodeSigningHelper.GnuPGSignFile(AFilename: string; AAllowProjectSpecificOptions: boolean): boolean;
var
   fs: TGnuPGSigner;
   sFilename: string;
   po: TCodeSigningProjectOptions;
begin
   fs := TGnuPGSigner.Create;
   po := TCodeSigningProjectOptions(TCodeSigningProjectOptions.GetInstance);
   try
      sFilename := AFilename;
      {$IFDEF Darwin}
      if not FileExists(sFilename) then begin
         sFilename += '.app/Contents/MacOS/' + ExtractFilename(sFilename);
      end;
      {$ENDIF Darwin}
      fs.SigningExecutable := CodeSigningOptions.GnuPGOptions.GnuPGExecutable;
      fs.UseCustomKey := CodeSigningOptions.GnuPGOptions.UseCustomKey;
      if po.GnuPGCustom.UseSpecificCertificate and AAllowProjectSpecificOptions then begin
         fs.Certificate.Assign(po.GnuPGOptions.Certificate);
      end else begin
         fs.Certificate.Assign(CodeSigningOptions.GnuPGOptions.Certificate);
      end;
      Result := fs.SignFile(UTF8Decode(sFilename));
      IDEMessagesWindow.AddCustomMessage(mluVerbose, fs.Outcome.CommandLine, '', 0, 0, rsGnuPGSigningViewSign);
      IDEMessagesWindow.AddCustomMessage(mluHint, Format(rsCodeSigningStatusErrorCode, [fs.Outcome.ExitCode]), '', 0, 0, rsGnuPGSigningViewSign);
      ProcessOutputToMessage(Result, fs.Outcome, rsGnuPGSigningViewSign);
   finally
      fs.Free;
   end;
end;

function TCodeSigningHelper.GnuPGVerifyFile(AFilename: string; AAllowProjectSpecificOptions: boolean): boolean;
var
   fs: TGnuPGSigner;
   sFilename: string;
begin
   fs := TGnuPGSigner.Create;
   try
      sFilename := AFilename;
      {$IFDEF Darwin}
      if not FileExists(sFilename) then begin
         sFilename += '.app/Contents/MacOS/' + ExtractFilename(sFilename);
      end;
      {$ENDIF Darwin}
      fs.SigningExecutable := CodeSigningOptions.GnuPGOptions.GnuPGExecutable;
      Result := fs.VerifyFile(UTF8Decode(sFilename));
      IDEMessagesWindow.AddCustomMessage(mluVerbose, fs.Outcome.CommandLine, '', 0, 0, rsGnuPGSigningViewVerify);
      IDEMessagesWindow.AddCustomMessage(mluHint, Format(rsCodeSigningStatusErrorCode, [fs.Outcome.ExitCode]), '', 0, 0, rsGnuPGSigningViewVerify);
      ProcessOutputToMessage(Result, fs.Outcome, rsGnuPGSigningViewVerify);
   finally
      fs.Free;
   end;
end;

function TCodeSigningHelper.ExtractDescriptionURL(out AURL: string): boolean;
var
   r: TRegExpr;
   sSource: string;
begin
   AURL := '';
   //    @codesigning-url(http://http://ccrdude.net/LazCodeSigningHelper/)
   sSource := LazarusIDE.ActiveProject.MainFile.GetSourceText;
   r := TRegExpr.Create('@codesigning-url\(([^\)]*)\)');
   try
      Result := r.Exec(sSource);
      if Result then begin
         AURL := r.Match[1];
      end;
   finally
      r.Free;
   end;
end;

destructor TCodeSigningHelper.Destroy;
begin
   RemoveHandlers;
   FOtherTargetDialog.Free;
end;

procedure TCodeSigningHelper.CreateMainMenuSubMenu;
begin
   // Codesigning
   FMenuSectionCodeSigning := RegisterIDESubMenu(mnuProject, 'CodesigningMenu', rsCodeSigningMenuName);
   {$IFDEF Darwin}
   FMenuItemCodeSign := RegisterIDEMenuCommand(FMenuSectionCodeSigning, 'CodeSigningSignFile', rsCodeSigningMenuItemSignBundle, @DoCertificateSign);
   {$ELSE Darwin}
   FMenuItemCodeSign := RegisterIDEMenuCommand(FMenuSectionCodeSigning, 'CodeSigningSignFile', rsCodeSigningMenuItemSign, @DoCertificateSign);
   {$ENDIF Darwin}
   FMenuItemCodeSignOther := RegisterIDEMenuCommand(FMenuSectionCodeSigning, 'CodeSigningSignFileOther', rsCodeSigningMenuItemSignOther, @DoCertificateSignOther);
   FMenuItemCodeSignOtherGlobal := RegisterIDEMenuCommand(FMenuSectionCodeSigning, 'CodeSigningSignFileOtherGlobal', rsCodeSigningMenuItemSignOtherGlobal, @DoCertificateSignOtherGlobal);
   FMenuItemCodeVerify := RegisterIDEMenuCommand(FMenuSectionCodeSigning, 'CodeSigningVerifyFile', rsCodeSigningMenuItemVerify, @DoCertificateVerify);
   FMenuItemCodeVerifyOther := RegisterIDEMenuCommand(FMenuSectionCodeSigning, 'CodeSigningVerifyFileOther', rsCodeSigningMenuItemVerifyOther, @DoCertificateVerifyOther);
   // GnuPG signing
   FMenuSectionGnuPGSigning := RegisterIDESubMenu(mnuProject, 'CodeSigningGnuPGSubMenu', rsGnuPGSigningMenuName);
   FMenuItemGPGSign := RegisterIDEMenuCommand(FMenuSectionGnuPGSigning, 'GPGSigningSignFile', rsGnuPGSigningMenuItemSign, @DoGnuPGSign);
   FMenuItemGPGSignOther := RegisterIDEMenuCommand(FMenuSectionGnuPGSigning, 'GPGSigningSignFileOther', rsGnuPGSigningMenuItemSignOther, @DoGnuPGSignOther);
   FMenuItemGPGSignOtherGlobal := RegisterIDEMenuCommand(FMenuSectionGnuPGSigning, 'GPGSigningSignFileOtherGlobal', rsGnuPGSigningMenuItemSignOtherGlobal, @DoGnuPGSignOtherGlobal);
   FMenuItemGPGVerify := RegisterIDEMenuCommand(FMenuSectionGnuPGSigning, 'GPGSigningVerifyFile', rsGnuPGSigningMenuItemVerify, @DoGnuPGSign);
   FMenuItemGPGVerifyOther := RegisterIDEMenuCommand(FMenuSectionGnuPGSigning, 'GPGSigningVerifyFileOther', rsGnuPGSigningMenuItemVerifyOther, @DoGnuPGSignOther);
end;

procedure TCodeSigningHelper.AddHandlers;
begin
   LazarusIDE.AddHandlerOnProjectBuildingFinished(@DoProjectBuildingFinished, True);
   LazarusIDE.AddHandlerOnLazarusBuildingFinished(@DoLazarusBuildingFinished, True);
   LazarusIDE.AddHandlerOnProjectOpened(@DoProjectOpened, True);
   LazarusIDE.AddHandlerOnProjectClose(@DoProjectClose, True);
end;

procedure TCodeSigningHelper.RemoveHandlers;
begin
   LazarusIDE.RemoveHandlerOnProjectBuildingFinished(@DoProjectBuildingFinished);
   LazarusIDE.RemoveHandlerOnLazarusBuildingFinished(@DoLazarusBuildingFinished);
   LazarusIDE.RemoveHandlerOnProjectOpened(@DoProjectOpened);
   LazarusIDE.RemoveHandlerOnProjectClose(@DoProjectClose);
end;

function TCodeSigningHelper.GetTargetOS: string;
var
   iBuildMode: integer;
   bm: TLazProjectBuildMode;
begin
   try
      iBuildMode := LazarusIDE.ActiveProject.LazBuildModes.IndexOf(LazarusIDE.ActiveProject.ActiveBuildModeID);
      bm := LazarusIDE.ActiveProject.LazBuildModes.BuildModes[iBuildMode];
      Result := bm.LazCompilerOptions.GetEffectiveTargetOS;
   except
      Result := '';
      {$IFDEF MSWindows}
      IDEMessagesWindow.AddCustomMessage(mluWarning, 'Unable to determine Target OS, falling back to Win32');
      Result := 'Win32';
      {$ENDIF MSWindows}
      {$IFDEF Darwin}
      IDEMessagesWindow.AddCustomMessage(mluWarning, 'Unable to determine Target OS, falling back to Darwin');
      Result := 'Darwin';
      {$ENDIF Darwin}
      {$IFDEF cpujvm}
      IDEMessagesWindow.AddCustomMessage(mluWarning, 'Unable to determine Target OS, falling back to Java');
      Result := 'Java';
      {$ENDIF cpujvm}
   end;
end;

procedure TCodeSigningHelper.DoCertificateSign(Sender: TObject);
var
   s: string;
begin
   s := '$(TargetFile)';
   if IDEMacros.SubstituteMacros(s) then begin
      IDEMessagesWindow.Clear;
      CertificateSignExecutable(s);
   end else begin
      ShowMessage(Format(rsCodeSigningErrorIDEMacrosSubstituteMacrosFailed, [s]));
   end;
end;

procedure TCodeSigningHelper.DoCertificateSignOther(Sender: TObject);
begin
   if FOtherTargetDialog.Execute then begin
      CertificateSignExecutable(FOtherTargetDialog.FileName, True);
   end;
end;

procedure TCodeSigningHelper.DoCertificateSignOtherGlobal(Sender: TObject);
begin
   if FOtherTargetDialog.Execute then begin
      CertificateSignExecutable(FOtherTargetDialog.FileName, False);
   end;
end;

procedure TCodeSigningHelper.DoGnuPGSign(Sender: TObject);
var
   s: string;
begin
   s := '$(TargetFile)';
   if IDEMacros.SubstituteMacros(s) then begin
      IDEMessagesWindow.Clear;
      if FileExists(s + '.sig') then begin
         DeleteFile(s + '.sig');
      end;
      GnuPGSignFile(s);
   end else begin
      ShowMessage(Format(rsCodeSigningErrorIDEMacrosSubstituteMacrosFailed, [s]));
   end;
end;

procedure TCodeSigningHelper.DoGnuPGSignOther(Sender: TObject);
begin
   if FOtherTargetDialog.Execute then begin
      GnuPGSignFile(FOtherTargetDialog.FileName, True);
   end;
end;

procedure TCodeSigningHelper.DoGnuPGSignOtherGlobal(Sender: TObject);
begin
   if FOtherTargetDialog.Execute then begin
      GnuPGSignFile(FOtherTargetDialog.FileName, False);
   end;
end;

procedure TCodeSigningHelper.DoCertificateVerify(Sender: TObject);
var
   s: string;
begin
   s := '$(TargetFile)';
   if IDEMacros.SubstituteMacros(s) then begin
      IDEMessagesWindow.Clear;
      CertificateVerifyExecutable(s);
   end else begin
      ShowMessage(Format(rsCodeSigningErrorIDEMacrosSubstituteMacrosFailed, [s]));
   end;
end;

procedure TCodeSigningHelper.DoCertificateVerifyOther(Sender: TObject);
begin
   if FOtherTargetDialog.Execute then begin
      CertificateVerifyExecutable(FOtherTargetDialog.FileName, True);
   end;
end;

procedure TCodeSigningHelper.DoGnuPGVerify(Sender: TObject);
var
   s: string;
begin
   s := '$(TargetFile)';
   if IDEMacros.SubstituteMacros(s) then begin
      IDEMessagesWindow.Clear;
      GnuPGVerifyFile(s);
   end else begin
      ShowMessage(Format(rsCodeSigningErrorIDEMacrosSubstituteMacrosFailed, [s]));
   end;
end;

procedure TCodeSigningHelper.DoGnuPGVerifyOther(Sender: TObject);
begin
   if FOtherTargetDialog.Execute then begin
      GnuPGVerifyFile(FOtherTargetDialog.FileName, True);
   end;
end;

procedure TCodeSigningHelper.AssignOptionsToMicrosoftSignTool(var AFilename: string; ASigner: TMicrosoftSignToolSigner; AAllowProjectSpecificOptions: boolean; ACustomDescription: string);
var
   sURL: string;
   po: TCodeSigningProjectOptions;
begin
   if AAllowProjectSpecificOptions then begin
      po := TCodeSigningProjectOptions(TCodeSigningProjectOptions.GetInstance);
      po.Read;
   end else begin
      po := nil;
   end;
   if AAllowProjectSpecificOptions and po.MicrosoftSignToolCustom.UseSpecificCertificate then begin
      IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessageProjectSpecificCertificate, '', 0, 0, rsCodeSigningViewCodeSign);
      ASigner.Certificate.Assign(po.MicrosoftSignToolOptions.Certificate);
   end else begin
      ASigner.Certificate.Assign(CodeSigningOptions.MicrosoftSignToolOptions.Certificate);
   end;
   PrintCertificate(ASigner.Certificate, rsCodeSigningViewCodeSign);
   if AAllowProjectSpecificOptions and po.MicrosoftSignToolCustom.UseSpecificTimestamping then begin
      IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessageProjectSpecificTimestamping, '', 0, 0, rsCodeSigningViewCodeSign);
      TMicrosoftSignToolSigner(ASigner).Timestamping.Assign(po.MicrosoftSignToolOptions.Timestamping);
   end else begin
      TMicrosoftSignToolSigner(ASigner).Timestamping.Assign(CodeSigningOptions.MicrosoftSignToolOptions.Timestamping);
   end;
   if AAllowProjectSpecificOptions and po.MicrosoftSignToolCustom.UseSpecificFlags then begin
      IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessageProjectSpecificFlags, '', 0, 0, rsCodeSigningViewCodeSign);
      TMicrosoftSignToolSigner(ASigner).Flags.Assign(po.MicrosoftSignToolOptions.Flags);
   end else begin
      TMicrosoftSignToolSigner(ASigner).Flags.Assign(CodeSigningOptions.MicrosoftSignToolOptions.Flags);
   end;
   if AAllowProjectSpecificOptions and po.MicrosoftSignToolCustom.UseSpecificCertificate then begin
      TMicrosoftSignToolSigner(ASigner).CrossSigning.Assign(po.MicrosoftSignToolOptions.CrossSigning);
   end else begin
      TMicrosoftSignToolSigner(ASigner).CrossSigning.Assign(CodeSigningOptions.MicrosoftSignToolOptions.CrossSigning);
   end;
   if (Length(ACustomDescription) > 0) then begin
      TMicrosoftSignToolSigner(ASigner).DescriptionSubject := UTF8Decode(ACustomDescription);
   end else if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then begin
      TMicrosoftSignToolSigner(ASigner).DescriptionSubject := UTF8Decode(LazarusIDE.ActiveProject.GetTitleOrName);
   end else begin
      TMicrosoftSignToolSigner(ASigner).DescriptionSubject := '';
   end;
   ExtractDescriptionURL(sURL);
   TMicrosoftSignToolSigner(ASigner).DescriptionURL := sURL;
   TMicrosoftSignToolSigner(ASigner).Algorithms := [];
   if CodeSigningOptions.MicrosoftSignToolOptions.SignWithSHA1 then begin
      TMicrosoftSignToolSigner(ASigner).Algorithms := TMicrosoftSignToolSigner(ASigner).Algorithms + [csaSHA1];
   end;
   if CodeSigningOptions.MicrosoftSignToolOptions.SignWithSHA256 then begin
      TMicrosoftSignToolSigner(ASigner).Algorithms := TMicrosoftSignToolSigner(ASigner).Algorithms + [csaSHA256];
   end;
   ASigner.SigningExecutable := CodeSigningOptions.MicrosoftSignToolOptions.SignToolExecutable;
end;

procedure TCodeSigningHelper.AssignOptionsToJavaKeyTool(var AFilename: string; ASigner: TJavaKeyToolSigner; AAllowProjectSpecificOptions: boolean);
begin
   //PrintCertificate(ASigner.Certificate);
end;

procedure TCodeSigningHelper.PrintMicrosoftSignToolOptions(ASigner: TMicrosoftSignToolSigner);
begin
   case ASigner.Flags.UsePageHashing of
      csphDefault:
      begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessagePageHashingDefault, '', 0, 0, rsCodeSigningViewCodeSign);
      end;
      csphNo:
      begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessagePageHashingNo, '', 0, 0, rsCodeSigningViewCodeSign);
      end;
      csphYes:
      begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessagePageHashingYes, '', 0, 0, rsCodeSigningViewCodeSign);
      end;
   end;
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageWindowsSystemComponentVerification,
      [BoolToStr(ASigner.Flags.UseWindowsSystemComponentVerification, True)]), '', 0, 0, rsCodeSigningViewCodeSign);
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageTimestampingActive, [BoolToStr(ASigner.Timestamping.Active, True)]),
      '', 0, 0, rsCodeSigningViewCodeSign);
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageTimestampingServer, [ASigner.Timestamping.ServerRFC3161]), '', 0, 0, rsCodeSigningViewCodeSign);
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageCrossSigningActive, [BoolToStr(ASigner.CrossSigning.Active, True)]),
      '', 0, 0, rsCodeSigningViewCodeSign);
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageCrossSigningFile, [UTF8Encode(ASigner.CrossSigning.Filename)]), '',
      0, 0, rsCodeSigningViewCodeSign);
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageDescriptionText, [UTF8Encode(ASigner.DescriptionSubject)]), '', 0, 0, rsCodeSigningViewCodeSign);
   IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageDescriptionURL, [ASigner.DescriptionURL]), '', 0, 0, rsCodeSigningViewCodeSign);
end;

procedure TCodeSigningHelper.PrintCertificate(ACertificate: TCustomFileSignerCertificate; AView: string);
begin
   case ACertificate.Source of
      cscsStoreByHash:
      begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageCertificateSourceHash, [ACertificate.Hash]), '', 0, 0, AView);
      end;
      cscsFileAsPFX:
      begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageCertificateSourceSubstring, [ACertificate.Substring]), '', 0, 0, AView);
      end;
      cscsStoreBySubstring:
      begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningMessageCertificateSourceFile, [UTF8Encode(ACertificate.Filename)]), '', 0, 0, AView);
      end;
   end;
end;

procedure TCodeSigningHelper.AssignOptionsToAppleCodeSign(var AFilename: string; ASigner: TAppleCodeSignSigner; AAllowProjectSpecificOptions: boolean; ACustomDescription: string);
var
   sFilenameBundle: string;
   sFilenameExeInBundle: string;
   sDirnameSignature: string;
   sFilenameSignature: string;
   po: TCodeSigningProjectOptions;
begin
   po := TCodeSigningProjectOptions(TCodeSigningProjectOptions.GetInstance);
   sFilenameBundle := AFilename + '.app';
   sDirnameSignature := sFilenameBundle + '/Contents/_CodeSignature';
   sFilenameSignature := SDirnameSignature + '/CodeResources';
   if FileExists(sFilenameSignature) then begin
      if DeleteFile(sFilenameSignature) then begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningStatusSignatureRemovedGood, [sFilenameSignature]), sFilenameSignature, 0, 0, rsCodeSigningViewCodeSign);
      end else begin
         IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningStatusSignatureRemovedFail, [sFilenameSignature]), sFilenameSignature, 0, 0, rsCodeSigningViewCodeSign);
      end;
   end;
   if DirectoryExists(sDirnameSignature) then begin
      if RemoveDir(sDirnameSignature) then begin
         IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningStatusSignatureFolderRemovedGood, [sFilenameSignature]), sFilenameSignature, 0, 0, rsCodeSigningViewCodeSign);
      end else begin
         IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningStatusSignatureFolderRemovedFail, [sFilenameSignature]), sFilenameSignature, 0, 0, rsCodeSigningViewCodeSign);
      end;
   end;
   if FileExists(AFilename) and DirectoryExists(sFilenameBundle) then begin
      sFilenameExeInBundle := sFilenameBundle + '/Contents/MacOS/' + ExtractFileName(AFilename);
      if FileExists(sFilenameExeInBundle) then begin
         if not DeleteFile(sFilenameExeInBundle) then begin
            IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningStatusSymlinkRemovedFail, [sFilenameExeInBundle]), sFilenameExeInBundle, 0, 0, rsCodeSigningViewCodeSign);
            Exit;
         end;
      end;
      if CopyFile(AFilename, sFilenameExeInBundle) then begin
         {$IFDEF Darwin}
         FpChmod(sFilenameExeInBundle, &755);
         {$ENDIF Darwin}
         IDEMessagesWindow.AddCustomMessage(mluVerbose, Format(rsCodeSigningStatusExecutableToBundleCopiedGood, [sFilenameExeInBundle]), sFilenameExeInBundle, 0, 0, rsCodeSigningViewCodeSign);
         if not DeleteFile(AFilename) then begin
            IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningStatusDeleteUnbundledExecutableFail, [AFilename]), AFilename, 0, 0, rsCodeSigningViewCodeSign);
         end;
      end else begin
         IDEMessagesWindow.AddCustomMessage(mluError, Format(rsCodeSigningStatusExecutableToBundleCopiedFail, [sFilenameExeInBundle]), sFilenameExeInBundle, 0, 0, rsCodeSigningViewCodeSign);
         Exit;
      end;
      AFilename := sFilenameBundle;
   end else if DirectoryExists(sFilenameBundle) then begin
      AFilename := sFilenameBundle;
   end else begin
      AFilename := AFilename;
   end;
   if po.AppleCodeSignCustom.UseSpecificCertificate and AAllowProjectSpecificOptions then begin
      IDEMessagesWindow.AddCustomMessage(mluVerbose, rsCodeSigningMessageProjectSpecificCertificate, '', 0, 0, rsCodeSigningViewCodeSign);
      ASigner.Certificate.Assign(po.AppleCodeSignOptions.Certificate);
   end else begin
      ASigner.Certificate.Assign(CodeSigningOptions.AppleCodeSignOptions.Certificate);
   end;
   PrintCertificate(ASigner.Certificate, rsCodeSigningViewCodeSign);
   ASigner.SigningExecutable := CodeSigningOptions.MicrosoftSignToolOptions.SignToolExecutable;
end;

constructor TCodeSigningHelper.Create;
begin
   FOtherTargetDialog := TOpenDialog.Create(nil);
end;

end.
