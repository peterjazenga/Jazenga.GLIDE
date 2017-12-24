{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Lazarus IDE options for codesigning.)

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
// 2017-05-11  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Options;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   LazIDEIntf,
   IDEOptionsIntf,
   IniFiles,
   PepiMK.Signing.MicrosoftSignTool,
   PepiMK.Signing.Base;

type

   { TCodeSigningOptionsGnuPG }

   TCodeSigningOptionsGnuPG = class(TPersistent)
   private
      FAutoSign: boolean;
      FCertificate: TCustomFileSignerCertificate;
      FGPGExecutable: WideString;
      FUseCustomKey: boolean;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
      property GnuPGExecutable: WideString read FGPGExecutable write FGPGExecutable;
      property AutoSign: boolean read FAutoSign write FAutoSign;
      property UseCustomKey: boolean read FUseCustomKey write FUseCustomKey;
      property Certificate: TCustomFileSignerCertificate read FCertificate;
   end;

   { TCodeSigningOptionsMicrosoftSignTool }

   TCodeSigningOptionsMicrosoftSignTool = class(TPersistent)
   private
      FAutoSign: boolean;
      FCertificate: TCustomFileSignerCertificate;
      FCrossSigning: TMicrosoftSignToolCrossSigning;
      FFlags: TMicrosoftSignToolFlags;
      FTimestamping: TMicrosoftSignToolTimestamping;
      FSignToolExecutable: WideString;
      FSignWithSHA1: boolean;
      FSignWithSHA256: boolean;
      function GetCertificateSource: TCodeSignCertificateSource;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
      property Timestamping: TMicrosoftSignToolTimestamping read FTimestamping;
      property CrossSigning: TMicrosoftSignToolCrossSigning read FCrossSigning;
      property Flags: TMicrosoftSignToolFlags read FFlags;
      property SignWithSHA1: boolean read FSignWithSHA1 write FSignWithSHA1;
      property SignWithSHA256: boolean read FSignWithSHA256 write FSignWithSHA256;
      property AutoSign: boolean read FAutoSign write FAutoSign;
      property SignToolExecutable: WideString read FSignToolExecutable write FSignToolExecutable;
      property Certificate: TCustomFileSignerCertificate read FCertificate;
   end;

   { TCodeSigningOptionsAppleCodeSign }

   TCodeSigningOptionsAppleCodeSign = class(TPersistent)
   private
      FAutoSign: boolean;
      FCertificate: TCustomFileSignerCertificate;
      FCodeSignExecutable: WideString;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
      property AutoSign: boolean read FAutoSign write FAutoSign;
      property CodeSignExecutable: WideString read FCodeSignExecutable write FCodeSignExecutable;
      property Certificate: TCustomFileSignerCertificate read FCertificate;
   end;

   { TCodeSigningOptionsJavaKeyTool }

   TCodeSigningOptionsJavaKeyTool = class(TPersistent)
   private
      FAutoSign: boolean;
      FKeyToolExecutable: WideString;
   public
      procedure Assign(Source: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
      property AutoSign: boolean read FAutoSign write FAutoSign;
      property KeyToolExecutable: WideString read FKeyToolExecutable write FKeyToolExecutable;
   end;

   { TCodeSigningOptions }

   TCodeSigningOptions = class(TAbstractIDEEnvironmentOptions)
   private
      FAppleCodeSignOptions: TCodeSigningOptionsAppleCodeSign;
      FGnuPGOptions: TCodeSigningOptionsGnuPG;
      FJavaKeyToolOptions: TCodeSigningOptionsJavaKeyTool;
      FMicrosoftSignToolOptions: TCodeSigningOptionsMicrosoftSignTool;
      FConfigFilename: string;
   protected
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string); virtual;
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string); virtual;
      function GetConfigFilename: string; virtual;
   public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Read;
      procedure Write;
      procedure Assign(Source: TPersistent); override;
      procedure DoAfterWrite(Restore: boolean); override;
      class function GetGroupCaption: string; override;
      class function GetInstance: TAbstractIDEOptions; override;
      property MicrosoftSignToolOptions: TCodeSigningOptionsMicrosoftSignTool read FMicrosoftSignToolOptions;
      property AppleCodeSignOptions: TCodeSigningOptionsAppleCodeSign read FAppleCodeSignOptions;
      property JavaKeyToolOptions: TCodeSigningOptionsJavaKeyTool read FJavaKeyToolOptions;
      property GnuPGOptions: TCodeSigningOptionsGnuPG read FGnuPGOptions;
   end;

function CodeSigningOptions: TCodeSigningOptions;

var
   CodeSigningOptionGroup: integer;

implementation

uses
   Dialogs,
   IDEIntf,
   IDEMsgIntf,
   IDEExternToolIntf,
   CodeSigningHelper.Debug,
   CodeSigningHelper.Strings;

var
   GCodeSigningOptions: TCodeSigningOptions = nil;

function CodeSigningOptions: TCodeSigningOptions;
begin
   if not Assigned(GCodeSigningOptions) then begin
      //CodeSigningLogInformation(GCodeSigningOptions, 'function CodeSigningOptions: TCodeSigningOptions;#Create');
      GCodeSigningOptions := TCodeSigningOptions.Create;
   end;
   Result := GCodeSigningOptions;
end;

{ TCodeSigningOptionsJavaKeyTool }

procedure TCodeSigningOptionsJavaKeyTool.Assign(Source: TPersistent);
begin
   if Source is TCodeSigningOptionsJavaKeyTool then begin
      Self.FAutoSign := TCodeSigningOptionsJavaKeyTool(Source).FAutoSign;
      Self.FKeyToolExecutable := TCodeSigningOptionsJavaKeyTool(Source).FKeyToolExecutable;
   end else begin
      inherited Assign(Source);
   end;
end;

procedure TCodeSigningOptionsJavaKeyTool.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   FAutoSign := AIni.ReadBool(ASectionName, 'AutoSign', True);
   FKeyToolExecutable := UTF8Decode(AIni.ReadString(ASectionName, 'Executable', 'keytool'));
end;

procedure TCodeSigningOptionsJavaKeyTool.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteBool(ASectionName, 'AutoSign', FAutoSign);
   AIni.WriteString(ASectionName, 'Executable', UTF8Encode(FKeyToolExecutable));
end;

{ TCodeSigningOptionsAppleCodeSign }

constructor TCodeSigningOptionsAppleCodeSign.Create;
begin
   FCertificate := TCustomFileSignerCertificate.Create;
end;

destructor TCodeSigningOptionsAppleCodeSign.Destroy;
begin
   FCertificate.Free;
   inherited Destroy;
end;

procedure TCodeSigningOptionsAppleCodeSign.Assign(Source: TPersistent);
begin
   if Source is TCodeSigningOptionsAppleCodeSign then begin
      Self.FAutoSign := TCodeSigningOptionsAppleCodeSign(Source).FAutoSign;
      Self.FCodeSignExecutable := TCodeSigningOptionsAppleCodeSign(Source).FCodeSignExecutable;
      Self.FCertificate.Assign(TCodeSigningOptionsAppleCodeSign(Source).FCertificate);
   end else begin
      inherited Assign(Source);
   end;
end;

procedure TCodeSigningOptionsAppleCodeSign.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   FAutoSign := AIni.ReadBool(ASectionName, 'AutoSign', True);
   FCodeSignExecutable := UTF8Decode(AIni.ReadString(ASectionName, 'Executable', 'codesign'));
   FCertificate.LoadFromIni(AIni, ASectionName);
end;

procedure TCodeSigningOptionsAppleCodeSign.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteBool(ASectionName, 'AutoSign', FAutoSign);
   AIni.WriteString(ASectionName, 'Executable', UTF8Encode(FCodeSignExecutable));
   FCertificate.SaveToIni(AIni, ASectionName);
end;

{ TCodeSigningOptionsMicrosoftSignTool }

function TCodeSigningOptionsMicrosoftSignTool.GetCertificateSource: TCodeSignCertificateSource;
begin
   Result := FCertificate.Source;
end;

constructor TCodeSigningOptionsMicrosoftSignTool.Create;
begin
   FCertificate := TCustomFileSignerCertificate.Create;
   FTimestamping := TMicrosoftSignToolTimestamping.Create;
   FCrossSigning := TMicrosoftSignToolCrossSigning.Create;
   FFlags := TMicrosoftSignToolFlags.Create;
end;

destructor TCodeSigningOptionsMicrosoftSignTool.Destroy;
begin
   FTimestamping.Free;
   FCrossSigning.Free;
   FFlags.Free;
   FCertificate.Free;
   inherited Destroy;
end;

procedure TCodeSigningOptionsMicrosoftSignTool.Assign(Source: TPersistent);
begin
   if Source is TCodeSigningOptionsMicrosoftSignTool then begin
      Self.FAutoSign := TCodeSigningOptionsMicrosoftSignTool(Source).FAutoSign;
      Self.FSignToolExecutable := TCodeSigningOptionsMicrosoftSignTool(Source).FSignToolExecutable;
      Self.FSignWithSHA1 := TCodeSigningOptionsMicrosoftSignTool(Source).FSignWithSHA1;
      Self.FSignWithSHA256 := TCodeSigningOptionsMicrosoftSignTool(Source).FSignWithSHA256;
      Self.FCertificate.Assign(TCodeSigningOptionsMicrosoftSignTool(Source).FCertificate);
      Self.FTimestamping.Assign(TCodeSigningOptionsMicrosoftSignTool(Source).FTimestamping);
      Self.FCrossSigning.Assign(TCodeSigningOptionsMicrosoftSignTool(Source).FCrossSigning);
      Self.FFlags.Assign(TCodeSigningOptionsMicrosoftSignTool(Source).FFlags);
   end else begin
      inherited Assign(Source);
   end;
end;

procedure TCodeSigningOptionsMicrosoftSignTool.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
var
   s: WideString;
begin
   s := '';
   GetSignToolExecutable(s);
   FAutoSign := AIni.ReadBool(ASectionName, 'AutoSign', True);
   FSignToolExecutable := UTF8Decode(AIni.ReadString(ASectionName, 'Executable', UTF8Encode(s)));
   FSignWithSHA1 := AIni.ReadBool(ASectionName, 'SignWithSHA1', True);
   FSignWithSHA256 := AIni.ReadBool(ASectionName, 'SignWithSHA256', True);
   FCertificate.LoadFromIni(AIni, ASectionName);
   FTimestamping.LoadFromIni(AIni, ASectionName);
   FCrossSigning.LoadFromIni(AIni, ASectionName);
   Flags.LoadFromIni(AIni, ASectionName);
end;

procedure TCodeSigningOptionsMicrosoftSignTool.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteBool(ASectionName, 'AutoSign', FAutoSign);
   AIni.WriteString(ASectionName, 'Executable', UTF8Encode(FSignToolExecutable));
   AIni.WriteBool(ASectionName, 'SignWithSHA1', FSignWithSHA1);
   AIni.WriteBool(ASectionName, 'SignWithSHA256', FSignWithSHA256);
   FCertificate.SaveToIni(AIni, ASectionName);
   FTimestamping.SaveToIni(AIni, ASectionName);
   FCrossSigning.SaveToIni(AIni, ASectionName);
   Flags.SaveToIni(AIni, ASectionName);
end;

{ TCodeSigningOptionsGnuPG }

constructor TCodeSigningOptionsGnuPG.Create;
begin
   FCertificate := TCustomFileSignerCertificate.Create;
end;

destructor TCodeSigningOptionsGnuPG.Destroy;
begin
   FCertificate.Free;
   inherited Destroy;
end;

procedure TCodeSigningOptionsGnuPG.Assign(Source: TPersistent);
begin
   if Source is TCodeSigningOptionsGnuPG then begin
      Self.FAutoSign := TCodeSigningOptionsGnuPG(Source).FAutoSign;
      Self.FGPGExecutable := TCodeSigningOptionsGnuPG(Source).FGPGExecutable;
      Self.FUseCustomKey := TCodeSigningOptionsGnuPG(Source).FUseCustomKey;
      Self.FCertificate.Assign(TCodeSigningOptionsGnuPG(Source).FCertificate);
   end else begin
      inherited Assign(Source);
   end;
end;

procedure TCodeSigningOptionsGnuPG.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   FGPGExecutable := UTF8Decode(AIni.ReadString(ASectionName, 'GPGExecutable', 'gpg'));
   FAutoSign := AIni.ReadBool(ASectionName, 'AutoSign', True);
   FUseCustomKey := AIni.ReadBool(ASectionName, 'UseCustomKey', True);
   FCertificate.LoadFromIni(AIni, ASectionName);
end;

procedure TCodeSigningOptionsGnuPG.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteString(ASectionName, 'GPGExecutable', UTF8Encode(FGPGExecutable));
   AIni.WriteBool(ASectionName, 'AutoSign', FAutoSign);
   AIni.WriteBool(ASectionName, 'UseCustomKey', FUseCustomKey);
   FCertificate.SaveToIni(AIni, ASectionName);
end;

{ TCodeSigningOptions }

function TCodeSigningOptions.GetConfigFilename: string;
begin
   if (Length(FConfigFilename) = 0) then begin
      try
         if Assigned(LazarusIDE) then begin
            FConfigFilename := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'CodeSigningHelper.ini';
         end;
      except
         on E: Exception do begin
            if Assigned(IDEMessagesWindow) then begin
               IDEMessagesWindow.AddCustomMessage(mluError, rsCodeSigningErrorGetPrimaryConfigPath + LineEnding + E.Message, '', 0, 0, 'CodeSigning Debug');
            end else begin
               ShowMessage(rsCodeSigningErrorGetPrimaryConfigPath + LineEnding + E.Message);
            end;
         end;
      end;
   end;
   Result := FConfigFilename;
   CodeSigningLogInformation(Self, 'GetConfigFilename:' + Result);
end;

procedure TCodeSigningOptions.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   CodeSigningLogInformation(Self, Format('TCodeSigningOptions.LoadFromIni:'#13#10'Loading options from section "%s" from file %s', [ASectionName, AIni.FileName]));
   FAppleCodeSignOptions.LoadFromIni(AIni, ASectionName + '.AppleCodeSign');
   FGnuPGOptions.LoadFromIni(AIni, ASectionName + '.GnuPG');
   FJavaKeyToolOptions.LoadFromIni(AIni, ASectionName + '.JavaKeyTool');
   FMicrosoftSignToolOptions.LoadFromIni(AIni, ASectionName + '.MicrosoftSignTool');
end;

procedure TCodeSigningOptions.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   CodeSigningLogInformation(Self, Format('TCodeSigningOptions.SaveToIni:'#13#10'Saving options to section "%s" to file %s', [ASectionName, AIni.FileName]));
   FAppleCodeSignOptions.SaveToIni(AIni, ASectionName + '.AppleCodeSign');
   FGnuPGOptions.SaveToIni(AIni, ASectionName + '.GnuPG');
   FJavaKeyToolOptions.SaveToIni(AIni, ASectionName + '.JavaKeyTool');
   FMicrosoftSignToolOptions.SaveToIni(AIni, ASectionName + '.MicrosoftSignTool');
end;

constructor TCodeSigningOptions.Create;
begin
   inherited Create;
   FConfigFilename := '';
   FAppleCodeSignOptions := TCodeSigningOptionsAppleCodeSign.Create;
   FGnuPGOptions := TCodeSigningOptionsGnuPG.Create;
   FJavaKeyToolOptions := TCodeSigningOptionsJavaKeyTool.Create;
   FMicrosoftSignToolOptions := TCodeSigningOptionsMicrosoftSignTool.Create;
   Read;
end;

destructor TCodeSigningOptions.Destroy;
begin
   Write;
   FAppleCodeSignOptions.Free;
   FGnuPGOptions.Free;
   FJavaKeyToolOptions.Free;
   FMicrosoftSignToolOptions.Free;
   inherited Destroy;
end;

procedure TCodeSigningOptions.Read;
var
   mif: TMemIniFile;
   sFilename: string;
begin
   sFilename := GetConfigFilename;
   if Length(sFilename) = 0 then begin
      CodeSigningLogInformation(Self, 'Unable to read options, no config filename available.');
      Exit;
   end;
   try
      //CodeSigningLogInformation(Self, 'Read:' + sFilename);
      mif := TMemIniFile.Create(sFilename);
      try
         LoadFromIni(mif, 'CodeSigning');
      finally
         mif.Free;
      end;
   except
      on E: Exception do begin
         if Assigned(IDEMessagesWindow) then begin
            IDEMessagesWindow.AddCustomMessage(mluError, 'TCodeSigningOptions.Read: ' + E.Message, '', 0, 0, 'CodeSigning Debug');
         end else begin
            ShowMessage(E.Message + LineEnding + DumpExceptionCallStack(E));
         end;
      end;
   end;
end;

procedure TCodeSigningOptions.Write;
var
   mif: TMemIniFile;
   sFilename: string;
   sErrorMessage: string;
begin
   sFilename := GetConfigFilename;
   if Length(sFilename) = 0 then begin
      CodeSigningLogInformation(Self, 'Unable to write options, no config filename available.');
      Exit;
   end;
   try
      //CodeSigningLogInformation(Self, 'Write:' + sFilename);
      mif := TMemIniFile.Create(sFilename);
      try
         SaveToIni(mif, 'CodeSigning');
         mif.UpdateFile;
      finally
         mif.Free;
      end;
   except
      on E: Exception do begin
         sErrorMessage := Format(rsCodeSigningErrorWritingConfig, [GetConfigFilename, E.Message]);
         if Assigned(IDEMessagesWindow) then begin
            IDEMessagesWindow.AddCustomMessage(mluError, sErrorMessage, '', 0, 0, 'CodeSigning Debug');
         end else begin
            ShowMessage(sErrorMessage + LineEnding + DumpExceptionCallStack(E));
         end;
      end;
   end;
end;

procedure TCodeSigningOptions.Assign(Source: TPersistent);
begin
   CodeSigningLogInformation(Self, 'TCodeSigningOptions.Assign');
   if Source is TCodeSigningOptions then begin
      AppleCodeSignOptions.Assign(TCodeSigningOptions(Source).AppleCodeSignOptions);
      GnuPGOptions.Assign(TCodeSigningOptions(Source).GnuPGOptions);
      JavaKeyToolOptions.Assign(TCodeSigningOptions(Source).JavaKeyToolOptions);
      MicrosoftSignToolOptions.Assign(TCodeSigningOptions(Source).MicrosoftSignToolOptions);
   end else begin
      inherited Assign(Source);
   end;
end;

procedure TCodeSigningOptions.DoAfterWrite(Restore: boolean);
begin
   CodeSigningLogInformation(Self, 'TCodeSigningOptions.DoAfterWrite');
   inherited DoAfterWrite(Restore);
   Write;
end;

class function TCodeSigningOptions.GetGroupCaption: string;
begin
   Result := rsCodeSigningOptionsGroupName;
end;

class function TCodeSigningOptions.GetInstance: TAbstractIDEOptions;
begin
   Result := CodeSigningOptions;
end;

initialization

   //RegisterIDEOptionsGroup(... , TCodeSigningptions);

finalization

   GCodeSigningOptions.Free;

end.
