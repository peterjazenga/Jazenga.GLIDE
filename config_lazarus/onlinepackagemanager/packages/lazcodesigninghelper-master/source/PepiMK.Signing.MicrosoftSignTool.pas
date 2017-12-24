{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Codesigning implementation for Microsofts signtool.exe.)

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
// 2017-05-17  pk  10m  Fixed verify issue (missing parameter for the policy).
// 2017-05-17  pk  30m  Moved from old PepiMK.Authenticode.Sign.External unit.
// 2017-05-17  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit PepiMK.Signing.MicrosoftSignTool;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   IniFiles,
   PepiMK.Signing.Base;

type
   TCodeSignAlgorithm = (csaSHA1, csaSHA256);
   TCodeSignAlgorithms = set of TCodeSignAlgorithm;
   TCodeSignPageHashing = (csphYes, csphNo, csphDefault);

   { TMicrosoftSignToolCrossSigning }

   TMicrosoftSignToolCrossSigning = class(TPersistent)
   private
      FActive: boolean;
      FFilename: WideString;
   protected
      procedure AssignTo(Dest: TPersistent); override;
   public
      constructor Create;
      procedure LoadFromIni(AIni: TCustomIniFile; ASection: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASection: string);
      property Active: boolean read FActive write FActive;
      property Filename: WideString read FFilename write FFilename;
   end;

   TMicrosoftSignToolTimestampingMethod = (tstClassic, tstRFC3161, tstSeal);

   { TMicrosoftSignToolTimestamping }

   TMicrosoftSignToolTimestamping = class(TPersistent)
   private
      FActive: boolean;
      FMethod: TMicrosoftSignToolTimestampingMethod;
      FServer: string;
      FServerRFC3161: string;
   protected
      procedure AssignTo(Dest: TPersistent); override;
   public
      constructor Create;
      procedure LoadFromIni(AIni: TCustomIniFile; ASection: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASection: string);
      property Active: boolean read FActive write FActive;
      property Server: string read FServer write FServer;
      property ServerRFC3161: string read FServerRFC3161 write FServerRFC3161;
      property Method: TMicrosoftSignToolTimestampingMethod read FMethod write FMethod;
   end;

   { TMicrosoftSignToolFlags }

   TMicrosoftSignToolFlags = class(TPersistent)
   private
      FUsePageHashing: TCodeSignPageHashing;
      FUseWindowsSystemComponentVerification: boolean;
   protected
      procedure AssignTo(Dest: TPersistent); override;
   public
      constructor Create;
      procedure LoadFromIni(AIni: TCustomIniFile; ASection: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASection: string);
   public
      property UsePageHashing: TCodeSignPageHashing read FUsePageHashing write FUsePageHashing;
      property UseWindowsSystemComponentVerification: boolean read FUseWindowsSystemComponentVerification write FUseWindowsSystemComponentVerification;
   end;

   { TMicrosoftSignToolSigner }

   TMicrosoftSignToolSigner = class(TCustomFileSigner)
   private
      FAlgorithm: TCodeSignAlgorithm;
      FAlgorithms: TCodeSignAlgorithms;
      FCrossSigning: TMicrosoftSignToolCrossSigning;
      FDescriptionSubject: WideString;
      FDescriptionURL: ansistring;
      FFlags: TMicrosoftSignToolFlags;
      FTimestamping: TMicrosoftSignToolTimestamping;
   protected
      procedure ConstructSignParameters(AAdder: TConstructParametersProc); override;
      procedure ConstructVerifyParameters(AAdder: TConstructParametersProc); override;
      function ReadDefaultsIniSectionName: ansistring; override;
   public
      class function SupportsLazarusTargetOS(AOS: string): boolean; override;
   public
      constructor Create; override;
      destructor Destroy; override;
      function SignFile(const AFilename: WideString): boolean; override;
      property DescriptionURL: ansistring read FDescriptionURL write FDescriptionURL;
      property DescriptionSubject: WideString read FDescriptionSubject write FDescriptionSubject;
      property Algorithms: TCodeSignAlgorithms read FAlgorithms write FAlgorithms;
      property Flags: TMicrosoftSignToolFlags read FFlags;
      property CrossSigning: TMicrosoftSignToolCrossSigning read FCrossSigning;
      property Timestamping: TMicrosoftSignToolTimestamping read FTimestamping;
   end;

function GetSignToolExecutable(var ASigningExecutable: WideString): boolean;

implementation

function GetSignToolExecutable(var ASigningExecutable: WideString): boolean;
begin
   Result := False;
   if FileExists(ExtractFilePath(ParamStr(0)) + 'signtool.exe') then begin
      ASigningExecutable := ExtractFilePath(UTF8Decode(ParamStr(0))) + 'signtool.exe';
      Result := True;
   end;
   if FileExists('c:\Program Files (x86)\Windows Kits\10\bin\x86\signtool.exe') then begin
      ASigningExecutable := 'c:\Program Files (x86)\Windows Kits\10\bin\x86\signtool.exe';
      Result := True;
   end;
end;

{ TMicrosoftSignToolFlags }

procedure TMicrosoftSignToolFlags.AssignTo(Dest: TPersistent);
begin
   if Dest is TMicrosoftSignToolFlags then begin
      TMicrosoftSignToolFlags(Dest).UsePageHashing := Self.UsePageHashing;
      TMicrosoftSignToolFlags(Dest).UseWindowsSystemComponentVerification := Self.UseWindowsSystemComponentVerification;
   end;
end;

constructor TMicrosoftSignToolFlags.Create;
begin
   FUseWindowsSystemComponentVerification := False;
   FUsePageHashing := csphDefault;
end;

procedure TMicrosoftSignToolFlags.LoadFromIni(AIni: TCustomIniFile; ASection: string);
begin
   FUsePageHashing := TCodeSignPageHashing(AIni.ReadInteger(ASection, 'Flags.PageHashing', integer(FUsePageHashing)));
   FUseWindowsSystemComponentVerification := AIni.ReadBool(ASection, 'Flags.WindowsSystemComponentVerification', FUseWindowsSystemComponentVerification);
end;

procedure TMicrosoftSignToolFlags.SaveToIni(AIni: TCustomIniFile; ASection: string);
begin
   AIni.WriteInteger(ASection, 'Flags.PageHashing', integer(FUsePageHashing));
   AIni.WriteBool(ASection, 'Flags.WindowsSystemComponentVerification', FUseWindowsSystemComponentVerification);
end;

{ TMicrosoftSignToolTimestamping }

procedure TMicrosoftSignToolTimestamping.AssignTo(Dest: TPersistent);
begin
   if Dest is TMicrosoftSignToolTimestamping then begin
      TMicrosoftSignToolTimestamping(Dest).Active := Self.Active;
      TMicrosoftSignToolTimestamping(Dest).Server := Self.Server;
      TMicrosoftSignToolTimestamping(Dest).ServerRFC3161 := Self.ServerRFC3161;
   end;
end;

constructor TMicrosoftSignToolTimestamping.Create;
begin
   FActive := False;
   FMethod := tstRFC3161;
end;

procedure TMicrosoftSignToolTimestamping.LoadFromIni(AIni: TCustomIniFile; ASection: string);
begin
   FActive := AIni.ReadBool(ASection, 'Timestamping.Active', FActive);
   FServer := AIni.ReadString(ASection, 'Timestamping.Server', FServer);
   FServerRFC3161 := AIni.ReadString(ASection, 'Timestamping.ServerRFC3161', FServerRFC3161);
end;

procedure TMicrosoftSignToolTimestamping.SaveToIni(AIni: TCustomIniFile; ASection: string);
begin
   AIni.WriteBool(ASection, 'Timestamping.Active', FActive);
   AIni.WriteString(ASection, 'Timestamping.Server', FServer);
   AIni.WriteString(ASection, 'Timestamping.ServerRFC3161', FServerRFC3161);
end;

{ TMicrosoftSignToolCrossSigning }

procedure TMicrosoftSignToolCrossSigning.AssignTo(Dest: TPersistent);
begin
   if Dest is TMicrosoftSignToolCrossSigning then begin
      TMicrosoftSignToolCrossSigning(Dest).Active := Self.Active;
      TMicrosoftSignToolCrossSigning(Dest).Filename := Self.Filename;
   end;
end;

constructor TMicrosoftSignToolCrossSigning.Create;
begin
   FActive := False;
end;

procedure TMicrosoftSignToolCrossSigning.LoadFromIni(AIni: TCustomIniFile; ASection: string);
begin
   FActive := AIni.ReadBool(ASection, 'CrossSigning.Active', FActive);
   FFilename := UTF8Decode(AIni.ReadString(ASection, 'CrossSigning.Filename', UTF8Encode(FFilename)));
end;

procedure TMicrosoftSignToolCrossSigning.SaveToIni(AIni: TCustomIniFile; ASection: string);
begin
   AIni.WriteBool(ASection, 'CrossSigning.Active', FActive);
   AIni.WriteString(ASection, 'CrossSigning.Filename', UTF8Encode(FFilename));
end;

{ TMicrosoftSignToolSigner }

procedure TMicrosoftSignToolSigner.ConstructSignParameters(AAdder: TConstructParametersProc);
begin
   AAdder('sign');
   AAdder('/v');
   case Flags.UsePageHashing of
      csphYes: AAdder('/ph');
      csphNo: AAdder('/nph');
   end;
   case Certificate.Source of
      cscsStoreByHash:
      begin
         AAdder('/sha1');
         AAdder(UTF8Decode(Certificate.Hash));
      end;
      cscsFileAsPFX:
      begin
         AAdder('/f');
         AAdder(Certificate.Filename);
      end;
      cscsStoreBySubstring:
      begin
         AAdder('/n');
         AAdder(UTF8Decode(Certificate.Substring));
      end;
   end;
   if Length(FDescriptionSubject) > 0 then begin
      AAdder('/d');
      AAdder(FDescriptionSubject);
   end;
   if Length(FDescriptionURL) > 0 then begin
      AAdder('/du');
      AAdder(UTF8Decode(FDescriptionURL));
   end;
   if FileExists(CrossSigning.Filename) and (CrossSigning.Active) then begin
      AAdder('/ac');
      AAdder(CrossSigning.Filename);
   end;
   if AppendSignature then begin
      AAdder('/as');
   end;
   case FAlgorithm of
      csaSHA1:
      begin
         AAdder('/fd');
         AAdder('sha1');
      end;
      csaSHA256:
      begin
         AAdder('/fd');
         AAdder('sha256');
      end;
   end;
   if Flags.UseWindowsSystemComponentVerification then begin
      AAdder('/uw');
   end;
   if Timestamping.Active then begin
      case Timestamping.Method of
         tstClassic:
         begin
            AAdder('/t');
            AAdder(UTF8Decode(Timestamping.Server));
         end;
         tstRFC3161:
         begin
            AAdder('/tr');
            AAdder(UTF8Decode(Timestamping.ServerRFC3161));
         end;
         tstSeal:
         begin
            AAdder('/tseal');
            AAdder(UTF8Decode(Timestamping.ServerRFC3161));
         end;
      end;
   end;
   AAdder(Filename);
end;

procedure TMicrosoftSignToolSigner.ConstructVerifyParameters(AAdder: TConstructParametersProc);
begin
   AAdder('verify');
   AAdder('/v');
   AAdder('/all'); // Verify all signatures in a file with multiple signatures.
   AAdder('/pa'); // Use the "Default Authenticode" Verification Policy.
   //AAdder('/sl'); // Verify sealing signatures for supported file types.
   AAdder('/tw'); // Generate a Warning if the signature is not timestamped.
   AAdder(Filename);
end;

function TMicrosoftSignToolSigner.ReadDefaultsIniSectionName: ansistring;
begin
   Result := 'Microsoft.SignTool';
end;

class function TMicrosoftSignToolSigner.SupportsLazarusTargetOS(AOS: string): boolean;
begin
   Result := SameTexT(AOS, 'Win32') or SameText(AOS, 'Win64');
end;

constructor TMicrosoftSignToolSigner.Create;
var
   s: WideString;

begin
   inherited Create;
   FFlags := TMicrosoftSignToolFlags.Create;
   FCrossSigning := TMicrosoftSignToolCrossSigning.Create;
   FTimestamping := TMicrosoftSignToolTimestamping.Create;
   s := SigningExecutable;
   if GetSignToolExecutable(s) then begin
      SigningExecutable := s;
   end;
end;

destructor TMicrosoftSignToolSigner.Destroy;
begin
   FFlags.Free;
   FCrossSigning.Free;
   FTimestamping.Free;
   inherited Destroy;
end;

function TMicrosoftSignToolSigner.SignFile(const AFilename: WideString): boolean;
var
   a: TCodeSignAlgorithm;
   slOutput: TStringList;
   slErrors: TStringList;
   bFirstApplied: boolean;
begin
   Result := True;
   bFirstApplied := False;
   slOutput := TStringList.Create;
   slErrors := TStringList.Create;
   try
      for a in TCodeSignAlgorithm do begin
         if a in Algorithms then begin
            FAlgorithm := a;
            AppendSignature := (bFirstApplied);
            Result := Result and inherited SignFile(AFilename);
            slOutput.AddStrings(Outcome.Output);
            slErrors.AddStrings(Outcome.Errors);
            bFirstApplied := True;
         end;
      end;
   finally
      Outcome.Output.Assign(slOutput);
      slOutput.Free;
      Outcome.Errors.Assign(slErrors);
      slErrors.Free;
   end;
end;

end.
