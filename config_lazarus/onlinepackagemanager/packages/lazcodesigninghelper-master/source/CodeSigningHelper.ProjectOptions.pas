{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Project specific settings.)

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

unit CodeSigningHelper.ProjectOptions;

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
   CodeSigningHelper.Options;

type

   { TCodeSigningProjectOptionsMicrosoftSignTool }

   TCodeSigningProjectOptionsMicrosoftSignTool = class(TPersistent)
   private
      FUseSpecificAlgorithms: boolean;
      FUseSpecificCertificate: boolean;
      FUseSpecificFlags: boolean;
      FUseSpecificTimestamping: boolean;
   protected
      procedure AssignTo(Dest: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
   public
      constructor Create;
      property UseSpecificCertificate: boolean read FUseSpecificCertificate write FUseSpecificCertificate;
      property UseSpecificTimestamping: boolean read FUseSpecificTimestamping write FUseSpecificTimestamping;
      property UseSpecificAlgorithms: boolean read FUseSpecificAlgorithms write FUseSpecificAlgorithms;
      property UseSpecificFlags: boolean read FUseSpecificFlags write FUseSpecificFlags;
   end;

   { TCodeSigningProjectOptionsAppleCodeSign }

   TCodeSigningProjectOptionsAppleCodeSign = class(TPersistent)
   private
      FUseSpecificCertificate: boolean;
   protected
      procedure AssignTo(Dest: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
   public
      constructor Create;
      property UseSpecificCertificate: boolean read FUseSpecificCertificate write FUseSpecificCertificate;
   end;

   { TCodeSigningProjectOptionsGnuPG }

   TCodeSigningProjectOptionsGnuPG = class(TPersistent)
   private
      FUseSpecificCertificate: boolean;
   protected
      procedure AssignTo(Dest: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
   public
      constructor Create;
      property UseSpecificCertificate: boolean read FUseSpecificCertificate write FUseSpecificCertificate;
   end;

   { TCodeSigningProjectOptionsJavaKeyTool }

   TCodeSigningProjectOptionsJavaKeyTool = class(TPersistent)
   private
   protected
      procedure AssignTo(Dest: TPersistent); override;
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string);
   public
   end;

   { TCodeSigningProjectOptions }

   TCodeSigningProjectOptions = class(TAbstractIDEOptions)
   private
      class var FInstance: TCodeSigningProjectOptions;
   private
      FAppleCodeSignCustom: TCodeSigningProjectOptionsAppleCodeSign;
      FAppleCodeSignOptions: TCodeSigningOptionsAppleCodeSign;
      FGnuPGCustom: TCodeSigningProjectOptionsGnuPG;
      FGnuPGOptions: TCodeSigningOptionsGnuPG;
      FJavaKeyToolCustom: TCodeSigningProjectOptionsJavaKeyTool;
      FJavaKeyToolOptions: TCodeSigningOptionsJavaKeyTool;
      FMicrosoftSignToolCustom: TCodeSigningProjectOptionsMicrosoftSignTool;
      FMicrosoftSignToolOptions: TCodeSigningOptionsMicrosoftSignTool;
   protected
      procedure LoadFromIni(AIni: TCustomIniFile; ASectionName: string); //override;
      procedure SaveToIni(AIni: TCustomIniFile; ASectionName: string); //override;
      function GetConfigFilename: string; //override;
   public
      class constructor Create;
      class destructor Destroy;
      class function GetGroupCaption: string; override;
      class function GetInstance: TAbstractIDEOptions; override;
      public
      constructor Create; //override;
      destructor Destroy; override;
      procedure Read;
      procedure Write;
      procedure Assign(Source: TPersistent); override;
      property MicrosoftSignToolOptions: TCodeSigningOptionsMicrosoftSignTool read FMicrosoftSignToolOptions;
      property AppleCodeSignOptions: TCodeSigningOptionsAppleCodeSign read FAppleCodeSignOptions;
      property JavaKeyToolOptions: TCodeSigningOptionsJavaKeyTool read FJavaKeyToolOptions;
      property GnuPGOptions: TCodeSigningOptionsGnuPG read FGnuPGOptions;
      property AppleCodeSignCustom: TCodeSigningProjectOptionsAppleCodeSign read FAppleCodeSignCustom;
      property GnuPGCustom: TCodeSigningProjectOptionsGnuPG read FGnuPGCustom;
      property JavaKeyToolCustom: TCodeSigningProjectOptionsJavaKeyTool read FJavaKeyToolCustom;
      property MicrosoftSignToolCustom: TCodeSigningProjectOptionsMicrosoftSignTool read FMicrosoftSignToolCustom;
   end;

implementation

uses
   Dialogs,
   IDEIntf,
   IDEMsgIntf,
   IDEExternToolIntf,
   CodeSigningHelper.Debug,
   CodeSigningHelper.Strings;

var
   GCodeSigningProjectOptions: TCodeSigningProjectOptions = nil;

{ TCodeSigningProjectOptionsJavaKeyTool }

procedure TCodeSigningProjectOptionsJavaKeyTool.AssignTo(Dest: TPersistent);
begin
   if Dest is TCodeSigningProjectOptionsJavaKeyTool then begin
   end;
end;

procedure TCodeSigningProjectOptionsJavaKeyTool.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin

end;

procedure TCodeSigningProjectOptionsJavaKeyTool.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin

end;

{ TCodeSigningProjectOptionsGnuPG }

procedure TCodeSigningProjectOptionsGnuPG.AssignTo(Dest: TPersistent);
begin
   if Dest is TCodeSigningProjectOptionsGnuPG then begin
      TCodeSigningProjectOptionsGnuPG(Dest).FUseSpecificCertificate := Self.FUseSpecificCertificate;
   end;
end;

procedure TCodeSigningProjectOptionsGnuPG.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   FUseSpecificCertificate := AIni.ReadBool(ASectionName, 'UseSpecificCertificate', False);
end;

procedure TCodeSigningProjectOptionsGnuPG.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteBool(ASectionName, 'UseSpecificCertificate', FUseSpecificCertificate);
end;

constructor TCodeSigningProjectOptionsGnuPG.Create;
begin
   UseSpecificCertificate := False;
end;

{ TCodeSigningProjectOptionsAppleCodeSign }

procedure TCodeSigningProjectOptionsAppleCodeSign.AssignTo(Dest: TPersistent);
begin
   if Dest is TCodeSigningProjectOptionsAppleCodeSign then begin
      TCodeSigningProjectOptionsAppleCodeSign(Dest).FUseSpecificCertificate := Self.FUseSpecificCertificate;
   end;
end;

procedure TCodeSigningProjectOptionsAppleCodeSign.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   FUseSpecificCertificate := AIni.ReadBool(ASectionName, 'UseSpecificCertificate', False);
end;

procedure TCodeSigningProjectOptionsAppleCodeSign.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteBool(ASectionName, 'UseSpecificCertificate', FUseSpecificCertificate);
end;

constructor TCodeSigningProjectOptionsAppleCodeSign.Create;
begin
   UseSpecificCertificate := False;
end;

{ TCodeSigningProjectOptionsMicrosoftSignTool }

procedure TCodeSigningProjectOptionsMicrosoftSignTool.AssignTo(Dest: TPersistent);
begin
   if Dest is TCodeSigningProjectOptionsMicrosoftSignTool then begin
      TCodeSigningProjectOptionsMicrosoftSignTool(Dest).FUseSpecificAlgorithms := Self.FUseSpecificAlgorithms;
      TCodeSigningProjectOptionsMicrosoftSignTool(Dest).FUseSpecificCertificate := Self.FUseSpecificCertificate;
      TCodeSigningProjectOptionsMicrosoftSignTool(Dest).FUseSpecificFlags := Self.FUseSpecificFlags;
      TCodeSigningProjectOptionsMicrosoftSignTool(Dest).FUseSpecificTimestamping := Self.FUseSpecificAlgorithms;
   end;
end;

procedure TCodeSigningProjectOptionsMicrosoftSignTool.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   FUseSpecificCertificate := AIni.ReadBool(ASectionName, 'UseSpecificCertificate', False);
   FUseSpecificTimestamping := AIni.ReadBool(ASectionName, 'UseSpecificTimestamping', False);
   FUseSpecificAlgorithms := AIni.ReadBool(ASectionName, 'UseSpecificAlgorithms', False);
   FUseSpecificFlags := AIni.ReadBool(ASectionName, 'UseSpecificFlags', False);
end;

procedure TCodeSigningProjectOptionsMicrosoftSignTool.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   AIni.WriteBool(ASectionName, 'UseSpecificCertificate', FUseSpecificCertificate);
   AIni.WriteBool(ASectionName, 'UseSpecificTimestamping', FUseSpecificTimestamping);
   AIni.WriteBool(ASectionName, 'UseSpecificAlgorithms', FUseSpecificAlgorithms);
   AIni.WriteBool(ASectionName, 'UseSpecificFlags', FUseSpecificFlags);
end;

constructor TCodeSigningProjectOptionsMicrosoftSignTool.Create;
begin
   UseSpecificAlgorithms := False;
   UseSpecificCertificate := False;
   UseSpecificFlags := False;
   UseSpecificTimestamping := False;
end;

{ TCodeSigningProjectOptions }

procedure TCodeSigningProjectOptions.LoadFromIni(AIni: TCustomIniFile; ASectionName: string);
begin
   CodeSigningLogInformation(Self, Format('TCodeSigningProjectOptions.LoadFromIni:'#13#10'Loading project options from section "%s" from file %s', [ASectionName, AIni.FileName]));
   //inherited LoadFromIni(AIni, ASectionName);
   FAppleCodeSignCustom.LoadFromIni(AIni, ASectionName + '.AppleCodeSign');
   FGnuPGCustom.LoadFromIni(AIni, ASectionName + '.GnuPG');
   FJavaKeyToolCustom.LoadFromIni(AIni, ASectionName + '.JavaKeyTool');
   FMicrosoftSignToolCustom.LoadFromIni(AIni, ASectionName + '.MicrosoftSignTool');
   FAppleCodeSignOptions.LoadFromIni(AIni, ASectionName + '.AppleCodeSign');
   FGnuPGOptions.LoadFromIni(AIni, ASectionName + '.GnuPG');
   FJavaKeyToolOptions.LoadFromIni(AIni, ASectionName + '.JavaKeyTool');
   FMicrosoftSignToolOptions.LoadFromIni(AIni, ASectionName + '.MicrosoftSignTool');
end;

procedure TCodeSigningProjectOptions.SaveToIni(AIni: TCustomIniFile; ASectionName: string);
begin
   CodeSigningLogInformation(Self, Format('TCodeSigningProjectOptions.SaveToIni:'#13#10'Saving project options to section "%s" to file %s', [ASectionName, AIni.FileName]));
   //inherited SaveToIni(AIni, ASectionName);
   FAppleCodeSignCustom.SaveToIni(AIni, ASectionName + '.AppleCodeSign');
   FGnuPGCustom.SaveToIni(AIni, ASectionName + '.GnuPG');
   FJavaKeyToolCustom.SaveToIni(AIni, ASectionName + '.JavaKeyTool');
   FMicrosoftSignToolCustom.SaveToIni(AIni, ASectionName + '.MicrosoftSignTool');
   FAppleCodeSignOptions.SaveToIni(AIni, ASectionName + '.AppleCodeSign');
   FGnuPGOptions.SaveToIni(AIni, ASectionName + '.GnuPG');
   FJavaKeyToolOptions.SaveToIni(AIni, ASectionName + '.JavaKeyTool');
   FMicrosoftSignToolOptions.SaveToIni(AIni, ASectionName + '.MicrosoftSignTool');
end;

function TCodeSigningProjectOptions.GetConfigFilename: string;
begin
   Result := '';
   try
      if Assigned(LazarusIDE) then begin
         if Assigned(LazarusIDE.ActiveProject) then begin
            if Assigned(LazarusIDE.ActiveProject.MainFile) then begin
               Result := LazarusIDE.ActiveProject.MainFile.Filename + '.codesigning.ini';
            end;
         end;
      end;
   except
      on E: Exception do begin
         if Assigned(IDEMessagesWindow) then begin
            IDEMessagesWindow.AddCustomMessage(mluError, rsCodeSigningErrorLazarusIDEActiveProjectMainFileFilename + LineEnding + E.Message, '', 0, 0, 'CodeSigning Debug');
         end else begin
            ShowMessage(rsCodeSigningErrorLazarusIDEActiveProjectMainFileFilename + LineEnding + E.Message);
         end;
         Result := '';
      end;
   end;
   CodeSigningLogInformation(Self, 'GetConfigFilename:' + Result);
end;

constructor TCodeSigningProjectOptions.Create;
begin
   FAppleCodeSignOptions := TCodeSigningOptionsAppleCodeSign.Create;
   FGnuPGOptions := TCodeSigningOptionsGnuPG.Create;
   FJavaKeyToolOptions := TCodeSigningOptionsJavaKeyTool.Create;
   FMicrosoftSignToolOptions := TCodeSigningOptionsMicrosoftSignTool.Create;
   FAppleCodeSignCustom := TCodeSigningProjectOptionsAppleCodeSign.Create;
   FGnuPGCustom := TCodeSigningProjectOptionsGnuPG.Create;
   FJavaKeyToolCustom := TCodeSigningProjectOptionsJavaKeyTool.Create;
   FMicrosoftSignToolCustom := TCodeSigningProjectOptionsMicrosoftSignTool.Create;
   inherited Create;
end;

destructor TCodeSigningProjectOptions.Destroy;
begin
   FAppleCodeSignOptions.Free;
   FGnuPGOptions.Free;
   FJavaKeyToolOptions.Free;
   FMicrosoftSignToolOptions.Free;
   FAppleCodeSignCustom.Free;
   FGnuPGCustom.Free;
   FJavaKeyToolCustom.Free;
   FMicrosoftSignToolCustom.Free;
   inherited Destroy;
end;

procedure TCodeSigningProjectOptions.Read;
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

procedure TCodeSigningProjectOptions.Write;
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

class constructor TCodeSigningProjectOptions.Create;
begin
   FInstance := nil;
end;

class destructor TCodeSigningProjectOptions.Destroy;
begin
   FInstance.Free;
end;

class function TCodeSigningProjectOptions.GetGroupCaption: string;
begin
   Result := rsCodeSigningProjectOptionsGroupName;
end;

class function TCodeSigningProjectOptions.GetInstance: TAbstractIDEOptions;
begin
   if not Assigned(FInstance) then begin
      FInstance := TCodeSigningProjectOptions.Create;
   end;
   Result := FInstance;
end;

procedure TCodeSigningProjectOptions.Assign(Source: TPersistent);
begin
   if Source is TCodeSigningProjectOptions then begin
      AppleCodeSignCustom.Assign(TCodeSigningProjectOptions(Source).AppleCodeSignCustom);
      GnuPGCustom.Assign(TCodeSigningProjectOptions(Source).GnuPGCustom);
      JavaKeyToolCustom.Assign(TCodeSigningProjectOptions(Source).JavaKeyToolCustom);
      MicrosoftSignToolCustom.Assign(TCodeSigningProjectOptions(Source).MicrosoftSignToolCustom);
      inherited Assign(Source);
   end else begin
      inherited Assign(Source);
   end;
end;

initialization

   RegisterIDEOptionsGroup(GroupProject, TCodeSigningProjectOptions);

finalization

   GCodeSigningProjectOptions.Free;

end.
