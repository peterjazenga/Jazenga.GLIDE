{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Filesigning base unit.)

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

unit PepiMK.Signing.Base;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$modeswitch nestedprocvars}
{$ENDIF FPC}

interface

uses
   SysUtils,
   {$IFDEF FPC}
   Process,
   Pipes,
   {$ELSE FPC}
   Windows,
   DosCommand,
   {$ENDIF FPC}
   Classes,
   IniFiles;

type

   TCodeSignCertificateSource = (cscsStoreByHash, cscsStoreBySubstring, cscsFileAsPFX, cscsInherited);
   TConstructParametersProc = procedure(AText: WideString) is nested;
   TConstructMethod = procedure(AAdder: TConstructParametersProc) of object;

   { TCustomFileSignerCertificate }

   TCustomFileSignerCertificate = class(TPersistent)
   private
      FFilename: WideString;
      FHash: ansistring;
      FSource: TCodeSignCertificateSource;
      FSubstring: ansistring;
      procedure SetFilename(AValue: WideString);
      procedure SetHash(AValue: ansistring);
      procedure SetSubstring(AValue: ansistring);
   protected
      procedure AssignTo(Dest: TPersistent); override;
   public
      property Hash: ansistring read FHash write SetHash;
      property Substring: ansistring read FSubstring write SetSubstring;
      property Filename: WideString read FFilename write SetFilename;
      property Source: TCodeSignCertificateSource read FSource write FSource;
      procedure LoadFromIni(AIni: TCustomIniFile; ASection: string);
      procedure SaveToIni(AIni: TCustomIniFile; ASection: string);
   end;

   { TCustomFileSignerResult }

   TCustomFileSignerResult = class
   private
      FCommandLine: string;
      FExitCode: integer;
      FErrors: TStringList;
      FOutput: TStringList;
   public
      constructor Create;
      destructor Destroy; override;
      property CommandLine: string read FCommandLine;
      property ExitCode: integer read FExitCode;
      property Errors: TStringList read FErrors;
      property Output: TStringList read FOutput;
   end;

   { TCustomFileSigner }

   TCustomFileSigner = class
      abstract
   private
      FAppendSignature: boolean;
      FCertificate: TCustomFileSignerCertificate;
      FFilename: WideString;
      FOutcome: TCustomFileSignerResult;
      FSigningExecutable: WideString;
   protected
      procedure ConstructSignParameters(AAdder: TConstructParametersProc); virtual; abstract;
      procedure ConstructVerifyParameters(AAdder: TConstructParametersProc); virtual; abstract;
      procedure ReadDefaults;
      procedure ReadDefaultsFromIniFile(AFilename: string);
      procedure ReadDefaultsFromIni(AIni: TCustomIniFile); virtual;
      function ReadDefaultsIniSectionName: ansistring; virtual;
      function Execute(AConstructor: TConstructMethod): boolean;
      {$IFDEF FPC}
      function ExecuteFreePascal(AConstructor: TConstructMethod): boolean;
      {$ELSE FPC}
      function ExecuteDosCommand(AConstructor: TConstructMethod): boolean;
      procedure DoDosCommandNewExecLine(ASender: TObject; ANewLine: string; AOutputType: TOutputType);
      {$ENDIF FPC}
   public
      class function SupportsLazarusTargetOS(AOS: String): boolean; virtual; abstract;
   public
      constructor Create; virtual;
      destructor Destroy; override;
      function SignFile(const AFilename: WideString): boolean; virtual;
      function VerifyFile(const AFilename: WideString): boolean; virtual;
      property Filename: WideString read FFilename;
      property SigningExecutable: WideString read FSigningExecutable write FSigningExecutable;
      property Certificate: TCustomFileSignerCertificate read FCertificate;
      property AppendSignature: boolean read FAppendSignature write FAppendSignature;
      property Outcome: TCustomFileSignerResult read FOutcome;
   end;

implementation

{ TCustomFileSignerResult }

constructor TCustomFileSignerResult.Create;
begin
   FOutput := TStringList.Create;
   FErrors := TStringList.Create;
end;

destructor TCustomFileSignerResult.Destroy;
begin
   FOutput.Free;
   FErrors.Free;
   inherited Destroy;
end;

{ TCustomFileSignerCertificate }

procedure TCustomFileSignerCertificate.SetFilename(AValue: WideString);
begin
   FFilename := AValue;
end;

procedure TCustomFileSignerCertificate.SetHash(AValue: ansistring);
begin
   FHash := AValue;
end;

procedure TCustomFileSignerCertificate.SetSubstring(AValue: ansistring);
begin
   FSubstring := AValue;
end;

procedure TCustomFileSignerCertificate.AssignTo(Dest: TPersistent);
begin
   if Dest is TCustomFileSignerCertificate then begin
      TCustomFileSignerCertificate(Dest).FSource := Self.Source;
      TCustomFileSignerCertificate(Dest).FSubstring := Self.Substring;
      TCustomFileSignerCertificate(Dest).FHash := Self.Hash;
      TCustomFileSignerCertificate(Dest).FFilename := Self.Filename;
   end;
end;

procedure TCustomFileSignerCertificate.LoadFromIni(AIni: TCustomIniFile; ASection: string);
begin
   FHash := AIni.ReadString(ASection, 'Certificate.Hash', FHash);
   FFilename := UTF8Decode(AIni.ReadString(ASection, 'Certificate.Filename', UTF8Encode(FFilename)));
   FSubstring := AIni.ReadString(ASection, 'Certificate.Substring', FSubstring);
   FSource := TCodeSignCertificateSource(AIni.ReadInteger(ASection, 'Certificate.Source', integer(FSource)));
end;

procedure TCustomFileSignerCertificate.SaveToIni(AIni: TCustomIniFile;
   ASection: string);
begin
   AIni.WriteString(ASection, 'Certificate.Hash', FHash);
   AIni.WriteString(ASection, 'Certificate.Filename', UTF8Encode(FFilename));
   AIni.WriteString(ASection, 'Certificate.Substring', FSubstring);
   AIni.WriteInteger(ASection, 'Certificate.Source', integer(FSource));
end;

{ TCustomFileSigner }

procedure TCustomFileSigner.ReadDefaults;
var
   sFilename: string;
begin
   sFilename := ExtractFilePath(ParamStr(0)) + 'codesigning.defaults.ini';
   if FileExists(sFilename) then begin
      ReadDefaultsFromIniFile(sFilename);
   end;
end;

procedure TCustomFileSigner.ReadDefaultsFromIniFile(AFilename: string);
var
   mif: TMemIniFile;
begin
   mif := TMemIniFile.Create(AFilename);
   try
      ReadDefaultsFromIni(mif);
   finally
      mif.Free;
   end;
end;

procedure TCustomFileSigner.ReadDefaultsFromIni(AIni: TCustomIniFile);
begin
   Certificate.LoadFromIni(AIni, ReadDefaultsIniSectionName);
end;

function TCustomFileSigner.ReadDefaultsIniSectionName: ansistring;
begin
   Result := 'Global';
end;

function TCustomFileSigner.Execute(AConstructor: TConstructMethod): boolean;
begin
   {$IFDEF FPC}
   Result := ExecuteFreePascal(AConstructor);
   {$ELSE FPC}
   Result := ExecuteDosCommand(AConstructor);
   {$ENDIF FPC}
end;

function TCustomFileSigner.ExecuteFreePascal(AConstructor: TConstructMethod): boolean;
var
   p: TProcess;

   procedure Adder(AText: WideString);
   begin
      p.Parameters.Add(UTF8Encode(AText));
   end;

   procedure ProcessInput(AProcess: TProcess);
   var
      ssOutput: TStringStream;
      ssError: TStringStream;
      iLinesPushedError: integer;
      iLinesPushedOutput: integer;

      procedure TriggerLines(ALines: TStrings; AWithoutLastNLines: integer; var ALineCounter: integer);
      var
         i: integer;
      begin
         if (ALines.Count - AWithoutLastNLines > ALineCounter) then begin
            for i := ALineCounter to Pred(ALines.Count) - AWithoutLastNLines do begin
               // TODO : trigger event
            end;
            ALineCounter := Pred(ALines.Count);
         end;
      end;

      function ProcessPipeStream(APipeStream: TInputPipeStream; AString: TStringStream; AOutput: TStringList; var ALineCounter: integer): integer;
      const
         BUF_SIZE = 2048; // Buffer size for reading the output in chunks
      var iRead: integer;
         Buffer: array[1..BUF_SIZE] of byte;
      begin
         iRead := APipeStream.Read(Buffer, BUF_SIZE);
         if (iRead > 0) then begin
            AString.Write(Buffer, iRead);
            AString.Seek(0, soFromBeginning);
            AOutput.Clear;
            AOutput.LoadFromStream(AString);
            TriggerLines(AOutput, 1, ALineCounter);
         end;
         Result := iRead;
      end;

      var iRead: integer;
   begin
      iLinesPushedOutput := 0;
      iLinesPushedError := 0;
      ssOutput := TStringStream.Create('');
      ssError := TStringStream.Create('');
      try
         repeat
            iRead := ProcessPipeStream(AProcess.Output, ssOutput, Outcome.Output, iLinesPushedOutput);
            if p.Stderr.NumBytesAvailable > 0 then begin
               iRead += ProcessPipeStream(AProcess.Stderr, ssError, Outcome.Errors, iLinesPushedError);
            end;
         until (iRead = 0) and (not p.Running);
         TriggerLines(Outcome.Output, 0, iLinesPushedOutput);
         TriggerLines(Outcome.Errors, 0, iLinesPushedError);
      finally
         ssOutput.Free;
         ssError.Free;
      end;
   end;

begin
   try
      p := TProcess.Create(nil);
      try
         p.Options := p.Options + [poUsePipes, poNoConsole]; // , poWaitOnExit , poStderrToOutPut
         p.Executable := UTF8Encode(SigningExecutable);
         p.CurrentDirectory := UTF8Encode(ExtractFilePath(Filename));
         p.Parameters.StrictDelimiter := False;
         p.Parameters.Delimiter := ' ';
         AConstructor(@Adder);
         {$IFDEF Darwin}
         p.Options := p.Options + [poWaitOnExit];
         {$ENDIF Darwin}
         p.Execute;
         ProcessInput(p);
         Result := (p.ExitCode = 0);
         FOutcome.FCommandLine := '"' + p.Executable + '" ' + p.Parameters.DelimitedText;
         FOutcome.FExitCode := p.ExitCode;
      finally
         p.Free;
      end;
   except
      on E: Exception do begin
         Result := False;
         FOutcome.Errors.Add(E.Message);
      end;
   end;
end;

{$IFNDEF FPC}
function TCustomFileSigner.ExecuteDosCommand(AConstructor: TConstructMethod): boolean;
var
   sParameters: WideString;

   procedure Adder(AText: WideString);
   begin
      sParameters := sParameters + ' ' + AText;
   end;

var
   dc: TDosCommand;
begin
   try
      dc := TDosCommand.Create(nil);
      try
         sParameters := '';
         AConstructor(Adder);
         dc.ExecutionPath := ExtractFilePath(Filename);
         dc.CommandLine := '"' + SigningExecutable + '" ' + sParameters;
         dc.OnNewLine := MyOnNewExecLine;
         // dc.ShowWindow := swSHOW;
         dc.Execute2;
         Result := (dc.ExitCode = 0);
         FOutcome.CommandLine := dc.CommandLine;
         FOutcome.ExitCode := dc.ExitCode;
      finally
         dc.Free;
      end;
   except
      Result := False;
   end;
end;

{$ENDIF FPC}

{$IFNDEF FPC}
procedure TCustomFileSigner.DoDosCommandNewExecLine(ASender: TObject; ANewLine: string; AOutputType: TOutputType);
begin
   if OutputType = otEntireLine then begin
      FOutput.Add(NewLine);
   end;
end;

{$ENDIF FPC}

constructor TCustomFileSigner.Create;
begin
   FCertificate := TCustomFileSignerCertificate.Create;
   FOutcome := TCustomFileSignerResult.Create;
   FAppendSignature := False;
end;

destructor TCustomFileSigner.Destroy;
begin
   FCertificate.Free;
   FOutcome.Free;
   inherited Destroy;
end;

function TCustomFileSigner.SignFile(const AFilename: WideString): boolean;
begin
   FFilename := AFilename;
   Outcome.Output.Clear;
   Outcome.Errors.Clear;
   Result := Execute(@ConstructSignParameters);
end;

function TCustomFileSigner.VerifyFile(const AFilename: WideString): boolean;
begin
   FFilename := AFilename;
   Outcome.Output.Clear;
   Outcome.Errors.Clear;
   Result := Execute(@ConstructVerifyParameters);
end;

end.
