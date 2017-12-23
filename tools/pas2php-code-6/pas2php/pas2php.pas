{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit Pas2Php;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, FileUtil, LazFileUtils, LCLIntf, Pas2PhpDefines, Pas2PhpFPList,
  Pas2PhpImplementation, Pas2PhpTranslate, Pas2PhpUtils, PasTree, PParser,
  PScanner, StrUtils, SysUtils;

type

  TPas2Php = class(TPas2PhpSection)
  strict private
    FIncludePaths: array of TFileName;
  protected
    procedure IncludeUnit(const APasUnresolvedUnitRef: TPasUnresolvedUnitRef);
    procedure IncludeSectionUsesList(const APasSection: TPasSection);
    function ParseSourceFilename(ASourceFilename: TFileName): TPasModule;
    function FileNameResolve(const AFileName: TFileName): TFileName;
  public
    procedure AddUnitPath(AFilePath: TFileName);
    procedure AddUnitPaths(const ABasePath: TFileName; const AFilePaths: array of TFileName);
    procedure TranslatePackage(const AFileName: TFileName);
  end;

procedure Pas2PhpTranspile(const APascalFile, APhpConfigFile: TFileName;
  const AIncludePaths: array of TFileName; const AURL: string;
  const AOptions: TPas2PhpOptions = [(*p2pIgnoreFileAge,*)p2pNoElementListTitles]);
procedure Pas2PhpTranspile(AFileName, AServerPathLocal, AServerPathRemote: string);

implementation

procedure Pas2PhpTranspile(const APascalFile, APhpConfigFile: TFileName;
  const AIncludePaths: array of TFileName; const AURL: string; const AOptions: TPas2PhpOptions);
begin
  try
    with TPas2Php.Create(AOptions) do begin
      try
        ConfigFileName := APhpConfigFile;
        AddUnitPath(ExtractFilePath(APascalFile));
        AddUnitPaths(ExtractFilePath(APascalFile), AIncludePaths);
        TranslatePackage(APascalFile);
      finally
        Free;
      end;
    end;
    OpenUrl(AURL);
  except
    on LException: Exception do begin
      WriteLn;
      WriteLn(LException.Message);
      ReadLn;
    end;
  end;
end;

procedure Pas2PhpTranspile(AFileName, AServerPathLocal, AServerPathRemote: string);
begin
  AServerPathLocal := CleanAndExpandDirectory(AServerPathLocal);
  EnforceDirectoryExists(AServerPathLocal);

  AFileName := CleanAndExpandFileName(AFileName);
  EnforceFileExists(AFileName);

  Pas2PhpTranspile(AFileName, CreateRelativePath(AServerPathLocal +
    OFileName.Config, ExtractFilePath(AFileName)), [AServerPathLocal + OFileName.Include],
    ReplaceStr(AServerPathRemote + CreateRelativePath(ChangeFileExt(AFileName, OFileName.ExtPhp),
    AServerPathLocal), '\', '/'));
end;

procedure TPas2Php.AddUnitPath(AFilePath: TFileName);
var
  LFilePath: TFileName;
begin
  Enforce(FilenameIsAbsolute(AFilePath), 'Path must be absolute', AFilePath);
  AFilePath := CleanAndExpandDirectory(AFilePath);
  Enforce(DirectoryExists(AFilePath), 'Path does not exist', AFilePath);
  for LFilePath in FIncludePaths do begin
    if SameFilename(AFilePath, LFilePath) then begin
      Exit;
    end;
  end;
  SetLength(FIncludePaths, Length(FIncludePaths) + 1);
  FIncludePaths[High(FIncludePaths)] := AFilePath;
end;

procedure TPas2Php.AddUnitPaths(const ABasePath: TFileName; const AFilePaths: array of TFileName);
var
  LFilePath: TFileName;
begin
  for LFilePath in AFilePaths do begin
    AddUnitPath(CleanAndExpandFilename(ExtractRelativepath(ABasePath, LFilePath)));
  end;
end;

procedure TPas2Php.IncludeUnit(const APasUnresolvedUnitRef: TPasUnresolvedUnitRef);
var
  LFileName, LFilePath, LFileExt: TFileName;
begin
  Assert(Assigned(APasUnresolvedUnitRef));
  try
    Package.Modules._FindElementByName(UnitNameTranslate(APasUnresolvedUnitRef.Name));
  except
    if APasUnresolvedUnitRef.FileName = EmptyStr then begin
      for LFilePath in FIncludePaths do begin
        for LFileExt in PascalFileExt do begin
          LFileName := LFilePath + LowerCase(APasUnresolvedUnitRef.Name) + LFileExt;
          if FileExists(LFileName) then begin
            APasUnresolvedUnitRef.Filename := LFileName;
            ParseSourceFilename(LFileName);
            Exit;
          end;
          LFileName := LFilePath + OPasLang.UnitNamePrefix +
            LowerCase(APasUnresolvedUnitRef.Name) + LFileExt;
          if FileExists(LFileName) then begin
            APasUnresolvedUnitRef.Filename := LFileName;
            ParseSourceFilename(LFileName);
            Exit;
          end;
        end;
      end;
      for LFilePath in FIncludePaths do begin
        if FileExists(LFilePath + APasUnresolvedUnitRef.Name + OFileName.ExtPhp) then begin
          Exit;
        end;
      end;
      RaiseException('Unabled to locate unit.', APasUnresolvedUnitRef.Name);
    end;
  end;
end;

procedure TPas2Php.IncludeSectionUsesList(const APasSection: TPasSection);
var
  LIndex: integer;
begin
  if Assigned(APasSection) then begin
    for LIndex := 0 to APasSection.UsesList.High do begin
      IncludeUnit(APasSection.UsesList[LIndex] as TPasUnresolvedUnitRef);
    end;
  end;
end;

function TPas2Php.FileNameResolve(const AFileName: TFileName): TFileName;
var
  LFilePath: TFileName;
begin
  Result := CleanAndExpandFilename(AFileName);
  if FileExists(Result) then begin
    Exit;
  end else begin
    for LFilePath in FIncludePaths do begin
      Result := CleanAndExpandFilename(LFilePath + AFileName);
      if FileExists(Result) then begin
        Exit;
      end;
    end;
  end;
  RaiseException('Unable to locate source file.', AFileName);
end;

function TPas2Php.ParseSourceFilename(ASourceFilename: TFileName): TPasModule;
var
  LIndex: integer;
  LFilePath: TFileName;
  LFileResolver: TFileResolver;
  LScanner: TPascalScanner;
  LParser: TPasParser;
begin
  ASourceFilename := FileNameResolve(ASourceFilename);
  for LIndex := 0 to Package.Modules.High do begin
    Enforce(not SameFilename(ASourceFilename, (Package.Modules[LIndex] as
      TPasModule).SourceFilename),
      'File has already been added.', ASourceFilename);
  end;
  LogMessage('Parsing: "' + ASourceFilename + '".');
  Result := nil;
  LFileResolver := TFileResolver.Create;
  try
    for LFilePath in FIncludePaths do begin
      LFileResolver.AddIncludePath(LFilePath);
    end;
    LScanner := TPascalScanner.Create(LFileResolver);
    try
      LScanner.AddDefine(GPas2PhpDefine);
      LScanner.LogEvents := ScannerLogEvents;
      LScanner.OnLog := Onlog;
      LParser := TPasParser.Create(LScanner, LFileResolver, Self);
      try
        LParser.OnLog := Onlog;
        LScanner.OpenFile(ASourceFilename);
        LParser.ParseMain(Result);
        Result.Name := UnitNameTranslate(Result.Name);
        Result.Filename := ExtractFilePath(ASourceFilename) +
          ChangeFileExt(UnitNameTranslate(ExtractFileName(ASourceFilename)), OFileName.ExtPhp);
        if Result.InterfaceSection.UsesList.Count = 0 then begin
          Result.InterfaceSection.UsesList.Add(TPasUnresolvedUnitRef.Create(OPasLang.SSystem,
            Result.InterfaceSection));
        end;
      finally
        FreeAndNil(LParser);
      end;
    finally
      FreeAndNil(LScanner);
    end;
  finally
    FreeAndNil(LFileResolver);
  end;
  IncludeSectionUsesList(Result.InterfaceSection);
  IncludeSectionUsesList(Result.ImplementationSection);
end;

procedure TPas2Php.TranslatePackage(const AFileName: TFileName);
var
  LIndex, LModuleCount: integer;
  LFilePath: TFileName;
  LStream: TStream;
  LModule: TPasModule;
begin
  Assert(FilenameIsAbsolute(AFileName));

  LogMessage('Module Search Paths:');
  for LFilePath in FIncludePaths do begin
    LogMessage(LFilePath);
  end;
  LogMessage;
  //ParseSourceFilename(OPasLang.RTL);
  ParseSourceFilename(AFileName);

  LogMessage;
  LogMessage('Package Translating:');
  LogMessage;
  LModuleCount := 0;
  for LIndex := 0 to Package.Modules.Count - 1 do begin
    LModule := TObject(Package.Modules.Items[LIndex]) as TPasModule;
    if (p2pIgnoreFileAge in Options) or (FileAge(LModule.SourceFilename) >=
      FileAge(LModule.Filename)) then begin
      LModuleCount += 1;
      LogMessage('Module: ' + QuotedStr(LModule.Name));
      LogMessage('Source: ' + QuotedStr(LModule.SourceFilename));
      LogMessage('Output: ' + QuotedStr(LModule.Filename));
      LogMessage;
      try
        LStream := TFileStream.Create(LModule.Filename, fmCreate);
        try
          PasModule(LModule, LStream, SameFileName(LModule.SourceFilename, AFileName));
        finally
          FreeAndNil(LStream);
        end;
      except
        DeleteFile(LModule.Filename);
        raise;
      end;
    end else begin
      //LogMessage('The unit ' + QuotedStr(LModule.Name) + ' is up to date. (skipped)');
    end;
  end;
  if LModuleCount = 0 then begin
    LogMessage('No modules need translating.');
  end else begin
    LogMessage(IntToStr(LModuleCount) + ' module(s) translated.');
  end;
  LogMessage;
  LogMessage('Package translation complete.');
end;

end.
