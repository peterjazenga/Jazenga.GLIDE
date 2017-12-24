{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Test case for the codesigning implementation for GnuPG.)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-17  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit PepiMK.Signing.GnuPG.TestCase;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   fpcunit,
   testutils,
   testregistry,
   PepiMK.Signing.GnuPG;

type

   { TTestCaseSigningGnuPG }

   TTestCaseSigningGnuPG = class(TTestCase)
   published
      procedure TestSigning;
   end;

implementation

uses
   Windows,
   FileUtil;

procedure TTestCaseSigningGnuPG.TestSigning;
var
   sFilenameIn: string;
   sFilenameSig: string;
   s: TGnuPGSigner;
begin
   AllocConsole;
   sFilenameIn := ParamStr(0) + '.test.exe';
   sFilenameSig := sFilenameIn + '.sig';
   if FileExists(sFilenameIn) then begin
      CheckTrue(SysUtils.DeleteFile(sFilenameIn), 'DeleteFile(testfile: ' + sFilenameIn + ')');
   end;
   if FileExists(sFilenameSig) then begin
      CheckTrue(SysUtils.DeleteFile(sFilenameSig), 'DeleteFile(sigfile: ' + sFilenameSig + ')');
   end;
   CheckTrue(CopyFile(ParamStr(0), sFilenameIn));
   s := TGnuPGSigner.Create;
   try
      CheckTrue(s.SignFile(UTF8Decode(sFilenameIn)), 'TGPGSigner.SignFile');
      WriteLn('------------------------------------------------------');
      WriteLn(s.Outcome.CommandLine);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Output.Text);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Errors.Text);
      CheckTrue(s.VerifyFile(UTF8Decode(sFilenameIn)));
      WriteLn('------------------------------------------------------');
      WriteLn(s.Outcome.CommandLine);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Output.Text);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Errors.Text);
   finally
      s.Free;
   end;
end;


initialization

   RegisterTest(TTestCaseSigningGnuPG);
end.
