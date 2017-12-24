{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Test case for the codesigning implementation for Microsofts signtool.exe.)

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

unit PepiMK.Signing.MicrosoftSignTool.TestCase;

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
   PepiMK.Signing.Base,
   PepiMK.Signing.MicrosoftSignTool;

type

   { TTestCaseSigningMicrosoftSignTool }

   TTestCaseSigningMicrosoftSignTool = class(TTestCase)
   published
      procedure TestSigning;
   end;

implementation

uses
   Windows,
   FileUtil;

{ TTestCaseSigningMicrosoftSignTool }

procedure TTestCaseSigningMicrosoftSignTool.TestSigning;
var
   sFilenameIn: string;
   s: TMicrosoftSignToolSigner;
   b: boolean;
begin
   AllocConsole;
   sFilenameIn := ParamStr(0) + '.test-signtool.exe';
   if FileExists(sFilenameIn) then begin
      CheckTrue(SysUtils.DeleteFile(sFilenameIn), 'DeleteFile(testfile: ' + sFilenameIn + ')');
   end;
   CheckTrue(CopyFile(ParamStr(0), sFilenameIn));
   s := TMicrosoftSignToolSigner.Create;
   s.Algorithms := [csaSHA1, csaSHA256];
   s.Certificate.Hash := '98bcd2ae677237ca1777bc0cb13b602e47badcc9';
   s.Certificate.Source := cscsStoreByHash;
   s.CrossSigning.Active := True;
   s.CrossSigning.Filename := 'g:\Tools\MSSDK\StartComCertificationAuthority.crt';
   s.Timestamping.Active := True;
   s.Timestamping.Method := tstRFC3161;
   s.Timestamping.ServerRFC3161 := 'http://tsa.startssl.com/rfc3161';
   try
      b := s.SignFile(UTF8Decode(sFilenameIn));
      WriteLn('------------------------------------------------------');
      WriteLn(s.Outcome.CommandLine);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Output.Text);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Errors.Text);
      CheckTrue(b, 'TMicrosoftSignToolSigner.SignFile');

      b := s.VerifyFile(UTF8Decode(sFilenameIn));
      WriteLn('------------------------------------------------------');
      WriteLn(s.Outcome.CommandLine);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Output.Text);
      WriteLn('......................................................');
      WriteLn(s.Outcome.Errors.Text);
      CheckTrue(b, 'TMicrosoftSignToolSigner.VerifyFile');
   finally
      s.Free;
   end;
end;

initialization

   RegisterTest(TTestCaseSigningMicrosoftSignTool);
end.
