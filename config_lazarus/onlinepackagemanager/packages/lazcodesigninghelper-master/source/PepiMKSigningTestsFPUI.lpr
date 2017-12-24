{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Code test for codesigning Lazarus IDE plugin files.)
   @codesigning-url(http://ccrdude.net/LazCodeSigningHelper/)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-17  --  ---  Renamed from ram PepiMKSigningTestsFPUI to PepiMKSigningTestsFPUI
// 2017-05-17  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

program PepiMKSigningTestsFPUI;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

uses
   Interfaces,
   Forms,
   GuiTestRunner,
   fpcunittestrunner,
   PepiMK.Signing.GnuPG.TestCase,
   PepiMK.Signing.AppleCodeSign,
   PepiMK.Signing.MicrosoftSignTool,
   PepiMK.Signing.MicrosoftSignTool.TestCase;

{$R *.res}

begin
   Application.Initialize;
   Application.CreateForm(TGuiTestRunner, TestRunner);
   Application.Run;
end.
