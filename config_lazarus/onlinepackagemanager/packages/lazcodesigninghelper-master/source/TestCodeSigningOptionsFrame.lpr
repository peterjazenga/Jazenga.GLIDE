{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Test program showing the options frame.)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-11  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

program TestCodeSigningOptionsFrame;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
   cthreads, {$ENDIF} {$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms,
   TestCodeSigningOptionsFrame.Form.Main, CodeSigningHelper.Debug;

{$R *.res}

begin
   RequireDerivedFormResource := True;
   Application.Initialize;
   Application.CreateForm(TFormOptionsFrameTestContainer, FormOptionsFrameTestContainer);
   Application.Run;
end.
