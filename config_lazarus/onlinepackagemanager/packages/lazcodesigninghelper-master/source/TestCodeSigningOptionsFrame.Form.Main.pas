{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Wrapper to show options frame.)

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

unit TestCodeSigningOptionsFrame.Form.Main;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   FileUtil,
   Forms,
   Controls,
   Graphics,
   Dialogs,
   ComCtrls,
   CodeSigningHelper.Options.AppleCodeSign.Frame,
   CodeSigningHelper.Options.GnuPG.Frame,
   CodeSigningHelper.Options.JavaKeyTool.Frame,
   CodeSigningHelper.Options.MicrosoftSignTool.Frame,
   CodeSigningHelper.ProjectOptions.AppleCodeSign.Frame,
   CodeSigningHelper.ProjectOptions.GnuPG.Frame,
   CodeSigningHelper.ProjectOptions.MicrosoftSignTool.Frame;

type

   { TFormOptionsFrameTestContainer }

   TFormOptionsFrameTestContainer = class(TForm)
      FrameCodeSigningOptionsMicrosoftSignTool1: TFrameCodeSigningOptionsMicrosoftSignTool;
      FrameCodeSigningProjectOptionsMicrosoftSignTool1: TFrameCodeSigningProjectOptionsMicrosoftSignTool;
      FrameCodeSigningOptionsGnuPG1: TFrameCodeSigningOptionsGnuPG;
      pcMain: TPageControl;
      tabMicrosoftSignTool: TTabSheet;
      tabGnuPG: TTabSheet;
      tabProjectMicrosoftSignTool: TTabSheet;
      procedure FormCreate({%H-}Sender: TObject);
   private
      { private declarations }
   public
      { public declarations }
   end;

var
   FormOptionsFrameTestContainer: TFormOptionsFrameTestContainer;

implementation

{$R *.lfm}

{ TFormOptionsFrameTestContainer }

procedure TFormOptionsFrameTestContainer.FormCreate(Sender: TObject);
begin
   FrameCodeSigningOptionsMicrosoftSignTool1.ApplyLocalizedTexts();
end;

end.
