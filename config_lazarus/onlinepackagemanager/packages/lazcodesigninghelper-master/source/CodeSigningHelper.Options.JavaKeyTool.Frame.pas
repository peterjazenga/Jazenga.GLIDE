{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(TODO : please fill in abstract here!)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-22  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Options.JavaKeyTool.Frame;

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
   StdCtrls,
   ExtCtrls,
   EditBtn,
   IDEOptionsIntf;

type

   { TTFrameCodeSigningOptionsJavaKeyTool }

   TTFrameCodeSigningOptionsJavaKeyTool = class(TAbstractIDEOptionsEditor)
   private

   public
      procedure ApplyLocalizedTexts();
      function GetTitle: string; override;
      procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
      procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
      procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
      class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
   end;

implementation

{$R *.lfm}

uses
   CodeSigningHelper.Options,
   CodeSigningHelper.Strings;

{ TTFrameCodeSigningOptionsJavaKeyTool }

procedure TTFrameCodeSigningOptionsJavaKeyTool.ApplyLocalizedTexts;
begin

end;

function TTFrameCodeSigningOptionsJavaKeyTool.GetTitle: string;
begin
   Result := rsCodeSigningOptionsTitleJavaKeyTool;
end;

procedure TTFrameCodeSigningOptionsJavaKeyTool.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
   ApplyLocalizedTexts();
end;

procedure TTFrameCodeSigningOptionsJavaKeyTool.ReadSettings(AOptions: TAbstractIDEOptions);
begin

end;

procedure TTFrameCodeSigningOptionsJavaKeyTool.WriteSettings(AOptions: TAbstractIDEOptions);
begin

end;

class function TTFrameCodeSigningOptionsJavaKeyTool.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
   Result := TCodeSigningOptions;
end;

initialization
   RegisterIDEOptionsEditor(CodeSigningOptionGroup, TTFrameCodeSigningOptionsJavaKeyTool, 3, NoParent, True);
end.
