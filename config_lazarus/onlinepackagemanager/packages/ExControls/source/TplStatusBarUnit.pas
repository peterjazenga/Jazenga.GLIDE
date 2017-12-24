
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplStatusBarUnit;

interface

{$I AllExControlsRegister.inc}

uses
  Classes, Controls, Messages, SysUtils, Forms, ComCtrls,
  plUtils;

type

  TplStatusBar = class(TStatusBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TplStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

end.
