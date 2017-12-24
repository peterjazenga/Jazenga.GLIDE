{**********************************************************************
                PilotLogic Software House.
  
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllGraphics32ExtRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TypInfo,lresources,PropEdits,
  ComponentEditors,

  {$IFDEF WINDOWS}
    XGR32_Clock,
    XGR32_StdCtrls,
  {$ELSE}

  {$ENDIF}
  XGR32_ExtLayers_Panel,
  XGR32_DesktopControl;


procedure Register;

implementation

{$R AllGraphics32ExtRegister.res}

procedure Register;
begin

 RegisterComponents ('Graphics32 Extra',[
                {$IFDEF WINDOWS}
                                { TGRClock,
                                  TGRPanel,
                                  TGRLabel,
                                  TGRSpeedButton,
                                  TGRButton,
                                  TGRCheckBox,
                                  TGRRadioButton,
                                  TGRGroupBox, }
                {$ENDIF}
                                  TExLayers32Panel,
                                  TGRDesktopControl
                                   ]);



end;


end.

