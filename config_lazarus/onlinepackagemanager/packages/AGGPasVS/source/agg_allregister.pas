{**********************************************************************
                PilotLogic Software House.
                   
 Package pl_AGGPasVS.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com)
***********************************************************************}

unit agg_AllRegister;

interface

{$I agg_vsmode.inc }

uses
  Classes, TypInfo,
  LCLIntf, LResources, LazIDEIntf, PropEdits, ComponentEditors;

procedure Register;

implementation

  {$R agg_allregister.res}

uses
  agg_Color,
  agg_lclpaintbox,
  agx_lclpaintbox,
  agg_lclcontrols;

{ Registration }
procedure Register;
begin

  RegisterComponents('AggPas', [
                                TAggLabel,
                               // TAggCheckBox,
                               // TAggRadioBox,
                               // TAggSlider,
                                TAggPaintBox,
                                TAgxPaintBox]);
end;


end.
