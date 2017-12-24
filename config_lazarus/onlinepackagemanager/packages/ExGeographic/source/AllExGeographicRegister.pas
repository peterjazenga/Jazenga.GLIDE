{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ExGeographic
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllExGeographicRegister;

interface


 uses
  Classes, SysUtils, TypInfo,lresources, PropEdits, ComponentEditors,
  TplVectorFlagUnit;

procedure Register;

implementation

{$R AllExGeographicRegister.res}

procedure Register;
begin

  RegisterComponents ('Extra Geographic',[
                                          TplVectorFlag
                                          ]);

end;

end.
