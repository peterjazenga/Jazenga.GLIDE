{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ExCompress
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllExCompressRegister;

interface


 uses
  Classes, SysUtils, TypInfo,lresources, PropEdits, ComponentEditors,

  TplZlibUnit,
  TplLzmaUnit;

procedure Register;

implementation

{$R AllExCompressRegister.res}

procedure Register;
begin

  RegisterComponents ('Extra Compress',[
                                       TplZLibCompress,
                                       TplZLibUnCompress,
                                       TplLzmaCompress,
                                       TplLzmaUNCompress
                                       ]);

end;

end.
