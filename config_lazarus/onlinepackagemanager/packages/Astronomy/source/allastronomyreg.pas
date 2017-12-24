{**********************************************************************
                PilotLogic Software House.
                   
 Package pl_Astronomy 
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit allastronomyreg;

interface

uses
  classes,TypInfo,lresources,PropEdits,ComponentEditors,
  mooncomp,MoonCompEditors ;

procedure Register;

implementation

{$R allastronomyreg.res}

procedure Register;

begin
  RegisterPropertyEditor(TypeInfo(TDateTime),NIL,'',t_ah_datetimeproperty);
  RegisterComponents('Astronomy', [
                               TMoon
                                ]);

  end;


end.

