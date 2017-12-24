
{**********************************************************************
 Package pl_OpenWire
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit allOpenWireReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ComponentEditors, PropEdits,
  OWPins,
  OWLComps,
  OWLStateComps,
  OpenWirePinEditors;

procedure Register;

implementation
{$R allOpenWireReg.res}

procedure Register;
begin
  RegisterComponents('OpenWire', [TOWLAdd,
                                  TOWLMultiply,
                                  TOWLDivide,
                                  TOWLTestClock,
                                  TOWLLabel,
                                  TOWLTrackBar,
                                  TOWLProgressBar
                                  ]);

  RegisterPropertyEditor( typeinfo(TOWSourcePin),     NIL, '', TOWSourcePinPropertyEditor);
  RegisterPropertyEditor( typeinfo(TOWMultiSinkPin),  NIL, '', TOWEventSinkPinPropertyEditor);
  RegisterPropertyEditor( typeinfo(TOWSinkPin),       NIL, '', TOWSinkPinPropertyEditor);
  RegisterPropertyEditor( typeinfo(TOWStatePin),      NIL, '', TOWStatePinPropertyEditor);
  RegisterPropertyEditor( typeinfo(TOWPinList),       NIL, '', TOWPinListPropertyEditor);
  RegisterPropertyEditor( typeinfo(TOWPinListOwner),  NIL, '', TOWPinListOwnerPropertyEditor);
end;

end.

