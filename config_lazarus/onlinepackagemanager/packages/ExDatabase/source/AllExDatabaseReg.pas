{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ExDatabase.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllExDatabaseReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LResources, PropEdits, DBPropEdits,
  TplDBTreeviewUnit;

procedure Register;

implementation

 {$R AllExDatabaseReg.res}

procedure Register;
begin
  RegisterComponents('Extra Database', [TplDBTreeView]);

  RegisterPropertyEditor(TypeInfo(string), TplDBTreeView, 'DBTextField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TplDBTreeView, 'DBKeyField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TplDBTreeView, 'DBParentField', TFieldProperty);
end;

end.

