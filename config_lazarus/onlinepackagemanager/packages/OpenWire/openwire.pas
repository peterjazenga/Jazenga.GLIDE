{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit openwire;

{$warn 5023 off : no warning about unused units}
interface

uses
  allOpenWireReg, OpenWirePinEditors, OWAboutFormUnit, 
  OWAfterPinSelectFormUnit, OWClassProperty, OWDesignSelectionsList, 
  OWDesignTypes, OWExtCollection, OWLComps, OWLStateComps, OWPins, 
  OWStateEditors, OWStdTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('allOpenWireReg', @allOpenWireReg.Register);
end;

initialization
  RegisterPackage('openwire', @Register);
end.
