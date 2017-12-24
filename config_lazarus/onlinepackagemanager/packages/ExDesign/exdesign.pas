{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit exdesign;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExDesignRegister, ELDsgxClip, ELDsgxConsts, ELDsgxImp, ELDsgxObjectInsp, 
  ELDsgxPropStore, ELDsgxResources, ELDsgxSurface, ELDsgxTypes, ELDsgxUtils, 
  ELMDIForm, ELMDIFormPanel, ELSizeControl, OMultiPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExDesignRegister', @AllExDesignRegister.Register);
end;

initialization
  RegisterPackage('exdesign', @Register);
end.
