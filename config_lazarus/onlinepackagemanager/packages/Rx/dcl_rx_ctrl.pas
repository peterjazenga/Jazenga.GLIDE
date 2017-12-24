{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dcl_rx_ctrl;

{$warn 5023 off : no warning about unused units}
interface

uses
  register_rxctrl, rxceEditLookupFields, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('register_rxctrl', @register_rxctrl.Register);
end;

initialization
  RegisterPackage('dcl_rx_ctrl', @Register);
end.
