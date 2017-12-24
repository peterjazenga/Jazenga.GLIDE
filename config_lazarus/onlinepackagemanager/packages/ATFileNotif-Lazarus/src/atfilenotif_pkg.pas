{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atfilenotif_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  ATFileNotif, ATFileNotif_Reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ATFileNotif_Reg', @ATFileNotif_Reg.Register);
end;

initialization
  RegisterPackage('atfilenotif_pkg', @Register);
end.
