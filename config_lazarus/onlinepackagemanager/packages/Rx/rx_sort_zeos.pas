{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rx_sort_zeos;

{$warn 5023 off : no warning about unused units}
interface

uses
  exsortzeos, RxSortZeos, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxSortZeos', @RxSortZeos.Register);
end;

initialization
  RegisterPackage('rx_sort_zeos', @Register);
end.
