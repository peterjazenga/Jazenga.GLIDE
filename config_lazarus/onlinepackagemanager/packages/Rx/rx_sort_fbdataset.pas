{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rx_sort_fbdataset;

{$warn 5023 off : no warning about unused units}
interface

uses
  exsortfb, RxSortFBDataSet, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxSortFBDataSet', @RxSortFBDataSet.Register);
end;

initialization
  RegisterPackage('rx_sort_fbdataset', @Register);
end.
