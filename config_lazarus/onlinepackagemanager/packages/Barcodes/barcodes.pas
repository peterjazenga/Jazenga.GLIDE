{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit barcodes;

{$warn 5023 off : no warning about unused units}
interface

uses
  allbarcodesreg, burender, St2DBarC, StBarC, StBarPN, StCommon, StDb2DBC, 
  StDbBarC, StDbPNBC, uaztec, ubarcodes, ubasic, udatamatrix, udbbarcodeaztec, 
  udbbarcodeaztecrune, udbbarcodedatamatrix, udbbarcodemicroqr, udbbarcodeqr, 
  uhelper, uqr, ureedsolomon, usjis, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('allbarcodesreg', @allbarcodesreg.Register);
end;

initialization
  RegisterPackage('barcodes', @Register);
end.
