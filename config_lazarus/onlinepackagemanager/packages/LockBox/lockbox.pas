{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lockbox;

{$warn 5023 off : no warning about unused units}
interface

uses
  alllockboxreg, LbAsym, LbBigInt, LbCipher, LbClass, LbConst, LbDESPEM, 
  LbDSA, LbKeyEd1, LbKeyEd2, LbProc, LbRandom, LbRSA, LbString, LbUtils, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('alllockboxreg', @alllockboxreg.Register);
end;

initialization
  RegisterPackage('lockbox', @Register);
end.
