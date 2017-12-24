{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit astronomy;

{$warn 5023 off : no warning about unused units}
interface

uses
  allastronomyreg, MoonAPI, mooncomp, MoonCompEditors, MoonMath, MoonVSOP, 
  solarsystems, StAstro, StAstroPlanetary, StBase, StConst, StDate, StDateSt, 
  StEclpse, StJup, StJupSat, StList, StMars, StMath, StMerc, StNeptun, 
  StPluto, StSaturn, StStrL, StStrS, StUranus, StUtils, StVenus, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('allastronomyreg', @allastronomyreg.Register);
end;

initialization
  RegisterPackage('astronomy', @Register);
end.
