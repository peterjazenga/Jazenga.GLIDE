unit register_smnetgradient;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazarusPackageIntf, SMNetGradient,LResources;

procedure Register;

implementation

procedure RegisterUnitSMNetGradient;
begin
  RegisterComponents('SMACE', [TNetGradient]);
end;

procedure Register;
begin
  RegisterUnit('SMNetGradient', @RegisterUnitSMNetGradient);
end;

initialization
  {$I register_smnetgradient.lrs}

end.

