{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit scrolltext;

interface

uses
  ScrollingText, AboutScrolltextunit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ScrollingText', @ScrollingText.Register);
  RegisterUnit('AboutScrolltextunit', @AboutScrolltextunit.Register);
end;

initialization
  RegisterPackage('scrolltext', @Register);
end.
