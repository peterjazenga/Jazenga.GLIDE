{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit ringwatch; 

interface

uses
  RingChart, AnalogWatch, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RingChart', @RingChart.Register); 
  RegisterUnit('AnalogWatch', @AnalogWatch.Register); 
end; 

initialization
  RegisterPackage('ringwatch', @Register); 
end.
