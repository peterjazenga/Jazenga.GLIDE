{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit MPlayerControlLaz; 

interface

uses
  MPlayerCtrl, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MPlayerCtrl', @MPlayerCtrl.Register); 
end; 

initialization
  RegisterPackage('MPlayerControlLaz', @Register); 
end.
