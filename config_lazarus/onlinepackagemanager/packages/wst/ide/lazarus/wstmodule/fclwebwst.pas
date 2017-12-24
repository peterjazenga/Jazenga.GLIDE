{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit fclwebwst; 

interface

uses
  regwstmodule, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('regwstmodule', @regwstmodule.Register); 
end; 

initialization
  RegisterPackage('fclwebwst', @Register); 
end.
