{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit FileMenuHandler; 

interface

uses
  FileMenuHdl, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FileMenuHdl', @FileMenuHdl.Register); 
end; 

initialization
  RegisterPackage('FileMenuHandler', @Register); 
end.
