{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FileAssociation;

interface

uses
  FileAssoc, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FileAssoc', @FileAssoc.Register);
end;

initialization
  RegisterPackage('FileAssociation', @Register);
end.
