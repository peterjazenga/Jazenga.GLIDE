{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DragDropLazarus;

interface

uses
  DragDropDesign, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DragDropDesign', @DragDropDesign.Register);
end;

initialization
  RegisterPackage('DragDropLazarus', @Register);
end.
