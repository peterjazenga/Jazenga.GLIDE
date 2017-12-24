{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit kaaj_controls;

interface

uses
  kaaj_button, kaaj_combobox, kaaj_edit, kaaj_panel, kaaj_progressbar, 
  kaaj_theme, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('kaaj_controls', @Register);
end.
