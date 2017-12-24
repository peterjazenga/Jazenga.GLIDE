{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextcore;

{$warn 5023 off : no warning about unused units}
interface

uses
  unite_messages, fonctions_proprietes, fonctions_string, fonctions_system, 
  fonctions_variant, fonctions_db, fonctions_array, fonctions_objects, 
  fonctions_dbobjects, fonctions_file, fonctions_ini, fonctions_filepath, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextcore', @Register);
end.
