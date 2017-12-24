{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextcore;

interface

uses
  unite_messages, fonctions_proprietes, fonctions_string, fonctions_system, 
  fonctions_variant, fonctions_db, fonctions_array, fonctions_objects, 
  fonctions_dbobjects, fonctions_file, fonctions_ini, fonctions_dialogs, 
  fonctions_filepath, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextcore', @Register);
end.
