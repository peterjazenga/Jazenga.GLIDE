{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
  Cette source est seulement employée pour compiler et installer le paquet.
 }

unit ExpertSuperForm; 

interface

uses
  CompSuperForm, CompSuperFormReg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('CompSuperFormReg', @CompSuperFormReg.Register); 
end; 

initialization
  RegisterPackage('ExpertSuperForm', @Register); 
end.
