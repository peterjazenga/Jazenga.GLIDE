{ Dit bestand is automatisch aangemaakt door Lazarus. Niet wijzigen!
  Deze broncode is alleen gebruikt voor compilatie en installatie.
 }

unit plot; 

interface

uses
  Plotpanel, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('Plotpanel', @Plotpanel.Register); 
end; 

initialization
  RegisterPackage('plot', @Register); 
end.
