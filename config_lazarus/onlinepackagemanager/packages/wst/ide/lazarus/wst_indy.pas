{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
  '
  +'Cette source est seulement employée pour compiler et installer le '
  +'paquet.
 }

unit wst_indy;

interface

uses
  indy_http_protocol, indy_http_server, indy_tcp_protocol, indy_tcp_server, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('wst_indy', @Register); 
end.
