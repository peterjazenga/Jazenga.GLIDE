{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit wst_synapse; 

interface

uses
  synapse_tcp_server, synapse_http_protocol, synapse_tcp_protocol, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('wst_synapse', @Register); 
end.
