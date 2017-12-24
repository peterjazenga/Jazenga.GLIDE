{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mapviewer;

{$warn 5023 off : no warning about unused units}
interface

uses
  allmapviewerreg, mvdatadrawgrid, mvCache, mvDLESynapse, mvDownloadEngine, 
  mvdragobj, mvEngine, mvextradata, mvglgeonames, mvgpsobj, mvJobQueue, 
  mvJobs, mvMapProvider, mvmapviewer, mvtypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('allmapviewerreg', @allmapviewerreg.Register);
end;

initialization
  RegisterPackage('mapviewer', @Register);
end.
