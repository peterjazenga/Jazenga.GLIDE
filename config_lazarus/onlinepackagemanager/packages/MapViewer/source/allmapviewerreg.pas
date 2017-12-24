unit allmapviewerreg;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, TypInfo,
 mvmapviewer,
 mvglgeonames,
 mvdatadrawgrid;

 procedure Register;

implementation

{$R allmapviewerreg.res}

procedure Register;
begin

  RegisterComponents ('GeoGIS',[
                                 TMVMapViewer,
                                 TMVGLGeoNames,
                                 TMVDataDrawGrid
                               ]);

end;

end.


