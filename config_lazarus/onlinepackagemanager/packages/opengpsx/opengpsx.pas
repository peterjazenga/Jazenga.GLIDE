{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit opengpsx; 

interface

uses
    geocompute, geoellipsoids, gpsutilities, gpssignalplot, gpsskyplot, 
  gpstarget, gpsdatadef, nmeadecode, nmea_gpvtg, nmea_gpgga, nmea_gpgll, 
  nmea_gpgsa, nmea_gpgsv, nmea_gprmc, nmea_gpwpl, nmea_gpbod, nmea_gprte, 
  gpsportconnected, nmea_gpgarmin, savetracktogpx, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('gpssignalplot', @gpssignalplot.Register); 
  RegisterUnit('gpsskyplot', @gpsskyplot.Register); 
  RegisterUnit('gpstarget', @gpstarget.Register); 
  RegisterUnit('nmeadecode', @nmeadecode.Register); 
  RegisterUnit('gpsportconnected', @gpsportconnected.Register); 
end; 

initialization
  RegisterPackage('opengpsx', @Register); 
end.
