{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_fpspreadsheet_visual;

interface

uses
  fpsActions, fpspreadsheetchart, fpspreadsheetctrls, fpspreadsheetgrid, 
  fpsvisualutils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit( 'fpsActions' , @fpsActions.Register);
  RegisterUnit( 'fpspreadsheetchart' , @fpspreadsheetchart.Register);
  RegisterUnit( 'fpspreadsheetctrls' , @fpspreadsheetctrls.Register);
  RegisterUnit( 'fpspreadsheetgrid' , @fpspreadsheetgrid.Register);
end;

initialization
  RegisterPackage( 'laz_fpspreadsheet_visual' , @Register);
end.
