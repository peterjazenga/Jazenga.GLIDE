{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxdbgrid_export_spreadsheet;

{$warn 5023 off : no warning about unused units}
interface

uses
  RxDBGridExportSpreadSheet, RxDBGridExportSpreadSheet_ParamsUnit, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxDBGridExportSpreadSheet', @RxDBGridExportSpreadSheet.Register
    );
end;

initialization
  RegisterPackage('rxdbgrid_export_spreadsheet', @Register);
end.
