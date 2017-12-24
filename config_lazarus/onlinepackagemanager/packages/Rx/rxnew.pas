{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxnew;

{$warn 5023 off : no warning about unused units}
interface

uses
  registerrx, RegisterRxDB, RegisterRxTools, RxHistoryNavigator, 
  RxAboutDialog, rxAboutFormUnit, rxclock, RxCloseFormValidator, rxapputils, 
  RxAutoPanel, rxboxprocs, rxctrls, rxcurredit, rxDateRangeEditUnit, 
  rxvclutils, RxVersInfo, RxViewsPanel, ex_rx_bin_datapacket, 
  ex_rx_datapacket, ex_rx_xml_datapacket, RxDBColorBox, rxdbcomb, RxDBCtrls, 
  rxdbcurredit, rxdbdateedit, rxdbgrid, rxdbgrid_columsunit, 
  rxdbgrid_findunit, exsortmds, RxDBGridExportPdf, RxDBGridExportPdfSetupUnit, 
  RxDBGridFooterTools, rxdbgridfootertools_setup, RxDBSpinEdit, RxDBTimeEdit, 
  rxdbutils, rxfilterby, rxlookup, rxmemds, rxpopupunit, rxseldsfrm, rxsortby, 
  rxsortmemds, rxdice, rxduallist, rxfduallst, rxfolderlister, RXHistory, 
  RxIniPropStorage, rxlclconst, rxlogin, RxMDI, rxpagemngr, rxpickdate, 
  rxShortCutUnit, rxspin, rxswitch, RxSystemServices, rxtbrsetup, RxTimeEdit, 
  rxtoolbar, rxtooledit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerrx', @registerrx.Register);
  RegisterUnit('RegisterRxDB', @RegisterRxDB.Register);
  RegisterUnit('RegisterRxTools', @RegisterRxTools.Register);
end;

initialization
  RegisterPackage('rxnew', @Register);
end.
