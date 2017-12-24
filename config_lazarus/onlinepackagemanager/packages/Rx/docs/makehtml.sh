#!/bin/bash
#надо скопировать rx.inc в текущий каталог, иначе не соберём (глюк fpdoc)
cp ../rx.inc rx.inc 

echo Write help files for RxFPC
fpdoc --package=rxfpc --format=html --index-colcount=4 --hide-protected \
  --input=../rxaboutdialog.pas --descr=rxaboutdialog.xml \
  --input=../rxaboutformunit.pas --descr=rxfpc.xml \
  --input=../rxautopanel.pas --descr=rxautopanel.xml \
  --input=../rxboxprocs.pas --descr=rxboxprocs.xml \
  --input=../rxclock.pas --descr=rxclock.xml \
  --input=../rxcloseformvalidator.pas --descr=rxcloseformvalidator.xml \
  --input=../rxctrls.pas --descr=rxctrls.xml \
  --input=../rxcurredit.pas --descr=rxcurredit.xml \
  --input=../rxdateutil.pas --descr=rxdateutil.xml \
  --input=../rxdbcomb.pas --descr=rxdbcomb.xml \
  --input=../rxdbctrls.pas --descr=rxfpc.xml \
  --input=../rxdbdateedit.pas --descr=rxfpc.xml \
  --input=../rxdbgrid_columsunit.pas --descr=rxfpc.xml \
  --input=../rxdbgridexportpdf.pas --descr=rxdbgridexportpdf.xml \
  --input=../rxdbgridexportspreadsheet_paramsunit.pas --descr=rxfpc.xml \
  --input=../rxdbgridexportspreadsheet.pas --descr=rxfpc.xml \
  --input=../rxdbgrid_findunit.pas --descr=rxfpc.xml \
  --input=../rxdbgridfootertools.pas --descr=rxdbgridfootertools.xml \
  --input=../rxdbgridfootertools_setup.pas --descr=rxdbgridfootertools_setup.xml \
  --input=../rxdbgrid.pas --descr=rxdbgrid.xml \
  --input=../rxdbspinedit.pas --descr=rxfpc.xml \
  --input=../rxdbtimeedit.pas --descr=rxfpc.xml \
  --input=../rxdbutils.pas --descr=rxdbutils.xml \
  --input=../rxdice.pas --descr=rxdice.xml \
  --input=../rxduallist.pas --descr=rxduallist.xml \
  --input=../rxfileutils.pas --descr=rxfileutils.xml \
  --input=../rxfolderlister.pas --descr=rxfolderlister.xml \
  --input=../rxinipropstorage.pas --descr=rxinipropstorage.xml \
  --input=../rxlogin.pas --descr=rxlogin.xml \
  --input=../rxlookup.pas --descr=rxlookup.xml \
  --input=../rxmdi.pas --descr=rxmdi.xml \
  --input=../rxmemds.pas --descr=rxmemds.xml \
  --input=../rxpickdate.pas --descr=rxpickdate.xml \
  --input=../rxspin.pas --descr=rxfpc.xml \
  --input=../rxspin.pas --descr=rxspin.xml \
  --input=../rxswitch.pas --descr=rxswitch.xml \
  --input=../rxtimeedit.pas --descr=rxtimeedit.xml \
  --input=../rxtoolbar.pas --descr=rxtoolbar.xml \
  --input=../rxtooledit.pas --descr=rxtooledit.xml \
  --input=../rxvclutils.pas --descr=rxvclutils.xml \
  --input=../rxversinfo.pas --descr=rxversinfo.xml \
  --input=../rxviewspanel.pas --descr=rxviewspanel.xml 

echo 
echo Write help files for RxDBGrid_Print
fpdoc --package=rxdbgrid_print --format=html --index-colcount=4 --hide-protected \
  --input=../rxdbgridprintgrid.pas --descr=rxdbgridprintgrid.xml

echo 
echo Write help files for rxdbgrid_export_spreadsheet
fpdoc --package=rxdbgrid_export_spreadsheet --format=html --index-colcount=4 --hide-protected \
  --input=../rxdbgridexportspreadsheet.pas --descr=rxdbgridexportspreadsheet.xml \ 
  --input=../rxdbgridexportspreadsheet_paramsunit.pas --descr=rxdbgridexportspreadsheet_paramsunit.xml

echo   
echo Write help files for rx_sort_zeos
fpdoc --package=rx_sort_zeos --format=html --index-colcount=4 --hide-protected \
  --input=../exsortzeos.pas --descr=exsortzeos.xml \
  --input=../rxsortzeos.pas --descr=rxsortzeos.xml
  