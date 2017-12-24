{**********************************************************************
                PilotLogic Software House.
                   
 Package pl_Barcode.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit allbarcodesreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LResources, Graphics,
  ubasic,
  uqr,
  zint,
  burender,
  uaztec,
  udatamatrix,
  ubarcodes,
  StBarC,
  StBarPN,
  St2DBarC,
  StDbBarC,
  StDbPNBC,
  StDb2DBC,
  udbbarcodeqr,
  udbbarcodemicroqr,
  udbbarcodeaztec,
  udbbarcodeaztecrune,
  udbbarcodedatamatrix;


procedure Register;

implementation

  {$R allbarcodesreg.res}

procedure Register;
begin

  RegisterComponents('Barcodes',[
                                    TStBarCode,
                                    TStPNBarCode,
                                    TStPDF417Barcode,
                                    TStMaxiCodeBarcode,

                                    TBarcodeQR,
                                    TBarcodeMicroQR,
                                    TBarcodeAztec,
                                    TBarcodeAztecRune,
                                    TBarcodeDataMatrix,

                              //---------------------- DB
                                    TStDbBarCode,
                                    TStDbPNBarCode,
                                    TStDbPDF417Barcode,
                                    TStDbMaxiCodeBarcode,

                                    TdbBarcodeQR,
                                    TdbBarcodeMicroQR,
                                    TdbBarcodeAztec,
                                    TdbBarcodeAztecRune,
                                    TdbBarcodeDataMatrix
                                    ]);

end;

end.

