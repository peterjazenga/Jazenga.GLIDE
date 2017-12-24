{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cindy;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllCindyRegister, cyAdvButton, cyAdvFlowPanel, cyAdvGridPanel, cyAdvLed, 
  cyAdvPaintBox, cyAdvPanel, cyAdvProgressionPanel, cyAdvSpeedButton, 
  cyAttract, cyBaseButton, cyBaseColorMatrix, cyBaseContainer, cyBaseExtCtrls, 
  cyBaseFlowPanel, cyBaseGridPanel, cyBaseLabel, cyBaseLed, cyBaseMeasure, 
  cyBasePanel, cyBaseSpeedButton, cyBevel, cyBitBtn, cyBook, cyBookmarkList, 
  cyBookmarks, cyClasses, cyColorGrid, cyColorMatrix, cyCustomGauge, 
  cyCustomMeasure, cyCustomProgressionPanel, cyDateUtils, cyDBAdvLed, 
  cyDBEdit, cyDBHotLabel, cyDBLabel, cyDBLed, cyDBSimpleGauge, cyDebug, 
  cyDmmCanvas, cyEdit, cyEditDate, cyEditFloat, cyEditInteger, cyEditTime, 
  cyFieldLink, cyFlowPanel, cyFlyingContainer, cyGraphics, cyGridPanel, 
  cyHotLabel, cyImage, cyLabel, cyLed, cyMathParser, cyModalContainer, 
  cyNavPanel, cyPaintBox, cyPanel, cyPieGauge, cyProgressionPanel, cyResizer, 
  cyRunTimeResize, cySearchFiles, cySimpleGauge, cySkinArea, cySkinButton, 
  cySpeedButton, cySplitter, cyStatusBar, cyStrUtils, cyTypes, cyVirtualGrid, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllCindyRegister', @AllCindyRegister.Register);
end;

initialization
  RegisterPackage('cindy', @Register);
end.
