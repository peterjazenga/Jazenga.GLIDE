unit AllCindyRegister;

{$mode objfpc}{$H+}

interface
uses
  Classes, TypInfo,
  lresources, lclintf,PropEdits, ComponentEditors,

  cyBevel,

  cyPanel, cyAdvPanel, cyNavPanel,

  cyPaintBox, cyAdvPaintBox, cyLabel,
  cyHotLabel, cySpeedButton, cyAdvSpeedButton, cyBitBtn,
  cySkinButton, cySkinArea, cyLed, cyAdvLed, cySimpleGauge, cySplitter,
  cyAttract, cyColorMatrix, cyColorGrid, cyPieGauge, cyStatusBar, cyBook,

  cySearchFiles, cyFlyingContainer, cyModalContainer,
  cyResizer, cyVirtualGrid, cyProgressionPanel, cyAdvProgressionPanel,
  cyRunTimeResize,
  cyMathParser, cyDebug,

  cyDBLabel, cyDBHotLabel, cyDBLed, cyDBAdvLed, cyDBSimpleGauge,cyDBEdit,

  cyBookmarks, cyFieldLink,

  cyFlowPanel, cyAdvFlowPanel,
  cyGridPanel, cyAdvGridPanel,

  cyEdit, cyEditDate, cyEditTime, cyEditInteger, cyEditFloat,

  cyAdvButton;
const
  RSVisualCompsPalette        = 'Cindy Base';
  RSDataVisualCompsPalette    = 'Cindy Data Visual';
  RSDataNonVisualCompsPalette = 'Cindy Data Non-Visual';
  RSNonVisualCompsPalette     = 'Cindy Non-Visual';
  RSIEWrappersPalette         = 'Cindy IE Wrappers';




procedure Register;

implementation

{$R AllCindyRegister.res}

procedure Register;
begin
   RegisterComponents(RSVisualCompsPalette, [
    TcyBevel,
    TcyPanel,
    TcyAdvPanel,
    TcyNavPanel,
    TcyPaintBox,
    TcyAdvPaintBox,
    TcyLabel,
    TcyHotLabel,
    TcySpeedButton,
    TcyAdvSpeedButton,
    TcyBitBtn,
    TcyAdvButton,
    TcySkinButton,
    tcySkinArea,

    TcyEdit, TcyEditInteger, TcyEditFloat, TcyEditDate, TcyEditTime,

    TcyLed,
    TcyAdvLed,
    TcySimpleGauge,
    TcyPieGauge,
    TcySplitter,

    TcyFlowPanel,
    TcyAdvFlowPanel,

    TcyGridPanel,
    TcyAdvGridPanel,

    TcyAttract,
    TcyColorMatrix,
    TcyColorGrid,
    TcyStatusBar,
    TcyBook
    ]);

   
    //==================================================
    RegisterComponents(RSNonVisualCompsPalette, [
    TcySearchFiles,
    TcyFlyingContainer,
    TcyModalContainer,
    TcyResizer,
    TcyVirtualGrid,
    TcyProgressionPanel,
    TcyAdvProgressionPanel,
    TcyRunTimeResize,
    TcyMathParser,
    TcyDebug
    ]);
   

    //==================================================
    RegisterComponents(RSDataVisualCompsPalette, [
    TcyDBLabel,
    TcyDBHotLabel,
    TcyDBEdit,
    TcyDBLed,
    TcyDBAdvLed,
    TcyDBSimpleGauge
    ]);
     
    //==================================================
    RegisterComponents(RSDataNonVisualCompsPalette, [
    TcyBookmarks,
    TcyFieldLink
    ]);
     
end;


end.

