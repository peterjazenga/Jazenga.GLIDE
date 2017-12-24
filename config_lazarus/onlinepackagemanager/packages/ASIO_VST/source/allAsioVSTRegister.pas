{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ASIOVST.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit allAsioVSTRegister;

interface

uses
  TypInfo,
  LCLIntf, LazIDEIntf, PropEdits, ComponentEditors,
  LResources, Classes;

procedure Register;

implementation

{$R allAsioVSTRegister.res}

 uses
  DAV_ASIOHost, DAV_ASIOHostAudioData, DAV_ASIOGenerator,
  //.............................................
  DAV_Common, DAV_SampleRateSource, DAV_AudioData, DAV_ComplexData,
  DAV_ProcessorInfoComponent,
  //.....
  DAV_DspRemez,
  //.....
  DAV_GuiADSRGraph, DAV_GuiAudioDataDisplay, DAV_GuiButton, DAV_GuiCheckBox,
  DAV_GuiCorrelationMeter, DAV_GuiDial, DAV_GuiDialDesign, DAV_GuiDialRenderer,
  DAV_GuiDynamicWaveform, DAV_GuiEQGraph, DAV_GuiEQSlide, DAV_GuiFader,
  DAV_GuiFont, DAV_GuiFontList, DAV_GuiFontDesign, DAV_GuiGroup,
  DAV_GuiInscription, DAV_GuiImageControl, DAV_GuiImageList, DAV_GuiGraphXY,
  DAV_GuiGraphXYDesign, DAV_GuiLabel, DAV_GuiLED, DAV_GuiMediaButton,
  DAV_GuiMidiKeys, DAV_GuiRadioButton, DAV_GuiPaintBox, DAV_GuiPanel,
  DAV_GuiPixelMap, DAV_GuiPixelMapDesign, DAV_GuiPng, DAV_GuiPngList,
  DAV_GuiPngDesign, DAV_GuiSelectBox, DAV_GuiSlider, DAV_GuiStaticWaveform,
  DAV_GuiStitchedButton, DAV_GuiStitchedControls, DAV_GuiStitchedDial,
  DAV_GuiStitchedDisplay, DAV_GuiStitchedImageList, DAV_GuiStitchedRadioSwitch,
  DAV_GuiStitchedPngList, DAV_GuiStitchedSwitch, DAV_GuiVUMeter,

  DAV_VSTHost,

  DAV_ModularManager, DAV_ModularBase, DAV_ModularPin,
  DAV_ModularEnvelopeFollower, DAV_ModularVoiceController, DAV_ModularOscSine,
  DAV_ModularOscSaw, DAV_ModularOscRamp, DAV_ModularOscSquare,
  DAV_ModularOscNoise, DAV_ModularOscAbsSine, DAV_ModularEnvelope,
  DAV_ModularVoice;

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TASIOHost,
                                        TASIOHostBasic,
                                        TASIOHostAudioData,
                                        TASIOGeneratorNoise,
                                        TASIOGeneratorTone,
                                        TVstHost]);

 RegisterComponents('ASIO/VST Basics', [TSampleRateSource,
                                        TAudioData32Component,
                                        TAudioData64Component,
                                        TComplexData32,
                                        TComplexData64,
                                        TAudioDataCollection32,
                                        TAudioDataCollection64,
                                        TProcessorInfoComponent]);

 //.............................................
  RegisterComponents('ASIO/VST DSP', [TRemezLowpassFilterDesigner,
                                      TRemezHighpassFilterDesigner,
                                      TRemezBandpassFilterDesigner,
                                      TRemezBandstopFilterDesigner]);

 //.............................................
  RegisterComponents('ASIO/VST GUI', [TGuiADSRGraph,
                                      TGuiAudioDataDisplay,
                                      TGuiButton,
                                      TGuiControlsCheckBox,
                                      TGuiCorrelationMeter,
                                      TGuiDial,
                                      TGuiDialEx,
                                      TGuiDialImageList,
                                      TGuiDialImageRenderer,
                                      TGuiDialMetal,
                                      TGuiDynamicWaveform,
                                      TGuiEQGraph,
                                      TGuiEQSlide,
                                      TGuiFader,
                                      TGuiFontList,
                                      TGuiGraphXY,
                                      TGuiGroup,
                                      TGuiGroupTop,
                                      TGuiGroupSide,
                                      TGuiGroupSimple,
                                      TGuiInscription,
                                      TGuiIntegerBox,
                                      TGuiImageList,
                                      TGuiLabel,
                                      TGuiLED,
                                      TGuiMediaButton,
                                      TGuiMidiKeys,
                                      TGuiControlsRadioButton,
                                      TGuiPaintBox,
                                      TGuiPanel,
                                      TGuiPngList,
                                      TGuiSelectBox,
                                      TGuiSlider,
                                      TGuiStaticWaveform,
                                      TGuiSwitch,
                                      TGuiVUMeter]);


  RegisterComponents('ASIO/VST GUI Stitched', [TGuiStitchedButton,
                                               TGuiStitchedDial,
                                               TGuiStitchedDisplay,
                                               TGuiStitchedImageList,
                                               TGuiStitchedPNGList,
                                               TGuiStitchedRadioSwitch,
                                               TGuiStitchedSwitch]);

  RegisterComponents('ASIO/VST Modular', [TModularManager,
                                          TDspVoiceController,
                                          TDspOscSine,
                                          TDspOscSaw,
                                          TDspOscRamp,
                                          TDspOscSquare,
                                          TDspOscNoise,
                                          TDspOscAbsSine,
                                          TDspEnvelope]);

  RegisterPropertyEditor(TypeInfo(TGuiCustomPixelMap), nil, '', TPixelMapProperty);
  RegisterPropertyEditor(TypeInfo(TPortableNetworkGraphicPixel32), nil, '', TPngProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiDialLayerCollectionItem, 'PrimitiveClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiDialPrimitive), TGuiDialLayerCollectionItem, 'Primitive', TPrimitiveClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiGraphXYSeriesCollectionItem, 'SeriesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiGraphXYSeries), TGuiGraphXYSeriesCollectionItem, 'Series', TSeriesClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiCustomFontCollectionItem, 'FontClassName', nil);
  RegisterPropertyEditor(TypeInfo(TGuiCustomFont), TGuiCustomFontCollectionItem, 'Font', TFontClassProperty);

  //.............................................

end;


end.
