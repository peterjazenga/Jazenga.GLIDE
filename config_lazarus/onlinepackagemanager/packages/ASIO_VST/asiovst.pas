{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit asiovst;

{$warn 5023 off : no warning about unused units}
interface

uses
  allAsioVSTRegister, allAsioVSTRegisterIDE, DAV_Asio, DAV_AsioConvert, 
  DAV_AsioGenerator, DAV_AsioHost, DAV_AsioInterface, DAV_OpenAsio, 
  DAV_Approximations, DAV_AudioData, DAV_Common, DAV_Complex, DAV_ComplexData, 
  DAV_Math, DAV_DspBesselFilter, DAV_DspCepstrum, DAV_DspChorus, 
  DAV_DspConvolution, DAV_DspCorrelation, DAV_DspCrosstalkCancellation, 
  DAV_DspCrosstalkSimulator, DAV_DspDelayLines, DAV_DspDFT, 
  DAV_DspDitherNoiseShaper, DAV_DspDownsampleScheduler, 
  DAV_DspDynamicLookaheadLimiter, DAV_DspExciter, DAV_DspFDNReverb, 
  DAV_DspFeedbackDelayNetwork, DAV_DspFFT, DAV_DspFftReal2Complex, 
  DAV_DspFilter, DAV_DspFilterAllpasses, DAV_DspFilterBasics, 
  DAV_DspFilterBasicsAutomatable, DAV_DspFilterButterworth, 
  DAV_DspFilterChebyshev, DAV_DspFilterChebyshevType1, 
  DAV_DspFilterChebyshevType2, DAV_DspFilterLinearPhase, 
  DAV_DspFilterLinearPhaseCrossover, DAV_DspFilterLinkwitzRiley, 
  DAV_DspFilterSimple, DAV_DspFilterSpectralDelay, DAV_DspFilterTransform, 
  DAV_DspFreeverb, DAV_DspFreeverbFilter, DAV_DspFrequencyDivider, 
  DAV_DspFrequencyShifter, DAV_DspHumRemoval, DAV_DspInterpolation, 
  DAV_DspLeslie, DAV_DspLevelingAmplifier, DAV_DspLFO, 
  DAV_DspLightweightDynamics, DAV_DspLorenzOscilator, DAV_DspMetronome, 
  DAV_DspMinBlep, DAV_DspModDelay, DAV_DspNoiseShapingFilterDesigner, 
  DAV_DspParametricEQ, DAV_DspPhaser, DAV_DspPinkNoiseGenerator, 
  DAV_DspPlateReverb, DAV_DspPolyphaseDownsampler, DAV_DspPolyphaseFilter, 
  DAV_DspPolyphaseIirDesigner, DAV_DspPolyphaseUpsampler, 
  DAV_DspPsychoacousticBassEnhancer, DAV_DspR128, DAV_DspRemez, 
  DAV_DspSpectralEffects, DAV_DspSpectralFilters, 
  DAV_DspSpectralNoiseReduction, DAV_DspStateVariableFilter, 
  DAV_DspUpDownsampling, DAV_DspWaveshaper, DAV_DspWindowing, DAV_AudioFile, 
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_AudioFileDataCache, 
  DAV_AudioFileWAV, DAV_ChannelDataCoder, DAV_ChunkAiffBasic, 
  DAV_ChunkClasses, DAV_ChunkWaveBasic, DAV_ChunkWaveCustom, DAV_MpegAudio, 
  DAV_MpegAudioLayer3, DAV_GuiADSRGraph, DAV_GuiAudioDataDisplay, 
  DAV_GuiAudioDataDisplayAxis, DAV_GuiAudioDataDisplayCursor, 
  DAV_GuiBaseControl, DAV_GuiBitmap, DAV_GuiBlend, DAV_GuiBlendReference, 
  DAV_GuiButton, DAV_GuiByteMap, DAV_GuiCheckBox, DAV_GuiCorrelationMeter, 
  DAV_GuiCustomMap, DAV_GuiDesign, DAV_GuiDial, DAV_GuiDialDesign, 
  DAV_GuiDialRenderer, DAV_GuiDynamicWaveform, DAV_GuiEQGraph, DAV_GuiEQSlide, 
  DAV_GuiFileFormatGraphics, DAV_GuiFileFormats, DAV_GuiFilters, 
  DAV_GuiFixedPoint, DAV_GuiFont, DAV_GuiFontDesign, DAV_GuiGraphXY, 
  DAV_GuiGraphXYDesign, DAV_GuiInscription, DAV_GuiInterface, DAV_GuiLabel, 
  DAV_GuiLED, DAV_GuiLevelMeter, DAV_GuiMediaButton, DAV_GuiMidiKeys, 
  DAV_GuiMidiKeyZones, DAV_GuiPaintBox, DAV_GuiPanel, DAV_GuiPixelMap, 
  DAV_GuiPixelMapDesign, DAV_GuiPng, DAV_GuiPngChunks, DAV_GuiPngClasses, 
  DAV_GuiPngCoder, DAV_GuiPngDesign, DAV_GuiPngImageList, 
  DAV_GuiPngResourceStrings, DAV_GuiPngTypes, DAV_GuiSelectBox, DAV_GuiShadow, 
  DAV_GuiSlider, DAV_GuiStaticWaveform, DAV_GuiStitchedButton, 
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedDisplay, 
  DAV_GuiStitchedPngList, DAV_GuiStitchedSwitch, DAV_GuiVector, 
  DAV_GuiVectorPixel, DAV_GuiVectorPixelCircle, DAV_GuiVectorPixelLine, 
  DAV_GuiVUMeter, DAV_VSTBasicModule, DAV_VSTChannels, DAV_VSTCustomModule, 
  DAV_VSTHost, DAV_VSTModule, DAV_VSTModuleWithDsp, DAV_VSTModuleWithMidi, 
  DAV_VSTModuleWithPrograms, DAV_VSTOfflineTask, DAV_VSTParameters, 
  DAV_VSTPrograms, DAV_VSTShellPlugins, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('allAsioVSTRegister', @allAsioVSTRegister.Register);
  RegisterUnit('allAsioVSTRegisterIDE', @allAsioVSTRegisterIDE.Register);
end;

initialization
  RegisterPackage('asiovst', @Register);
end.
