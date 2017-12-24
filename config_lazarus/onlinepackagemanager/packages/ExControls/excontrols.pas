{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit excontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExControlsRegister, plUtils, plUtilsForHSL, TplA3nalogGaugeUnit, 
  TplButtonExUnit, TplButtonsPanelUnit, TplButtonUnit, TplCheckBoxUnit, 
  TplCheckListBoxUnit, TplCircleProgressUnit, TplColorPanelUnit, 
  TplComboBoxBrushUnit, TplComboBoxColorUnit, TplComboBoxFontUnit, 
  TplComboBoxPenStyleUnit, TplComboBoxPenWidthUnit, TplComboBoxUnit, 
  TplEditUnit, TplGalleryUnit, TplGaugeUnit, TplGnouMeterUnit, 
  TplGradGaugeUnit, TplGradientUnit, TplGradUnit, TplGroupBoxUnit, 
  TplHorVerRulersUnit, TplImageButtonUnit, TplKnobUnit, TplLabelUnit, 
  TplLCDAnimatorEditor, TplLCDLineUnit, TplLCDLineUnit_Char, 
  TplLCDScreenEditor, TplLCDScreenUnit, TplLed7SegUnit, TplLEDIndicatorUnit, 
  TplListBoxUnit, TplMagnifayUnit, TplMaskEditUnit, TplMemoUnit, 
  TplMultiGraphUnit, TplPageControlUnit, TplPaintGridUnit, 
  TplPanelTextureUnit, TplPanelTextureUnitForm, TplPanelUnit, 
  TplRadioButtonUnit, TplRulerUnit, TplScopeUnit, TplScrollbarUnit, 
  TplSearchPanelUnit, TplShapeProgressUnit, TplSideBarUnit, 
  TplSimpleChartUnit, TplSimplePieUnit, TplSliderUnit, TplSmartGridUnit, 
  TplSpeedButtonUnit, TplSpiderGraphUnit, TplSpinButtonUnit, TplSpinEditUnit, 
  TplStatusBarUnit, TplTabControlUnit, TplTreeListViewUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExControlsRegister', @AllExControlsRegister.Register);
end;

initialization
  RegisterPackage('excontrols', @Register);
end.
