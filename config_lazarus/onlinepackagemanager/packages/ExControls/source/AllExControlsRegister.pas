{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ExControls.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllExControlsRegister;

{$MODE Delphi}

interface

{$I AllExControlsRegister.inc}

 uses
  Classes,SysUtils,TypInfo,lresources,PropEdits,ComponentEditors,
  Controls, Forms,ComCtrls, ExtCtrls,
  //...........................
  TplColorPanelUnit,
  TplGaugeUnit,
  TplGnouMeterUnit,
  TplKnobUnit,
  TplPaintGridUnit,
  TplProgressBarUnit,
  TplStatusBarUnit,
  TplShapeProgressUnit,
  TplCircleProgressUnit,
  TplGradientUnit,
  TplLed7SegUnit,
  TplLCDLineUnit,
  TplLCDScreenUnit,
  TplA3nalogGaugeUnit,
  TplSpiderGraphUnit,
  TplMultiGraphUnit,
  TplLEDIndicatorUnit,
  TplScopeUnit,
  TplSimplePieUnit,
  TplSideBarUnit,
  TplGalleryUnit,
  TplButtonExUnit,
  TplSearchPanelUnit,
  TplTreeListViewUnit,
  TplSmartGridUnit,
  TplSimpleChartUnit,
  TplGradUnit,
  TplGradGaugeUnit,
  TplButtonsPanelUnit,
  TplHorVerRulersUnit,
  TplRulerUnit,
  TplSpinEditUnit,

  TplButtonUnit,
  TplLabelUnit,
  TplCheckBoxUnit,
  TplCheckListBoxUnit,
  TplPanelUnit,
  TplEditUnit,
  TplGroupBoxUnit,
  TplImageButtonUnit,
  TplListBoxUnit,
  TplMagnifayUnit,
  TplMaskEditUnit,
  TplMemoUnit,
  TplRadioButtonUnit,
  TplSliderUnit,
  TplSpeedButtonUnit,
  TplSpinButtonUnit,

  TplPanelTextureUnit,
  TplPanelTextureUnitForm,
  TplScrollbarUnit,

  TplComboBoxUnit,
  TplComboBoxBrushUnit,
  TplComboBoxColorUnit,

  TplTabControlUnit,
  TplPageControlUnit,
  TplComboBoxFontUnit,
  TplComboBoxPenStyleUnit,
  TplComboBoxPenWidthUnit,

  //...........................
  plUtils,
  plUtilsForHSL;

type


TplPageControlEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

TplTabSheetEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

TEZTextureProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TEZShadeEditor = class(TDefaultComponentEditor)
  protected
    procedure EditProperty(const Prop: TPropertyEditor; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

{$R AllExControlsRegister.res}

 uses dialogs,LazIDEIntf,
      TplLCDAnimatorEditor,
      TplLCDScreenEditor;


//==========================================================
procedure Register;
begin
 RegisterComponents ('Extra Controls 1',[
               TplButton,
               TplSpeedButton,
               TplPanel,
               TplColorPanel,
               TplGradient,
               TplPanelTexture,
               TplCheckBox,
               TplRadioButton,
               TplLabel, TplURLLabel,
               TplEdit,
               TplMaskEdit,
               TplSpinEditInteger,
               TplSpinEditFloat,
               TplGroupBox,
               TplListBox,
               TplCheckListBox,
               TplMemo,
               TplSlider,
               TplScrollbar,
               TplSpinButton,
               TplGauge,
               TplProgressBar,
               TplStatusBar,
               TplTabControl,

          //     TplPageControl,
               TplComboBox,
               TplColorComboBox,
               TplFontComboBox,
               TplBrushComboBox,
               TplPenStyleComboBox,
               TplPenWidthComboBox
                                 ]);

 RegisterComponents ('Extra Controls 2',[
               TplButtonEx,
               TplImageButton,
               TplSuperKnob,
               TplShapeProgress,
               TplCircleProgress,
               TplGrad,
               TplGradGauge,
               TplRuler, TplRulerCorner,
               TplHorizontalRuler, TplVerticalRuler,
               TplButtonsPanel,
               TplSearchPanel,
               TplSmartGrid, TplSmartGridSync,
               TplTreeListView,
               TplSideBar,
               TplGallery,
               TplLed7Seg,
               TplLCDLine,
               TplLCDScreen, TplLCDAnimator,
               TplGnouMeter,
               TplA3nalogGauge,
               TplLEDIndicator,
               TplScope,
               TplSimplePie,
               TplMultiGraph,
               TplSpiderGraph,
               TplPaintBoxGrid,
               TplBitmapGrid,
               TplSimpleChart,
               TplMagnifay
                                 ]);

 RegisterComponentEditor(TplLCDAnimator,TLCDAnimatorEditor);
 RegisterPropertyEditor(TypeInfo(TStrings), TplLCDAnimator, 'Code', TLCDAnimatorCodeEditorProperty);
 RegisterComponentEditor(TplLCDScreen,TLCDScreenEditor);
 RegisterPropertyEditor(TypeInfo(TStringList), TplLCDScreen, 'Lines', TLCDScreenLinesEditorProperty);

 RegisterPropertyEditor(TypeInfo(TEZShades), nil, '', TEZTextureProperty);
 RegisterComponentEditor(TplPanelTexture, TEZShadeEditor);

 RegisterClass(TplTabSheet);
// RegisterComponentEditor(TplPageControl,TplPageControlEditor);
 RegisterComponentEditor(TplTabSheet, TplTabSheetEditor);

end;

//=========================== TEZShadeEditor ====================================

procedure TEZShadeEditor.EditProperty(const Prop: TPropertyEditor; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'SHADEOBJECTS') = 0) then begin
    Prop.Edit;
    Designer.Modified;
    Continue := False;
  end;
end;

function TEZShadeEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TEZShadeEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := 'Edit shaded objects'
  else Result := '';
end;

//=========================TEZShadeEditor============================

procedure TEZShadeEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

procedure TEZTextureProperty.Edit;
var
  EZSP : TEZShades;
  EZShadeEditor: TEZShadeDlg;
begin
  EZSP := TEZShades(GetObjectValue);//GetOrdValue);       // ct9999
  EZShadeEditor := TEZShadeDlg.Create(Application);
  try
    EZShadeEditor.EditorShades := TEZShades.Create;
    EZShadeEditor.EditorShades.LoadFromStream(EZSP);
    LoadShades(EZShadeEditor.ShadeObjects,EZShadeEditor.EditorShades);
    EZShadeEditor.ShowModal;
  finally
    SaveShades(EZShadeEditor.EditorShades,EZShadeEditor.ShadeObjects);
    EZSP.LoadFromStream(EZShadeEditor.EditorShades);
    EZShadeEditor.Free;
  end;
  with GetComponent(0) as TplPanelTexture do begin
    UpdateShadeObjects;
    UpdateTexture;
    Invalidate;
  end;
end;

function TEZTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//===================== TplPageControlEditor =====================================

procedure TplPageControlEditor.ExecuteVerb(Index: integer);
var NewPage : TplTabSheet;
    xname:string;
    Hook: TPropertyEditorHook;
begin
   Hook:=nil;
   if not GetHook(Hook) then exit;

  case Index of
  0:
    begin
      TCustomTabControl(Component).ControlStyle := TCustomTabControl(Component).ControlStyle + [csAcceptsControls];
      //page := TplTabSheet(Designer.cCreateComponent(TplTabSheet,Component,23,0,100,100));

      //TplPageControl(component).pages

      NewPage:=TplTabSheet.Create(TplPageControl(component));
      NewPage.SetBounds(23,0,100,100);

      xname:=Designer.CreateUniqueComponentName(NewPage.ClassName);
      NewPage.Name:=xname;

      NewPage.parent := TplPageControl(component);
      NewPage.AdvPageControl := TplPageControl(component);
      NewPage.Caption := NewPage.name;
      TplPageControl(component).ActivePage:=NewPage;
      with TplPageControl(Component) do Update;

      (Component as TCustomTabControl).Invalidate;
      TCustomTabControl(Component).ControlStyle := TCustomTabControl(Component).ControlStyle - [csAcceptsControls];

      Hook.PersistentAdded(NewPage,true);
      Modified;
    end;
  1: TplPageControl(Component).SelectNextPage(false);
  2: TplPageControl(Component).SelectNextPage(True);
  end;

end;

function TplPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  end;
end;

function TplPageControlEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

//============== TplTabSheetEditor ==============================================

procedure TplTabSheetEditor.ExecuteVerb(Index: integer);
var NewPage : TplTabSheet;
    xname:string;
    Hook: TPropertyEditorHook;
begin
   Hook:=nil;
   if not GetHook(Hook) then exit;

  case Index of
  0:
    begin
      TWinControl(Component).Parent.ControlStyle := TWinControl(Component).Parent.ControlStyle + [csAcceptsControls];
     //NewPage := TplTabSheet(Designer.CreateComponent(TplTabSheet,TWinControl(Component).Parent,23,0,100,100));
      NewPage:=TplTabSheet.Create(TplPageControl(component));
      NewPage.SetBounds(23,0,100,100);

      xname:=Designer.CreateUniqueComponentName(NewPage.ClassName);
      NewPage.Name:=xname;

      NewPage.parent := TWinControl(Component).Parent;
      NewPage.AdvPageControl := TplPageControl(TWinControl(Component).Parent);
      NewPage.Caption := NewPage.name;
      TplPageControl(TWinControl(Component).Parent).ActivePage:= NewPage;

      with TplPageControl(TWinControl(Component).Parent) do Update;

      (TWinControl(Component).Parent as TWinControl).Invalidate;
      TWinControl(Component).Parent.ControlStyle := TWinControl(Component).Parent.ControlStyle - [csAcceptsControls];

      Hook.PersistentAdded(NewPage,true);
      Modified;

    end;
  1: TplPageControl(TCustomPanel(Component).Parent).SelectNextPage(false);
  2: TplPageControl(TCustomPanel(Component).Parent).SelectNextPage(true);
  3: begin
      TplTabSheet(Component).AdvPageControl := nil;
      Component.Free;
      Designer.Modified;
    end;
  end;
end;

function TplTabSheetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  3: Result := 'Delete Page';
  end;
end;

function TplTabSheetEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;



end.
