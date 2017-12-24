unit spkte_AppearanceEditor;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, Spin,
  SpkGUITools, SpkXMLParser,
  spkt_Buttons, spkt_BaseItem, spkt_Pane, spkt_Types, spkt_Tab, SpkToolbar,
  spkt_Appearance;

type

  { TfrmAppearanceEditWindow }

  TfrmAppearanceEditWindow = class(TForm)
    Label15: TLabel;
    Label16: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    PaneHSpacer: TBevel;
    ItemHSpacer: TBevel;
    edPaneHotTrackBrightnessChange: TSpinEdit;
    edItemHotTrackBrightnessChange: TSpinEdit;
    TabVSpacer: TBevel;
    bInactiveTabHeaderFontColor: TSpeedButton;
    bItemActiveInnerDarkColor: TSpeedButton;
    bItemActiveGradientFromColor: TSpeedButton;
    bItemActiveGradientToColor: TSpeedButton;
    bItemActiveCaptionColor: TSpeedButton;
    bItemActiveInnerLightColor: TSpeedButton;
    bItemHotTrackInnerDarkColor: TSpeedButton;
    bItemHotTrackFrameColor: TSpeedButton;
    bItemActiveFrameColor: TSpeedButton;
    bItemHotTrackGradientFromColor: TSpeedButton;
    bItemHotTrackGradientToColor: TSpeedButton;
    bItemHotTrackCaptionColor: TSpeedButton;
    bItemHotTrackInnerLightColor: TSpeedButton;
    bItemIdleInnerDarkColor: TSpeedButton;
    bItemIdleGradientFromColor: TSpeedButton;
    bItemIdleGradientToColor: TSpeedButton;
    bItemIdleCaptionColor: TSpeedButton;
    bItemIdleInnerLightColor: TSpeedButton;
    bPaneBorderDarkColor: TSpeedButton;
    bPaneBorderLightColor: TSpeedButton;
    bPaneGradientFromColor: TSpeedButton;
    bPaneGradientToColor: TSpeedButton;
    bPaneCaptionBackgroundColor: TSpeedButton;
    bPaneCaptionFontColor: TSpeedButton;
    bItemIdleFrameColor: TSpeedButton;
    bTabGradientFromColor: TSpeedButton;
    bTabGradientToColor: TSpeedButton;
    bActiveTabHeaderFontColor: TSpeedButton;
    bExportToPascal: TButton;
    bCopyToClipboard: TButton;
    cbItemStyle: TComboBox;
    cbPaneStyle: TComboBox;
    ColorView: TShape;
    ItemVSpacer: TBevel;
    gbPreview: TGroupBox;
    Label12: TLabel;
    Label27: TLabel;
    LblCaptionBackground1: TLabel;
    LblRGB: TLabel;
    SmallImages: TImageList;
    LargeImages: TImageList;
    Label18: TLabel;
    LblInactiveTabHeaderFontColor: TLabel;
    pInactiveTabHeaderFont: TPanel;
    ButtonPanel: TPanel;
    bTabFrameColor: TSpeedButton;
    SpkTab2: TSpkTab;
    TabHSpacer: TBevel;
    tbPreview: TSpkToolbar;
    SpkTab1: TSpkTab;
    SpkPane1: TSpkPane;
    SpkLargeButton1: TSpkLargeButton;
    SpkLargeButton3: TSpkLargeButton;
    SpkLargeButton2: TSpkLargeButton;
    SpkPane2: TSpkPane;
    SpkSmallButton1: TSpkSmallButton;
    SpkSmallButton2: TSpkSmallButton;
    SpkSmallButton3: TSpkSmallButton;
    SpkPane3: TSpkPane;
    SpkSmallButton4: TSpkSmallButton;
    SpkSmallButton5: TSpkSmallButton;
    SpkSmallButton6: TSpkSmallButton;
    SpkSmallButton7: TSpkSmallButton;
    SpkSmallButton8: TSpkSmallButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label2: TLabel;
    pTabFrame: TPanel;
    pTabGradientFrom: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    pTabGradientTo: TPanel;
    cbTabGradientKind: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    pTabHeaderFont: TPanel;
    Label8: TLabel;
    pPaneBorderDark: TPanel;
    pPaneBorderLight: TPanel;
    Label21: TLabel;
    Label9: TLabel;
    pPaneGradientFrom: TPanel;
    pPaneGradientTo: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    cbPaneGradientKind: TComboBox;
    pPaneCaptionBackground: TPanel;
    LblPaneCaptionBackground: TLabel;
    Label13: TLabel;
    pPaneCaptionFont: TPanel;
    Label1: TLabel;
    Label7: TLabel;
    Label14: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    pItemFont: TPanel;
    cbItemIdleGradientKind: TComboBox;
    pItemIdleGradientTo: TPanel;
    pItemIdleGradientFrom: TPanel;
    pItemIdleFrame: TPanel;
    LblIdle: TLabel;
    Label28: TLabel;
    pItemIdleCaptionColor: TPanel;
    Label29: TLabel;
    pItemIdleInnerDark: TPanel;
    LblLinkInnerLightColor: TLabel;
    pItemIdleInnerLight: TPanel;
    cbItemHottrackGradientKind: TComboBox;
    pItemHottrackGradientTo: TPanel;
    pItemHottrackGradientFrom: TPanel;
    pItemHottrackFrame: TPanel;
    LblHotTrack: TLabel;
    pItemHottrackCaptionColor: TPanel;
    pItemHottrackInnerDark: TPanel;
    pItemHottrackInnerLight: TPanel;
    cbItemActiveGradientKind: TComboBox;
    pItemActiveGradientTo: TPanel;
    pItemActiveGradientFrom: TPanel;
    pItemActiveFrame: TPanel;
    LblActive: TLabel;
    pItemActiveCaptionColor: TPanel;
    pItemActiveInnerDark: TPanel;
    pItemActiveInnerLight: TPanel;
    bOK: TButton;
    bCancel: TButton;
    cdColorDialog: TColorDialog;
    fdFontDialog: TFontDialog;
    pActiveTabHeaderFont: TPanel;
    pPaneCaptionFontColor: TPanel;
    TabSheet4: TTabSheet;
    bImport: TButton;
    bExportToXML: TButton;
    mXML: TMemo;
    sTabRectangle: TShape;
    cbLinkTab: TCheckBox;
    sPaneRectangle: TShape;
    cbLinkPane: TCheckBox;
    cbLinkItem: TCheckBox;
    sItemRectangle: TShape;
    TabSheet5: TTabSheet;
    Label17: TLabel;
    LbAppearanceStyle: TListbox;

    procedure bExportToPascalClick(Sender: TObject);
    procedure bExportToXMLClick(Sender: TObject);
    procedure bImportClick(Sender: TObject);
    procedure bInactiveTabHeaderFontColorClick(Sender: TObject);
    procedure bItemActiveCaptionColorClick(Sender: TObject);
    procedure bItemActiveFrameColorClick(Sender: TObject);
    procedure bItemActiveGradientFromColorClick(Sender: TObject);
    procedure bItemActiveGradientToColorClick(Sender: TObject);
    procedure bItemActiveInnerDarkColorClick(Sender: TObject);
    procedure bItemActiveInnerLightColorClick(Sender: TObject);
    procedure bItemHotTrackCaptionColorClick(Sender: TObject);
    procedure bItemHotTrackFrameColorClick(Sender: TObject);
    procedure bItemHotTrackGradientFromColorClick(Sender: TObject);
    procedure bItemHotTrackGradientToColorClick(Sender: TObject);
    procedure bItemHotTrackInnerDarkColorClick(Sender: TObject);
    procedure bItemHotTrackInnerLightColorClick(Sender: TObject);
    procedure bItemIdleCaptionColorClick(Sender: TObject);
    procedure bItemIdleFrameColorClick(Sender: TObject);
    procedure bItemIdleGradientFromColorClick(Sender: TObject);
    procedure bItemIdleGradientToColorClick(Sender: TObject);
    procedure bItemIdleInnerDarkColorClick(Sender: TObject);
    procedure bItemIdleInnerLightColorClick(Sender: TObject);
    procedure bPaneBorderDarkColorClick(Sender: TObject);
    procedure bPaneBorderLightColorClick(Sender: TObject);
    procedure bPaneCaptionBackgroundColorClick(Sender: TObject);
    procedure bPaneCaptionFontColorClick(Sender: TObject);
    procedure bPaneGradientFromColorClick(Sender: TObject);
    procedure bPaneGradientToColorClick(Sender: TObject);
    procedure bResetClick(Sender: TObject);

    procedure bTabBorderColorClick(Sender: TObject);
    procedure bTabGradientFromColorClick(Sender: TObject);
    procedure bTabGradientToColorClick(Sender: TObject);
    procedure bActiveTabHeaderFontColorClick(Sender: TObject);
    procedure bCopyToClipboardClick(Sender: TObject);
    procedure cbItemActiveGradientKindChange(Sender: TObject);
    procedure cbItemHottrackGradientKindChange(Sender: TObject);
    procedure cbItemIdleGradientKindChange(Sender: TObject);
    procedure cbItemStyleChange(Sender: TObject);
    procedure cbPaneGradientKindChange(Sender: TObject);
    procedure cbPaneStyleChange(Sender: TObject);
    procedure cbTabGradientKindChange(Sender: TObject);

    procedure cbLinkItemClick(Sender: TObject);
    procedure cbLinkPaneClick(Sender: TObject);
    procedure cbLinkTabClick(Sender: TObject);
    procedure edItemHotTrackBrightnessChangeChange(Sender: TObject);
    procedure edPaneHotTrackBrightnessChangeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbAppearanceStyleClick(Sender: TObject);
    procedure pActiveTabHeaderFontClick(Sender: TObject);
    procedure pInactiveTabHeaderFontClick(Sender: TObject);

    procedure pTabFrameClick(Sender: TObject);
    procedure pTabGradientFromClick(Sender: TObject);
    procedure pTabGradientToClick(Sender: TObject);

    procedure pPaneBorderDarkClick(Sender: TObject);
    procedure pPaneBorderLightClick(Sender: TObject);
    procedure pPaneCaptionFontClick(Sender: TObject);
    procedure pPaneCaptionFontColorClick(Sender: TObject);
    procedure pPaneGradientFromClick(Sender: TObject);
    procedure pPaneGradientToClick(Sender: TObject);
    procedure pPaneCaptionBackgroundClick(Sender: TObject);

    procedure pItemActiveCaptionColorClick(Sender: TObject);
    procedure pItemActiveFrameClick(Sender: TObject);
    procedure pItemActiveGradientFromClick(Sender: TObject);
    procedure pItemActiveGradientToClick(Sender: TObject);
    procedure pItemActiveInnerDarkClick(Sender: TObject);
    procedure pItemActiveInnerLightClick(Sender: TObject);

    procedure pItemFontClick(Sender: TObject);
    procedure pItemIdleCaptionColorClick(Sender: TObject);
    procedure pItemIdleFrameClick(Sender: TObject);
    procedure pItemIdleGradientFromClick(Sender: TObject);
    procedure pItemIdleGradientToClick(Sender: TObject);
    procedure pItemIdleInnerDarkClick(Sender: TObject);
    procedure pItemIdleInnerLightClick(Sender: TObject);

    procedure pItemHottrackCaptionColorClick(Sender: TObject);
    procedure pItemHottrackFrameClick(Sender: TObject);
    procedure pItemHottrackGradientFromClick(Sender: TObject);
    procedure pItemHottrackGradientToClick(Sender: TObject);
    procedure pItemHottrackInnerDarkClick(Sender: TObject);
    procedure pItemHottrackInnerLightClick(Sender: TObject);

    procedure pTabHeaderFontClick(Sender: TObject);

  private
    procedure SetLinkedFrameColor(AColor : TColor);
    procedure SetLinkedGradientFromColor(AColor : TColor);
    procedure SetLinkedGradientToColor(AColor : TColor);
    procedure SetLinkedGradientKind(AKindIndex : integer);

    function GetAppearance: TSpkToolbarAppearance;
    procedure SetAppearance(const Value: TSpkToolbarAppearance);

    procedure SwitchAttributesLink(const Value : boolean);

    function ChangeColor(Panel : TPanel) : boolean;
    procedure SetPanelColor(Panel: TPanel; AColor : TColor);
    function ChangeFont(Panel : TPanel) : boolean;
    procedure SetPanelFont(Panel : TPanel; AFont : TFont);
    procedure SetComboGradientKind(Combo : TComboBox; GradientType : TBackgroundKind);
    procedure LoadAppearance(AAppearance : TSpkToolbarAppearance);

  private  { Color picker }
    FScreenBitmap: TBitmap;
    FScreenshotForm: TForm;
    function PickColor(APanel: TPanel): Boolean;
    procedure ScreenshotKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure ScreenshotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ScreenshotMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: integer);
    procedure ScreenshotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  public
    property Appearance : TSpkToolbarAppearance read GetAppearance write SetAppearance;
  end;

var
  frmAppearanceEditWindow: TfrmAppearanceEditWindow;

implementation

{$R *.lfm}

uses
  clipbrd, Spkt_Const;

var
  CurrPageIndex: Integer = 0;

{ TForm3 }

procedure TfrmAppearanceEditWindow.SetAppearance(const Value: TSpkToolbarAppearance);
begin
  tbPreview.Appearance.Assign(Value);
end;

procedure TfrmAppearanceEditWindow.SetComboGradientKind(Combo: TComboBox;
  GradientType: TBackgroundKind);
begin
  Combo.ItemIndex := ord(GradientType);
end;

procedure TfrmAppearanceEditWindow.SetLinkedFrameColor(AColor: TColor);
begin
  tbPreview.Appearance.Tab.BorderColor := AColor;
  SetPanelColor(pTabFrame, AColor);

  tbPreview.Appearance.Pane.BorderDarkColor := AColor;
  SetPanelColor(pPaneBorderDark, AColor);

  tbPreview.Appearance.Element.IdleFrameColor := AColor;
  SetPanelColor(pItemIdleFrame, AColor);
end;

procedure TfrmAppearanceEditWindow.SetLinkedGradientFromColor(AColor: TColor);
begin
  tbPreview.Appearance.Tab.GradientFromColor := AColor;
  SetPanelColor(pTabGradientFrom, AColor);

  tbPreview.Appearance.Pane.GradientFromColor := AColor;
  SetPanelColor(pPaneGradientFrom, AColor);

  tbPreview.Appearance.Element.IdleGradientFromColor := AColor;
  SetPanelColor(pItemIdleGradientFrom, AColor);
end;

procedure TfrmAppearanceEditWindow.SetLinkedGradientKind(AKindIndex: integer);
var
  Kind: TBackgroundKind;
begin
  Kind := TBackgroundKind(AKindIndex);

  tbPreview.Appearance.Tab.GradientType := Kind;
  SetComboGradientKind(cbTabGradientKind, Kind);

  tbPreview.Appearance.Pane.GradientType := Kind;
  SetComboGradientKind(cbPaneGradientKind, Kind);

  tbPreview.Appearance.Element.IdleGradientType := Kind;
  SetComboGradientKind(cbItemIdleGradientKind, Kind);
end;

procedure TfrmAppearanceEditWindow.SetLinkedGradientToColor(AColor: TColor);
begin
  tbPreview.Appearance.Tab.GradientToColor := AColor;
  SetPanelColor(pTabGradientTo, AColor);

  tbPreview.Appearance.Pane.GradientToColor := AColor;
  SetPanelColor(pPaneGradientTo, AColor);

  tbPreview.Appearance.Element.IdleGradientToColor := AColor;
  SetPanelColor(pItemIdleGradientTo, AColor);
end;

procedure TfrmAppearanceEditWindow.SetPanelColor(Panel: TPanel; AColor: TColor);
begin
  Panel.Color := AColor;
  if Panel.Color <> AColor then
     Showmessage('lipa!');
  if (GetRValue(AColor) + GetGValue(AColor) + GetBValue(AColor)) div 3 >= 128 then
    Panel.Font.Color := clBlack
  else
    Panel.Font.Color := clWhite;
  Panel.Caption := '$' + IntToHex(AColor, 8);
end;

procedure TfrmAppearanceEditWindow.SetPanelFont(Panel: TPanel; AFont: TFont);
begin
  Panel.Font.Assign(AFont);
  Panel.Caption := AFont.Name + ', ' + IntToStr(AFont.Size);
end;

procedure TfrmAppearanceEditWindow.bTabBorderColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pTabFrame) then begin
    tbPreview.Appearance.Tab.BorderColor := pTabFrame.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pTabFrame.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bTabGradientFromColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pTabGradientFrom) then begin
    tbPreview.Appearance.Tab.GradientFromColor := pTabGradientFrom.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pTabGradientFrom.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bTabGradientToColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pTabGradientTo) then begin
    tbPreview.Appearance.Tab.GradientToColor := pTabGradientTo.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pTabGradientTo.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bCopyToClipboardClick(Sender: TObject);
begin
  if mXML.Lines.Count > 0 then
    Clipboard.AsText := mXML.Text;
end;

procedure TfrmAppearanceEditWindow.bActiveTabHeaderFontColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pActiveTabHeaderFont) then begin
    tbPreview.Appearance.Tab.TabHeaderFont.Color := pActiveTabHeaderFont.Color;
    tbPreview.ForceRepaint;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bExportToPascalClick(Sender: TObject);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    tbPreview.Appearance.SaveToPascal(L);
    mXML.Clear;
    mXML.Lines.Assign(L);
  finally
    L.Free;
  end;
end;

procedure TfrmAppearanceEditWindow.bExportToXMLClick(Sender: TObject);
var
  Xml: TSpkXMLParser;
  Node: TSpkXMLNode;
begin
  XML:=TSpkXMLParser.Create;
  try
    Node := XML['Appearance', true];
    tbPreview.Appearance.SaveToXML(Node);
    mXML.Clear;
    mXml.Text:=XML.Generate;
  finally
    XML.Free;
  end;
end;

procedure TfrmAppearanceEditWindow.bInactiveTabHeaderFontColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pInactiveTabHeaderFont) then begin
    tbPreview.Appearance.Tab.InactiveTabHeaderFontColor := pInactiveTabHeaderFont.Color;
    tbPreview.ForceRepaint;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemActiveCaptionColorClick(Sender: TObject
  );
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemActiveCaptionColor) then begin
    tbPreview.Appearance.Element.ActiveCaptionColor := pItemActiveCaptionColor.Color;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemActiveFrameColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemActiveFrame) then begin
    tbPreview.Appearance.Element.ActiveFrameColor := pItemactiveFrame.Color;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemActiveGradientFromColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemActiveGradientFrom) then
    tbPreview.Appearance.Element.ActiveGradientFromColor := pItemActiveGradientFrom.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemActiveGradientToColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemActiveGradientTo) then
    tbPreview.Appearance.Element.ActiveGradientToColor := pItemActiveGradientTo.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemActiveInnerDarkColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemActiveInnerDark) then
    tbPreview.Appearance.Element.ActiveInnerDarkColor := pItemActiveInnerDark.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemActiveInnerLightColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemActiveInnerLight) then
    tbPreview.Appearance.Element.ActiveInnerLightColor := pItemActiveInnerLight.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemHotTrackCaptionColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemHotTrackCaptionColor) then begin
    tbPreview.Appearance.Element.HotTrackCaptionColor := pItemHotTrackCaptionColor.Color;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemHotTrackFrameColorClick(Sender: TObject
  );
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemHotTrackFrame) then begin
    tbPreview.Appearance.Element.HotTrackFrameColor := pItemHotTrackFrame.Color;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemHotTrackGradientFromColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemHotTrackGradientFrom) then
    tbPreview.Appearance.Element.HotTrackGradientFromColor := pItemHotTrackGradientFrom.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemHotTrackGradientToColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemHotTrackGradientTo) then
    tbPreview.Appearance.Element.HotTrackGradientToColor := pItemHotTrackGradientTo.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemHotTrackInnerDarkColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemHotTrackInnerDark) then
    tbPreview.Appearance.Element.HotTrackInnerDarkColor := pItemHotTrackInnerDark.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemHotTrackInnerLightColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemHotTrackInnerLight) then
    tbPreview.Appearance.Element.HotTrackInnerLightColor := pItemHotTrackInnerLight.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemIdleCaptionColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemIdleCaptionColor) then begin
    tbPreview.Appearance.Element.IdleCaptionColor := pItemIdleCaptionColor.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pItemIdleCaptionColor.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemIdleFrameColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemIdleFrame) then begin
    tbPreview.Appearance.Element.IdleFrameColor := pItemIdleFrame.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pItemIdleFrame.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemIdleGradientFromColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemIdleGradientFrom) then begin
    tbPreview.Appearance.Element.IdleGradientFromColor := pItemIdleGradientFrom.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pItemIdleGradientFrom.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemIdleGradientToColorClick(Sender: TObject
  );
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemIdleGradientTo) then begin
    tbPreview.Appearance.Element.IdleGradientToColor := pItemIdleGradientTo.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pItemIdleGradientTo.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemIdleInnerDarkColorClick(Sender: TObject
  );
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemIdleInnerDark) then begin
    tbPreview.Appearance.Element.IdleInnerDarkColor := pItemIdleInnerDark.Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor(pItemIdleInnerDark.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bItemIdleInnerLightColorClick(Sender: TObject
  );
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pItemIdleInnerLight) then begin
    tbPreview.Appearance.Element.IdleInnerLightColor := pItemIdleInnerLight.Color;
    if cbLinkPane.Checked then
      SetLinkedFrameColor(pItemIdleInnerLight.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bPaneBorderDarkColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pPaneBorderDark) then begin
    tbPreview.Appearance.Pane.BorderDarkColor := pPaneBorderDark.Color;
    if cbLinkPane.Checked then
      SetLinkedFrameColor(pPaneBorderDark.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bPaneBorderLightColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pPaneBorderLight) then begin
    tbPreview.Appearance.Pane.BorderLightColor := pPaneBorderLight.Color;
    if cbLinkPane.Checked then
      SetLinkedFrameColor(pPaneBorderLight.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bPaneCaptionBackgroundColorClick(
  Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pPaneCaptionBackground) then
    tbPreview.Appearance.Pane.CaptionBgColor := pPaneCaptionBackground.Color;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bPaneCaptionFontColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pPaneCaptionFontColor) then begin
    tbPreview.Appearance.Pane.CaptionFont.Color := pPaneCaptionFontColor.Color;
    tbPreview.ForceRepaint;
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bPaneGradientFromColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pPaneGradientFrom) then begin
    tbPreview.Appearance.Pane.GradientFromColor := pPaneGradientFrom.Color;
    if cbLinkPane.Checked then
      SetLinkedFrameColor(pPaneGradientFrom.Color)
  end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.bPaneGradientToColorClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  if PickColor(pPaneGradientTo) then begin
    tbPreview.Appearance.Pane.GradientToColor := pPaneGradientTo.Color;
    if cbLinkPane.Checked then
      SetLinkedFrameColor(pPaneGradientTo.Color)
    end;
  (Sender as TSpeedButton).Down := false;
end;

procedure TfrmAppearanceEditWindow.SwitchAttributesLink(const Value: boolean);
begin
  cbLinkTab.checked := Value;
  cbLinkPane.Checked := Value;
  cbLinkItem.Checked := Value;

  sTabRectangle.Visible := Value;
  sPaneRectangle.Visible := Value;
  sItemRectangle.Visible := Value;
end;

procedure TfrmAppearanceEditWindow.cbItemHottrackGradientKindChange(Sender: TObject);
begin
  with tbPreview.Appearance.Element do
    HotTrackGradientType := TBackgroundKind((Sender as TCombobox).ItemIndex);
end;

procedure TfrmAppearanceEditWindow.cbItemIdleGradientKindChange(Sender: TObject);
begin
  with tbPreview.Appearance.Element do
    IdleGradientType := TBackgroundKind((Sender as TCombobox).ItemIndex);
  if cbLinkItem.Checked then
     SetLinkedGradientKind((Sender as TComboBox).ItemIndex);
end;

procedure TfrmAppearanceEditWindow.cbItemStyleChange(Sender: TObject);
begin
  with tbPreview.Appearance.Element do
    Style := TSpkElementStyle((Sender as TCombobox).ItemIndex);
end;

procedure TfrmAppearanceEditWindow.cbLinkItemClick(Sender: TObject);
begin
  SwitchAttributesLink(cbLinkItem.Checked);
end;

procedure TfrmAppearanceEditWindow.cbLinkPaneClick(Sender: TObject);
begin
  SwitchAttributesLink(cbLinkPane.Checked);
end;

procedure TfrmAppearanceEditWindow.cbLinkTabClick(Sender: TObject);
begin
  SwitchAttributesLink(cbLinkTab.Checked);
end;

procedure TfrmAppearanceEditWindow.cbTabGradientKindChange(Sender: TObject);
begin
  with tbPreview.Appearance.Tab do
    GradientType := TBackgroundKind((Sender as TCombobox).ItemIndex);
  if cbLinkTab.Checked then
    SetLinkedGradientKind((Sender as TComboBox).ItemIndex);
end;

function TfrmAppearanceEditWindow.ChangeColor(Panel: TPanel): boolean;
begin
  cdColorDialog.Color:=Panel.Color;
  if cdColorDialog.Execute then
  begin
    SetPanelColor(Panel, cdColorDialog.Color);
    Result := true
  end
  else
    Result := false;
end;

function TfrmAppearanceEditWindow.ChangeFont(Panel: TPanel): boolean;
begin
  fdFontDialog.Font.Assign(Panel.Font);
  if fdFontDialog.Execute then
  begin
    SetPanelFont(Panel, fdFontDialog.Font);
    Result := true;
  end
  else
    Result := false;
end;

procedure TfrmAppearanceEditWindow.edItemHotTrackBrightnessChangeChange(
  Sender: TObject);
begin
  with tbPreview.Appearance.Element do
    HotTrackBrightnessChange := (Sender as TSpinEdit).Value;
  tbPreview.Invalidate;
end;

procedure TfrmAppearanceEditWindow.edPaneHotTrackBrightnessChangeChange(
  Sender: TObject);
begin
  with tbPreview.Appearance.Pane do
    HotTrackBrightnessChange := (Sender as TSpinEdit).Value;
  tbPreview.Invalidate;
end;

procedure TfrmAppearanceEditWindow.FormActivate(Sender: TObject);
var
  w, h: Integer;
begin
  ColorView.Width := ColorView.Height;

  w := SpkScaleX(pTabFrame.Width, 96);
  h := SpkScaleY(pTabFrame.Height, 96);

  pTabFrame.Width := w;
  pTabFrame.Height := h;
  pTabGradientFrom.Height := h;
  pTabGradientTo.Height := h;
  pActiveTabHeaderFont.Height := h;
  pInactiveTabHeaderFont.Height := h;
  pTabHeaderFont.Height := h;

  pPaneBorderDark.Width := w;
  pPaneBorderDark.Height := h;
  pPaneBorderLight.Height := h;
  pPaneGradientFrom.Height := h;
  pPaneGradientTo.Height := h;
  pPaneCaptionBackground.Height := h;
  pPaneCaptionFontColor.Height := h;
  pPaneCaptionFont.Height := h;

  pItemIdleFrame.Width := w;
  pItemHotTrackFrame.Width := w;
  pItemActiveFrame.Width := w;
  pItemIdleFrame.Height := h;
  pItemIdleGradientFrom.Height := h;
  pItemIdleGradientTo.Height := h;
  pItemIdleCaptionColor.Height := h;
  pItemIdleInnerDark.Height := h;
  pItemIdleInnerLight.Height := h;
  pItemFont.Height := h;

  Width := SpkScaleX(Width, 96);
  Height := SpkScaleY(Height, 96);
  Position := poScreenCenter;
end;

procedure TfrmAppearanceEditWindow.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if CanClose then CurrPageIndex := PageControl1.PageIndex;
end;

procedure TfrmAppearanceEditWindow.FormCreate(Sender: TObject);
begin
  bOK.AutoSize := false;
  bOK.Width := bCancel.Width;

  LargeImages.AddIcon(Application.Icon);
  SmallImages.AddIcon(Application.Icon);

  SmallImages.GetBitmap(0, bTabFrameColor.Glyph);
  SmallImages.GetBitmap(0, bTabGradientFromColor.Glyph);
  SmallImages.GetBitmap(0, bTabGradientToColor.Glyph);
  SmallImages.GetBitmap(0, bActiveTabHeaderFontColor.Glyph);
  SmallImages.GetBitmap(0, bInactiveTabHeaderFontColor.Glyph);

  SmallImages.GetBitmap(0, bPaneBorderDarkColor.Glyph);
  SmallImages.GetBitmap(0, bPaneBorderLightColor.Glyph);
  SmallImages.GetBitmap(0, bPaneGradientFromColor.Glyph);
  SmallImages.GetBitmap(0, bPaneGradientToColor.Glyph);
  SmallImages.GetBitmap(0, bPaneCaptionBackgroundColor.Glyph);
  SmallImages.GetBitmap(0, bPaneCaptionFontColor.Glyph);

  SmallImages.GetBitmap(0, bItemIdleCaptionColor.Glyph);
  SmallImages.GetBitmap(0, bItemIdleFrameColor.Glyph);
  SmallImages.GetBitmap(0, bItemIdleGradientFromColor.Glyph);
  SmallImages.GetBitmap(0, bItemIdleGradientToColor.Glyph);
  SmallImages.GetBitmap(0, bItemIdleInnerDarkColor.Glyph);
  SmallImages.GetBitmap(0, bItemIdleInnerLightColor.Glyph);

  SmallImages.GetBitmap(0, bItemHotTrackCaptionColor.Glyph);
  SmallImages.GetBitmap(0, bItemHotTrackFrameColor.Glyph);
  SmallImages.GetBitmap(0, bItemHotTrackGradientFromColor.Glyph);
  SmallImages.GetBitmap(0, bItemHotTrackGradientToColor.Glyph);
  SmallImages.GetBitmap(0, bItemHotTrackInnerDarkColor.Glyph);
  SmallImages.GetBitmap(0, bItemHotTrackInnerLightColor.Glyph);

  SmallImages.GetBitmap(0, bItemActiveCaptionColor.Glyph);
  SmallImages.GetBitmap(0, bItemActiveFrameColor.Glyph);
  SmallImages.GetBitmap(0, bItemActiveGradientFromColor.Glyph);
  SmallImages.GetBitmap(0, bItemActiveGradientToColor.Glyph);
  SmallImages.GetBitmap(0, bItemActiveInnerDarkColor.Glyph);
  SmallImages.GetBitmap(0, bItemActiveInnerLightColor.Glyph);

  PageControl1.PageIndex := CurrPageIndex;
end;

procedure TfrmAppearanceEditWindow.FormShow(Sender: TObject);
begin
  LoadAppearance(tbPreview.Appearance);
end;

function TfrmAppearanceEditWindow.GetAppearance: TSpkToolbarAppearance;
begin
  result := tbPreview.Appearance;
end;

procedure TfrmAppearanceEditWindow.LbAppearanceStyleClick(Sender: TObject);
begin
  tbPreview.Appearance.Reset(TSpkStyle(LbAppearanceStyle.ItemIndex));
  LoadAppearance(tbPreview.Appearance);
end;

procedure TfrmAppearanceEditWindow.LoadAppearance(AAppearance: TSpkToolbarAppearance);
begin
  with AAppearance do
  begin
    with Tab do
    begin
      SetPanelColor(pTabFrame, BorderColor);
      SetPanelColor(pTabGradientFrom, GradientFromColor);
      SetPanelColor(pTabGradientTo, GradientToColor);
      SetComboGradientKind(cbTabGradientKind, GradientType);
      SetPanelFont(pTabHeaderFont, TabHeaderFont);
      SetPanelColor(pActiveTabHeaderFont, TabHeaderFont.Color);
      SetPanelColor(pInactiveTabHeaderFont, InactiveTabHeaderFontColor);
    end;

    with Pane do
    begin
      SetPanelColor(pPaneBorderDark, BorderDarkColor);
      SetPanelColor(pPaneBorderLight, BorderLightColor);
      SetPanelColor(pPaneGradientFrom, GradientFromColor);
      SetPanelColor(pPaneGradientTo, GradientToColor);
      SetComboGradientKind(cbPaneGradientKind, GradientType);
      SetPanelColor(pPaneCaptionBackground, CaptionBgColor);
      SetPanelFont(pPaneCaptionFont, CaptionFont);
      SetPanelColor(pPaneCaptionFontColor, CaptionFont.Color);
      cbPaneStyle.ItemIndex := ord(Style);
      edPaneHotTrackBrightnessChange.Value := HotTrackBrightnessChange;
    end;

    with Element do
    begin
      SetPanelFont(pItemFont, CaptionFont);

      SetPanelColor(pItemIdleFrame, IdleFrameColor);
      SetPanelColor(pItemIdleGradientFrom, IdleGradientFromColor);
      SetPanelColor(pItemIdleGradientTo, IdleGradientToColor);
      SetComboGradientKind(cbItemIdleGradientKind, IdleGradientType);
      SetPanelColor(pItemIdleCaptionColor, IdleCaptionColor);
      SetPanelColor(pItemIdleInnerDark, IdleInnerDarkColor);
      SetPanelColor(pItemIdleInnerLight, IdleInnerLightColor);

      SetPanelColor(pItemHottrackFrame, HottrackFrameColor);
      SetPanelColor(pItemHottrackGradientFrom, HottrackGradientFromColor);
      SetPanelColor(pItemHottrackGradientTo, HottrackGradientToColor);
      SetComboGradientKind(cbItemHottrackGradientKind, HottrackGradientType);
      SetPanelColor(pItemHottrackCaptionColor, HottrackCaptionColor);
      SetPanelColor(pItemHottrackInnerDark, HottrackInnerDarkColor);
      SetPanelColor(pItemHottrackInnerLight, HottrackInnerLightColor);

      SetPanelColor(pItemActiveFrame, ActiveFrameColor);
      SetPanelColor(pItemActiveGradientFrom, ActiveGradientFromColor);
      SetPanelColor(pItemActiveGradientTo, ActiveGradientToColor);
      SetComboGradientKind(cbItemActiveGradientKind, ActiveGradientType);
      SetPanelColor(pItemActiveCaptionColor, ActiveCaptionColor);
      SetPanelColor(pItemActiveInnerDark, ActiveInnerDarkColor);
      SetPanelColor(pItemActiveInnerLight, ActiveInnerLightColor);

      cbItemStyle.ItemIndex := ord(Style);
      edItemHotTrackBrightnessChange.Value := HotTrackBrightnessChange;
    end;
  end;
end;

procedure TfrmAppearanceEditWindow.pItemActiveCaptionColorClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then begin
    tbPreview.Appearance.Element.ActiveCaptionColor:=(Sender as TPanel).Color;
    tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.pItemActiveFrameClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.ActiveFrameColor:=(Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemActiveGradientFromClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.ActiveGradientFromColor:=(Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.bImportClick(Sender: TObject);
var
  XML: TSpkXMLParser;
  Node: TSpkXMLNode;
begin
  tbPreview.BeginUpdate;
  XML := TSpkXMLParser.Create;
  try
    XML.Parse(PChar(mXML.text));
    Node := XML['Appearance', false];
    if assigned(Node) then
      tbPreview.Appearance.LoadFromXML(Node);
    LoadAppearance(tbPreview.Appearance);
  finally
    XML.Free;
    tbPreview.EndUpdate;
    tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.bResetClick(Sender: TObject);
begin
  tbPreview.Appearance.Reset;
  LoadAppearance(tbPreview.Appearance);
end;

procedure TfrmAppearanceEditWindow.cbItemActiveGradientKindChange(Sender: TObject);
begin
  with tbPreview.Appearance.Element do
    ActiveGradientType := TBackgroundKind((Sender as TCombobox).ItemIndex);
end;

procedure TfrmAppearanceEditWindow.pItemActiveGradientToClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.ActiveGradientToColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemActiveInnerDarkClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.ActiveInnerDarkColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemActiveInnerLightClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.ActiveInnerLightColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemHottrackCaptionColorClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then begin
    tbPreview.Appearance.Element.HotTrackCaptionColor := (Sender as TPanel).Color;
    tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.pItemHottrackFrameClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.HotTrackFrameColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemHottrackGradientFromClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.HotTrackGradientFromColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemHottrackGradientToClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.HotTrackGradientToColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemHottrackInnerDarkClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.HotTrackInnerDarkColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemHottrackInnerLightClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.HotTrackInnerLightColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemIdleCaptionColorClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then begin
    tbPreview.Appearance.Element.IdleCaptionColor := (Sender as TPanel).Color;
    tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.pItemIdleFrameClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Element.IdleFrameColor := (Sender as TPanel).Color;
    if cbLinkItem.Checked then
      SetLinkedFrameColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pItemIdleGradientFromClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Element.IdleGradientFromColor := (Sender as TPanel).Color;
    if cbLinkItem.Checked then
      SetLinkedGradientFromColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pItemIdleGradientToClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Element.IdleGradientToColor := (Sender as TPanel).Color;
    if cbLinkItem.Checked then
      SetLinkedGradientToColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pItemIdleInnerDarkClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.IdleInnerDarkColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemIdleInnerLightClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Element.IdleInnerLightColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pItemFontClick(Sender: TObject);
begin
  if ChangeFont(Sender as TPanel) then
    tbPreview.Appearance.Element.CaptionFont.Assign((Sender as TPanel).Font);
  tbPreview.ForceRepaint;
end;

procedure TfrmAppearanceEditWindow.pPaneBorderDarkClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Pane.BorderDarkColor := (Sender as TPanel).Color;
    if cbLinkPane.Checked then
      SetLinkedFrameColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pPaneBorderLightClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Pane.BorderLightColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pPaneCaptionBackgroundClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
    tbPreview.Appearance.Pane.CaptionBgColor := (Sender as TPanel).Color;
end;

procedure TfrmAppearanceEditWindow.pPaneCaptionFontClick(Sender: TObject);
begin
  if ChangeFont(Sender as TPanel) then
    tbPreview.Appearance.Pane.CaptionFont.Assign((Sender as TPanel).Font);
  tbPreview.ForceRepaint;
end;

procedure TfrmAppearanceEditWindow.pPaneCaptionFontColorClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
     tbPreview.Appearance.Pane.CaptionFont.Color:=((Sender as TPanel).Color);
     pPaneCaptionFont.Font.color:=((Sender as TPanel).Color);
     tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.pPaneGradientFromClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Pane.GradientFromColor:=(Sender as TPanel).Color;
    if cbLinkPane.Checked then
      SetLinkedGradientFromColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.cbPaneGradientKindChange(Sender: TObject);
begin
  with tbPreview.Appearance.Pane do
    GradientType := TBackgroundKind((Sender as TCombobox).ItemIndex);
  if cbLinkPane.Checked then
    SetLinkedGradientKind((Sender as TComboBox).ItemIndex);
end;

procedure TfrmAppearanceEditWindow.cbPaneStyleChange(Sender: TObject);
begin
  with tbPreview.Appearance.Pane do
    Style := TSpkPaneStyle((Sender as TCombobox).ItemIndex);
end;

procedure TfrmAppearanceEditWindow.pPaneGradientToClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Pane.GradientToColor:=(Sender as TPanel).Color;
    if cbLinkPane.Checked then
      SetLinkedGradientToColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pTabFrameClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Tab.BorderColor:=(Sender as TPanel).Color;
    if cbLinkTab.checked then
      SetLinkedFrameColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pTabGradientFromClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Tab.GradientFromColor := (Sender as TPanel).Color;
    if cbLinkTab.Checked then
      SetLinkedGradientFromColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pTabGradientToClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Tab.GradientToColor := (Sender as TPanel).Color;
    if cbLinkTab.Checked then
      SetLinkedGradientToColor((Sender as TPanel).Color);
  end;
end;

procedure TfrmAppearanceEditWindow.pTabHeaderFontClick(Sender: TObject);
begin
  if ChangeFont(Sender as TPanel) then begin
    tbPreview.Appearance.Tab.TabHeaderFont.Assign((Sender as TPanel).Font);
    tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.pActiveTabHeaderFontClick(Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Tab.TabHeaderFont.Color := (Sender as TPanel).Color;
    pTabHeaderFont.Font.Color := (Sender as TPanel).Color;
    tbPreview.ForceRepaint;
  end;
end;

procedure TfrmAppearanceEditWindow.pInactiveTabHeaderFontClick(
  Sender: TObject);
begin
  if ChangeColor(Sender as TPanel) then
  begin
    tbPreview.Appearance.Tab.InactiveTabHeaderFontColor := (Sender as TPanel).Color;
    tbPreview.ForceRepaint;
  end;
end;

function TfrmAppearanceEditWindow.PickColor(APanel: TPanel): Boolean;
var
  screenDC: HDC;
  cvOrigin: TPoint;
  rgbOrigin: TPoint;
  P: TPoint;
  img: TImage;
begin
  FScreenshotForm := TForm.Create(self);
  FScreenBitmap := TBitmap.Create;
  try
    screenDC := GetDC(0);
    try
      FScreenBitmap.LoadFromDevice(ScreenDC);
    finally
      ReleaseDC(0, screenDC);
    end;

    FScreenshotForm.BorderStyle := bsNone;
    FScreenshotForm.FormStyle := fsStayOnTop;
    FScreenshotForm.SetBounds(0, 0, Screen.Width, Screen.Height);

    img := TImage.Create(FScreenshotForm);
    img.Picture.Bitmap := FScreenBitmap;
    img.Parent := FScreenshotForm;
    img.Align := alClient;

    cvOrigin := ColorView.BoundsRect.TopLeft;
    P := ColorView.ClientToScreen(Point(0, 0));
    ColorView.Parent := FScreenshotForm;
    ColorView.Top := P.Y;
    ColorView.Left := P.X;
    ColorView.Show;

    rgbOrigin := LblRGB.BoundsRect.TopLeft;
    P := LblRGB.ClientToScreen(Point(0, 0));
    LblRGB.Parent := FScreenshotForm;
    LblRGB.Top := P.Y;
    LblRGB.Left := P.X;
    LblRGB.Show;

    //  Screen.Cursors[1] := LoadCursorFromLazarusResource('picker');
    FScreenshotForm.Cursor := crCross; //1;
    img.Cursor := crCross; //1;

    FScreenshotForm.OnKeyDown := ScreenshotKeyDown;
    img.OnMouseUp := ScreenshotMouseUp;
    img.OnMouseDown := ScreenshotMouseDown;
    img.OnMouseMove := ScreenshotMouseMove;

    Result := FScreenshotForm.ShowModal = mrOK;
    if Result then
      SetPanelColor(APanel, ColorView.Brush.Color);

    ColorView.Hide;
    ColorView.Top := cvOrigin.Y;
    ColorView.Left := cvOrigin.X;
    ColorView.Parent := ButtonPanel;

    LblRGB.Hide;
    LblRGB.Top := rgbOrigin.Y;
    LblRGB.Left := rgbOrigin.X;
    LblRGB.Parent := ButtonPanel;

  finally
    FScreenshotForm.Free;
    FScreenBitmap.Free;
  end;
end;

procedure TfrmAppearanceEditWindow.ScreenshotKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: FScreenshotForm.ModalResult := mrCancel;
    VK_RETURN: FScreenshotForm.ModalResult := mrOK;
  end;
end;

procedure TfrmAppearanceEditWindow.ScreenshotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  ColorView.Brush.Color := FScreenBitmap.Canvas.Pixels[X, Y];
  LblRGB.Caption := '$' + IntToHex(ColorView.Brush.Color, 8);
end;

procedure TfrmAppearanceEditWindow.ScreenshotMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  ColorView.Brush.Color := FScreenBitmap.Canvas.Pixels[X, Y];
  LblRGB.Caption := '$' + IntToHex(ColorView.Brush.Color, 8);
end;

procedure TfrmAppearanceEditWindow.ScreenshotMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FScreenshotForm.ModalResult := mrOK;
end;

end.


