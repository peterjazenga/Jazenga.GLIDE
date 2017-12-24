unit fonctions_reports;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf,
{$ENDIF}
  SysUtils, RLReport, DBGrids, DB,
  u_extdbgrid,U_ExtMapImageIndex,Forms,
  u_reportform,ImgList, Graphics,
  RLFilters,
  Printers,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes, Grids,
  u_reports_rlcomponents,
  VirtualTrees,
  RLTypes,
  IniFiles;

const
{$IFDEF VERSIONS}
  gVer_fonctions_reports : T_Version = ( Component : 'System management' ; FileUnit : 'fonctions_reports' ;
                        		 Owner : 'Matthieu Giroux' ;
                        		 Comment : 'Reports'' Functions, with grid reports.' ;
                        		 BugsStory : 'Version 1.0.2.1 : Panel header.' + #13#10 +
                                                     'Version 1.0.2.0 : Adding OnGetCellProps searching.' + #13#10 +
                                                     'Version 1.0.1.3 : Unfating.' + #13#10 +
                                                     'Version 1.0.1.2 : Simplifying.' + #13#10 +
                                                     'Version 1.0.1.1 : Testing tree.' + #13#10 +
                                                     'Version 1.0.1.0 : Ini management.' + #13#10 +
                                                     'Version 1.0.0.2 : Testing reports.' + #13#10 +
                                                     'Version 1.0.0.1 : image centering.' + #13#10 +
                                                     'Version 1.0.0.0 : Working.';
                        		 UnitType : 1 ;
                        		 Major : 1 ; Minor : 0 ; Release : 2 ; Build : 1 );
{$ENDIF}
  CST_COLUMN_Visible = 'Visible';
  CST_COLUMN_MIN_Width= 4;
  CST_COLUMN_Resize  = 'Resize';
  CST_COLUMN_Title   = 'Title';
  CST_COLUMN_Images  = 'Images';
  CST_PRINT_HEADER_FONT_NAME        = 'Tahoma';
  CST_PRINT_COLUMN_FONT_NAME        = 'Undotum';
  CST_PRINT_COLUMN_HEADER_FONT_NAME = 'Tahoma';
  CST_PRINT_FONT_SIZE = 9;
  CST_PRINT_FONT_SIZE_TREE = 9;
  CST_PRINT_HEADER_FONT_COLOR  = clMaroon;
  CST_PRINT_HEADER_COLOR = clLime;
  CST_PRINT_HEADER_BACK_COLOR = clSilver;
  CST_PRINT_COLUMN_HEADER_FONT_COLOR = clGreen;
  CST_PRINT_COLUMN_HEADER_BACK_COLOR = clMoneyGreen;
  CST_PRINT_COLUMN_FONT_COLOR = clBlack;
  CST_PRINT_COLUMN_BORDER_COLOR = clLime;
  CST_PRINT_COLUMN_COLOR = clWhite;
  CST_PRINT_TREE_LINE_COLOR = clMaroon;
  CST_PRINT_COLUMN_HEADER_FONT_STYLE = [fsBold];
  CST_PRINT_COLUMN_BREAKCAPTION = 'BreakCaption' ;
  CST_PRINT_COLUMN_LINEBREAK = 'LineBreak' ;
  CST_PRINT_COMPONENT_EVENT = 'DrawReportImage';
  CST_PRINT_INTERNAL_BAND_MARGIN = 1;
  CST_PRINT_INI_SECTION_REPORT = 'Reports' ;
  CST_PRINT_INI_COLOR_HEADER   = 'HeaderColor';
  CST_PRINT_INI_COLOR_COLUMN_HEADER   = 'ColumnHeaderColor';
  CST_PRINT_INI_COLOR_COLUMN   = 'ColumnColor';
  CST_PRINT_INI_COLOR_BORDER   = 'BorderColor';
  CST_PRINT_INI_COLOR_FONT_HEADER = 'HeaderFontColor';
  CST_PRINT_INI_COLOR_FONT_COLUMN = 'ColumnFontColor';
  CST_PRINT_INI_COLOR_FONT_COLUMN_HEADER = 'ColumnHeaderFontColor';
  CST_PRINT_INI_HEADER_FONT        = 'HeaderFont';
  CST_PRINT_INI_COLUMN_FONT        = 'ColumnFont';
  CST_PRINT_INI_COLUMN_HEADER_FONT = 'ColumnHeaderFont';
  CST_PRINT_INI_HBORDERS           = 'HorizontalBorders';
  CST_PRINT_INI_ROUNDED_BORDERS    = 'RoundedBorders';
  CST_PRINT_INI_VBORDERS           = 'VerticalBorders';



  // customized's reports
var RLLeftTopPage : TPoint = ( X: 20; Y:20 );
    ExtHeaderColorBack : TColor = CST_PRINT_HEADER_COLOR;
    ExtTitleColorBorder : TColor = CST_PRINT_COLUMN_BORDER_COLOR;
    ExtHeaderFont : TFont  = nil;
    ExtColorBorder : TColor = CST_PRINT_COLUMN_BORDER_COLOR;
    ExtColumnHeaderFont  : TFont  = nil;
    ExtColumnHeaderColorBack : TColor = CST_PRINT_COLUMN_HEADER_BACK_COLOR;
    ExtColumnFont        : TFont  = nil;
    ExtTreeLineColor     : TColor = CST_PRINT_TREE_LINE_COLOR;
    ExtColumnHBorders    : Boolean = False;
    ExtColumnVBorders    : Boolean = False;
    ExtColumnRoundedBorders : Boolean = True;
    ExtColumnColorBack   : TColor = CST_PRINT_COLUMN_COLOR;
    ExtLandscapeColumnsCount : Integer = 9;
    ExtHeader  : TRLBand = nil;

function fb_CreateReport ( const AReportComponent : TComponent; const AReport : TRLReport ; const agrid : TCustomDBGrid; const ADatasource : TDatasource; const AColumns : TCollection; const ATempCanvas : TCanvas;const as_Title : String): Boolean; overload;
function fb_CreateReport ( const AReport : TRLReport ; const aPicture : TPicture; const as_Title : String):Boolean; overload;
function fref_CreateReport ( const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil ): TReportForm; overload;
function fref_CreateReport ( const aReportComponent : TComponent; const agrid : TCustomDBGrid; const ADatasource : TDatasource; const AColumns : TCollection; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil): TReportForm; overload;
function fref_CreateReport ( const atree : TCustomVirtualStringTree; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil): TReportForm; overload;
function fref_CreateReport ( const aReportComponent : TComponent; const aPicture : TPicture; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil): TReportForm; overload;
function frlr_CreateNewReport ( const ASourceReport : TRLReport ):TRLReport;
procedure p_CreateAndPreviewReport ( const atree : TCustomVirtualStringTree; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil); overload;
procedure p_CreateAndPreviewReport ( const aReportComponent : TComponent; const agrid : TCustomDBGrid; const ADatasource : TDatasource; const AColumns : TCollection; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil); overload;
procedure p_CreateReportTree ( const AParentReport : TRLReport ; const atree : TCustomVirtualStringTree;const ATempCanvas : TCanvas;const as_Title : String);
procedure p_ReadReportsViewFromIni ( const AIniFile : TIniFile );
procedure p_WriteReportsViewToIni ( const AIniFile : TIniFile );
procedure p_ReinitValues;
function  fb_IsVisibleAPrintedColumn ( const AItem : TCollectionItem; const ADatasource : TDatasource = nil ) : Boolean;
function fcol_GetPrintedColor ( const AColor : TColor ) : TColor;

implementation

uses fonctions_proprietes,
     fonctions_images,
     fonctions_string,
     fonctions_vtree,
{$IFDEF RX}
     rxdbgrid,
{$ENDIF}
{$IFDEF JEDI}
  jvDBUltimGrid,
{$ENDIF}
     controls,
     Math;

function fcol_GetPrintedColor ( const AColor : TColor ) : TColor;
Begin
  case AColor of
    clWindowText,clCaptionText,clMenuText,clBtnText,clActiveBorder: Result:=clBlack;
    clWindow,clWindowFrame,clInactiveBorder,clBackground,clMenu,clNone: Result:=clWhite;
{$IFDEF FPC}
    clActiveBackground,clActiveButton,clActiveHighlight,
{$ENDIF}
    clBtnShadow,clMenuBar: Result:=clGray;
    clBtnFace,clBtnHighlight,clHighlight,clMenuHighlight: Result:=$404040;
{$IFDEF FPC}
    clActiveText,clActiveForeground,clActiveCaption,clActiveBrightText,clActiveButtonText : Result:=clRed;
    clActiveDark,clActiveHighlightedText,clHighlightedText,clActiveShadow:Result:=$8888FF;
{$ENDIF}
   Else
    Result:=AColor;
  end;
end;

procedure p_CreateAndPreviewReport ( const aReportComponent : TComponent; const agrid : TCustomDBGrid; const ADatasource : TDatasource; const AColumns : TCollection; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil); overload;
Begin
  with fref_CreateReport ( aReportComponent, agrid, ADatasource, AColumns, as_Title, AOrientation, APaperSize, acf_filter ) do
   try
     RLReport.Preview(nil);
   Finally
     Destroy;
   End;
End;

procedure p_CreateAndPreviewReport ( const atree : TCustomVirtualStringTree; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil); overload;
Begin
  with fref_CreateReport ( atree, as_Title, AOrientation, APaperSize, acf_filter ) do
   try
     RLReport.Preview(nil);
   Finally
     Destroy;
   End;
End;

procedure p_ReadReportsViewFromIni ( const AIniFile : TIniFile );
Begin
  ExtColumnColorBack       :=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_COLUMN,ExtColumnColorBack);
  ExtColorBorder     :=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_BORDER,ExtColorBorder);
  ExtColumnHeaderColorBack :=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_COLUMN_HEADER,ExtColumnHeaderColorBack);
  ExtHeaderColorBack       :=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_HEADER,ExtHeaderColorBack);
  ExtHeaderFont.Name       :=AIniFile.ReadString (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_HEADER_FONT,ExtHeaderFont.Name);
  ExtHeaderFont.Color      :=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_FONT_HEADER,ExtHeaderFont.Color);
  ExtColumnHeaderFont.Name :=AIniFile.ReadString (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLUMN_HEADER_FONT,ExtColumnHeaderFont.Name);
  ExtColumnHeaderFont.Color:=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_FONT_COLUMN_HEADER,ExtColumnHeaderFont.Color);
  ExtColumnFont.Color      :=AIniFile.ReadInteger(CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_FONT_COLUMN,ExtColumnFont.Color);
  ExtColumnFont.Name       :=AIniFile.ReadString (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLUMN_FONT,ExtColumnFont.Name);
  ExtColumnHBorders        :=AIniFile.ReadBool   (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_HBORDERS,ExtColumnHBorders);
  ExtColumnVBorders        :=AIniFile.ReadBool   (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_VBORDERS,ExtColumnVBorders);
  ExtColumnRoundedBorders  :=AIniFile.ReadBool   (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_ROUNDED_BORDERS,ExtColumnRoundedBorders);
end;


procedure p_WriteReportsViewToIni ( const AIniFile : TIniFile );
Begin
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_COLUMN,ExtColumnColorBack);
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_BORDER,ExtColorBorder);
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_COLUMN_HEADER,ExtColumnHeaderColorBack);
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_HEADER,ExtHeaderColorBack);
  AIniFile.WriteString  (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLUMN_HEADER_FONT,ExtColumnHeaderFont.Name);
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_FONT_HEADER,ExtHeaderFont.Color);
  AIniFile.WriteString  (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_HEADER_FONT,ExtHeaderFont.Name);
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_FONT_COLUMN,ExtColumnFont.Color);
  AIniFile.WriteInteger (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLOR_FONT_COLUMN_HEADER,ExtColumnHeaderFont.Color);
  AIniFile.WriteString  (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_COLUMN_FONT,ExtColumnFont.Name);
  AIniFile.WriteBool    (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_HBORDERS,ExtColumnHBorders);
  AIniFile.WriteBool    (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_VBORDERS,ExtColumnVBorders);
  AIniFile.WriteBool    (CST_PRINT_INI_SECTION_REPORT,CST_PRINT_INI_ROUNDED_BORDERS,ExtColumnRoundedBorders);
end;

procedure p_ReinitValues;
Begin
  ExtColumnColorBack       :=CST_PRINT_COLUMN_COLOR;
  ExtColumnHeaderColorBack :=CST_PRINT_COLUMN_HEADER_BACK_COLOR;
  ExtHeaderColorBack       :=CST_PRINT_HEADER_BACK_COLOR;
  ExtColumnHeaderFont.Name :=CST_PRINT_COLUMN_HEADER_FONT_NAME;
  ExtColumnHeaderFont.Color:=CST_PRINT_COLUMN_HEADER_FONT_COLOR;
  ExtColumnHeaderFont.Style:=CST_PRINT_COLUMN_HEADER_FONT_STYLE;
  ExtHeaderFont.Name       :=CST_PRINT_HEADER_FONT_NAME;
  ExtHeaderFont.Color      :=CST_PRINT_HEADER_FONT_COLOR;
  ExtColumnFont.Color      :=CST_PRINT_COLUMN_FONT_COLOR;
  ExtColumnFont.Name       :=CST_PRINT_COLUMN_FONT_NAME;
end;

function frlr_CreateNewReport ( const ASourceReport : TRLReport ):TRLReport;
Begin
  Result := TRLReport.Create(ASourceReport.Owner);
  with Result.PageSetup, ASourceReport do
   Begin
     Orientation:=PageSetup.Orientation;
     PaperSize  :=PageSetup.PaperSize;
     PaperHeight:=PageSetup.PaperHeight;
     PaperWidth :=PageSetup.PaperWidth;
   end;
end;

function frlc_createLabel ( const AReport : TRLReport; const ARLBand : TRLBand;const ALeft, ATop, AWidth : Integer ; const afont : TFont; const as_Text : String = '' ; const ai_SizeFont : Integer = 0 ): TRLLabel;
Begin
  Result := TRLLabel.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    Top:=ATop;
    Left:=ALeft;
    with Font do
     Begin
      assign ( afont );
       if ai_SizeFont <> 0 Then
        Size:=ai_SizeFont;
     end;
    if AWidth > 0 Then
     Begin
      AutoSize:=False;
      Width   :=AWidth;
     end;
    Caption:=as_Text;
   end;
end;

function frlc_createSystemInfo ( const AReport : TRLReport; const ARLBand : TRLBand; const ALeft, ATop, Awidth : Integer ; const AInfo:TRLInfoType; const afont : TFont; const AFontWidth : Integer = 0; const as_Text : String = '' ; const AAlign : TRLControlAlign = faRight ; const AAlignment : TRLTextAlignment = TRLTextAlignment(taRightJustify) ; const ALayout : TRLTextLayout = {$IFDEF FPC }TRLTextLayout.{$ENDIF}tlJustify):TRLSystemInfo;
Begin
  Result := TRLSystemInfo.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    HoldStyle:=hsRelatively;
    Top:=ATop;
    Left:=ALeft;
    with Font do
      Begin
        Assign(afont);
        if AFontWidth <> 0 Then
          Size:=AFontWidth;
      end;
    Align:=AAlign;
    Alignment:=AAlignment;
    Layout:=ALayout;
    Text:=as_Text;
    Info:=AInfo;
    if awidth <> 0 Then
     Begin
      AutoSize:=False;
      Width := Awidth;
     end;
   end;
end;

function frlc_createBand ( const AReport : TRLCustomControl; const Apanel : TRLPanel; const ALeft, ATop, Aheight : Integer ; const Abandtype : TRLBandType ; const AName : String = ''; const AColor : TColor = clWhite ) : TRLBand;
Begin
  Result := TRLBand.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=Apanel;
    if AName <> '' Then
      Name := AName;
    BandType:=Abandtype;
    Top:=ATop;
    Left:=ALeft;
    Color:=AColor;
    Height:=Aheight;
    Width:=aReport.Width-RLLeftTopPage.X+CST_PRINT_INTERNAL_BAND_MARGIN*2;
   end;
end;
function frlc_createPanel ( const AReport : TRLCustomControl; const ALeft, ATop, AWidth, Aheight : Integer ; const AName : String = ''; const AColor : TColor = clWhite ) : TRLPanel;
Begin
  Result := TRLPanel.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=AReport;
    if AName <> '' Then
      Name := AName;
    Top:=ATop;
    Left:=ALeft;
    Width := AWidth ;
    Color:=AColor;
    if Aheight = 0
     Then AutoSize:=True
     Else Height:=Aheight;
   end;
end;

function frlc_createDBText ( const AReport : TRLReport; const ARLBand : TRLBand; const ADatasource : TDatasource; const ALeft, ATop, AWidth : Integer ; const afont : TFont; const as_Fieldname, as_format : String ; const ai_SizeFont : Integer = 0):TRLDBText;
Begin
  Result := TRLDBText.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    DataSource:=ADatasource;
    Top:=ATop;
    Left:=ALeft;
    with Font do
     Begin
      Assign(afont);
      if ai_SizeFont > 0 Then
        Size:=ai_SizeFont;
     end;
    if AWidth > 0 Then
     Begin
       AutoSize:=False;
       Width:=AWidth;
     end;
    DataField:=as_Fieldname;
    if AReport is TExtReport Then
      BeforePrint:=(AReport as TExtReport).p_BeforePrintTexts;
    if ( as_format > '' )
    and ( ADatasource.DataSet.FieldByName(as_Fieldname) is TNumericField )Then
      TNumericField(ADatasource.DataSet.FieldByName(as_Fieldname)).DisplayFormat:=as_format;
   end;
end;

function frlc_createDBMemo ( const AReport : TRLReport; const ARLBand : TRLBand; const ADatasource : TDatasource; const ALeft, ATop, AWidth : Integer ; const afont : TFont; const as_Fieldname : String ; const ai_SizeFont : Integer = 0):TRLDBMemo;
Begin
  Result := TRLDBMemo.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    DataSource:=ADatasource;
    Top:=ATop;
    Left:=ALeft;
    with Font do
     Begin
      Assign(afont);
      if ai_SizeFont > 0 Then
        Size:=ai_SizeFont;
     end;
    if AWidth > 0 Then
     Begin
       AutoSize:=False;
       Width:=AWidth;
     end;
    if AReport is TExtReport Then
      BeforePrint:=(AReport as TExtReport).p_BeforePrintTexts;
    DataField:=as_Fieldname;
   end;
end;

procedure p_AdaptRLImage ( const ARLImage : TRLCustomImage ; const AImages : TCustomImageList; const AAlign : TAlign );
var LPoint : TPoint;
Begin
  with ARLImage do
   Begin
    LPoint := fPoi_FromAlignToCoord(AImages.Width,AImages.Height,Width,Height,AAlign);
    if LPoint.X > 0 Then
     Left:=Left+LPoint.X;
    if LPoint.Y > 0 Then
     Top:=Top+LPoint.Y;
   end;
end;

function frlc_createImage ( const AReport : TRLReport; const ARLBand : TWinControl; const ALeft, ATop, AWidth : Integer; const AColor : TColor ):TRLImage;
Begin
  Result := TRLImage.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    Top:=ATop;
    Left:=ALeft;
    Color:=AColor;
    Width:=AWidth;
   end;
end;

function frlc_createImageList ( const AReport : TRLReport; const ARLBand : TRLBand; const AImages : TCustomImageList; const ALeft, ATop, AWidth, AHeight, AImageIndex : Integer; const AColor : TColor):TRLExtImageList;
Begin
  Result := TRLExtImageList.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    Top:=ATop;
    Left:=ALeft;
    Width:=AWidth;
    Color:=AColor;
    Height:=AHeight;
    ImageIndex:=AImageIndex;
    Images := AImages;
   end;
  p_AdaptRLImage ( Result, AImages, alLeft );
end;

function frlc_createDBImage ( const AReport : TRLReport; const ARLBand : TRLBand; const ADatasource : TDatasource; const ALeft, ATop, AWidth, AHeight : Integer; const AField : String ; const AColor : TColor):TRLDBExtImage;
Begin
  Result := TRLDBExtImage.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    Top:=ATop;
    Left:=ALeft;
    Height := AHeight;
    Width:=AWidth;
    Color:=AColor;
    DataField:=AField;
    DataSource:=ADatasource;
   end;
end;

function frlc_createDBImageList ( const AReport : TRLReport; const ARLBand : TRLBand; const ADatasource : TDatasource; const ALeft, ATop, AWidth, AHeight : Integer; const AField : String ; const AColor : TColor; const AImages : TCustomImageList):TRLDBExtImageList;
Begin
  Result := TRLDBExtImageList.Create(AReport.Owner);
  with Result do
   Begin
    Parent:=ARLBand;
    Top:=ATop;
    Left:=ALeft;
    Height := AHeight;
    Width:=AWidth;
    Color:=AColor;
    DataField:=AField;
    DataSource:=ADatasource;
    Images := AImages;
   end;
  p_AdaptRLImage ( Result, AImages, alLeft );
end;

procedure p_DrawBorders ( const ABorders : TRLBorders ; const AColor : TColor; const ADrawLeft, ADrawRight, ADrawTop, ADrawBottom : Boolean );
Begin
   ABorders.Color := AColor;
   with ABorders do
    Begin
     DrawTop   :=ADrawTop;
     DrawBottom:=ADrawBottom;
    end;
   with ABorders do
    Begin
     DrawLeft   :=ADrawLeft;
     DrawRight  :=ADrawRight;
    end;
end;


// procedure p_AdaptBands;
// Compressing
procedure p_AdaptBands ( const ARLBand : TRLBand; const AHeight : Integer; const AIsFirst : Boolean ; const ALines : Integer = 1 );
Begin
  ARLBand.Margins.BottomMargin:=0;
  ARLBand.Margins.TopMargin   :=0;
  ARLBand.InsideMargins.BottomMargin:=0;
  ARLBand.InsideMargins.TopMargin   :=0;
  if not AIsFirst Then
    ARLBand.Height:=AHeight*ALines;
end;

// dbtitle property to report's header
procedure p_addTitle (const AReport : TRLReport ; const ABand : TRLBand; const as_Title : String; var atitleHeight : Integer ; const aEndOfName : String = '' );
var astl_Title : TStrings;
    i, AHeight, ALineHeight : Integer;
    arlabel : TRLLabel;
Begin
  astl_Title := nil;
  arlabel   := nil;
  ALineHeight := 0;
  p_ChampsVersListe(astl_Title, StringReplace ( as_Title, #10, '', [rfReplaceAll] ), #13);
  with AReport do
  for i := 0 to astl_Title.Count -1 do
   if astl_Title [ i ] <> '' Then
    ALineHeight := Max ( ALineHeight, Min ( 32, round ( ( Width - 160 ) div length ( astl_Title [ i ] )*1.95 )));
  atitleHeight := CST_PRINT_INTERNAL_BAND_MARGIN * 2 ;
  p_DrawBorders ( ABand.Borders, ExtTitleColorBorder, ExtColumnRoundedBorders, ExtColumnRoundedBorders, ExtColumnRoundedBorders, ExtColumnRoundedBorders );
  for i := 0 to astl_Title.Count -1 do
   Begin
     if arlabel = nil
      Then AHeight:=CST_PRINT_INTERNAL_BAND_MARGIN
      Else AHeight:=arlabel.Top+arlabel.Height;
     arlabel := frlc_createLabel ( AReport, ABand,CST_PRINT_INTERNAL_BAND_MARGIN,AHeight,0, ExtHeaderFont, astl_Title [ i ],aLineHeight*2 div 3);
     inc ( atitleHeight, arlabel.Height + 1 );
     if astl_Title.Count > 0
      Then arlabel.Name:='RLTitle'+IntToStr(i+1)+'_' + aEndOfName
      Else arlabel.Name:='RLTitle' + aEndOfName;
   end;
  ABand.Height:=atitleHeight;
end;

// entête de rapport
// report's header
function frlc_CreateHeader (const AReport : TRLReport ; const APanel : TRLPanel; const as_Title : String; var atitleHeight : Integer ; const aEndOfName : String = '' ):TRLBand;
var
    ARLSystemInfo : TRLSystemInfo;

Begin
  atitleHeight := 0;

  with AReport do
    if ExtHeader = nil Then
     Begin
       with RLLeftTopPage do
        Result := frlc_createBand ( AReport, APanel, X, Y, atitleHeight + 4, btHeader, 'RLHeader'+AEndOfName, ExtHeaderColorBack );
      if as_Title > '' Then    // title string ?
         p_addTitle ( AReport, Result, as_Title, atitleHeight, aEndOfName );

       with Result do
        Begin
         ARLSystemInfo := frlc_createSystemInfo ( AReport, Result,Width,CST_PRINT_INTERNAL_BAND_MARGIN,0,itFullDate, ExtHeaderFont, 0,'',faRightTop);
         ARLSystemInfo.Name:='RLDate'+AEndOfName;
         Height:=Max(ARLSystemInfo.Height*2,Height);  // adapt height to 2 lines of system info
         ARLSystemInfo := frlc_createSystemInfo ( AReport, Result,Width,Height,0,itLastPageNumber, ExtHeaderFont, 0, '/', faRightBottom,TRLTextAlignment(taLeftJustify));
         // due to autosize bug
         ARLSystemInfo.Anchors:=[fkRight,fkBottom];
         ARLSystemInfo.Width:=43;
         ARLSystemInfo.Left := Width - 44;
         ARLSystemInfo.Name:='RLLastPageNumber'+AEndOfName;
         ARLSystemInfo := frlc_createSystemInfo ( AReport, Result,Width,Height,0,itPageNumber, ExtHeaderFont, 0, '', faRightBottom);
         // due to autosize bug
         ARLSystemInfo.Anchors:=[fkRight,fkBottom];
         ARLSystemInfo.Width:=44;
         ARLSystemInfo.Left := Width - 88;
         ARLSystemInfo.Name:='RLPageNumber'+AEndOfName;
         atitleHeight := Height;
        end;
     end
    Else
    with ExtHeader do
     Begin
      Parent:=AReport;
      atitleHeight := Height;
     end;
end;

//   virtual tree sources
function NodeIsVisible(const Node: PVirtualNode): Boolean;

// Checks if a node will effectively be hidden as this depends on the nodes state and the paint options.

begin
  if Assigned(Node) then
    Result := ( vsVisible in Node.States )
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------
//   virtual tree sources
function HasVisibleNextSibling( Node: PVirtualNode): Boolean;

// Helper method to determine if the given node has a visible next sibling. This is needed to
// draw correct tree lines.

begin
  // Check if there is a sibling at all.
  Result := Assigned(Node.NextSibling);

  if Result then
  begin
    repeat
      Node := Node.NextSibling;
      Result := NodeIsVisible ( Node );
    until Result or (Node.NextSibling = nil);
  end;
end;


//   virtual tree sources
function HasVisiblePreviousSibling( Node: PVirtualNode): Boolean;

// Helper method to determine if the given node has a visible previous sibling. This is needed to
// draw correct tree lines.

begin
  // Check if there is a sibling at all.
  Result := Assigned(Node.PrevSibling);

  if Result then
  begin
    repeat
      Node := Node.PrevSibling;
      Result := NodeIsVisible ( Node );
    until Result or (Node.PrevSibling = nil);
  end;
end;

//   virtual tree sources
function IsFirstVisibleChild(Parent, Node: PVirtualNode): Boolean;

// Helper method to check if Node is the same as the first visible child of Parent.

var
  Run: PVirtualNode;

begin
  // Find first visible child.
  Run := Parent.FirstChild;
  while Assigned(Run) and not (NodeIsVisible ( Node )) do
    Run := Run.NextSibling;

  Result := Assigned(Run) and (Run = Node);
end;

function IsLastVisibleChild(Parent, Node: PVirtualNode): Boolean;

// Helper method to check if Node is the same as the last visible child of Parent.

var
  Run: PVirtualNode;

begin
  // Find last visible child.
  Run := Parent.LastChild;
  while Assigned(Run) and not (NodeIsVisible ( Node )) do
    Run := Run.PrevSibling;

  Result := Assigned(Run) and (Run = Node);
end;


//   virtual tree sources
function DetermineLineImagesAndSelectLevel( const ATree : TBaseVirtualTree; const ATreeOptions : TStringTreeOptions; const Node: PVirtualNode; out LineImage: TLineImage): Integer;

// This method is used during paint cycles and initializes an array of line type IDs. These IDs are used to paint
// the tree lines in front of the given node.
// Additionally an initial count of selected parents is determined and returned which is used for specific painting.

var
  X: Integer;
  Run: PVirtualNode;

begin
  Result := 0;
  with ATree, ATreeOptions do
   Begin
    if toShowRoot in PaintOptions then
      X := 1
    else
      X := 0;
    Run := Node;
    // Determine indentation level of top node.
    while Run.Parent <> RootNode do
    begin
      Inc(X);
      Run := Run.Parent;
      // Count selected nodes (FRoot is never selected).
      if vsSelected in Run.States then
        Inc(Result);
    end;

    // Set initial size of line index array, this will automatically initialized all entries to ltNone.
    SetLength(LineImage, X);

    // Only use lines if requested.
    if (toShowTreeLines in PaintOptions) then
    begin
        // Start over parent traversal if necessary.
        Run := Node;

        if Run.Parent <> RootNode then
        begin
          // The very last image (the one immediately before the item label) is different.
          if HasVisibleNextSibling(Run) then
            LineImage[X - 1] := ltTopDownRight
          else
            LineImage[X - 1] := ltTopRight;
          Run := Run.Parent;

          // Now go up all parents.
          repeat
            if Run.Parent = RootNode then
              Break;
            Dec(X);
            if HasVisibleNextSibling(Run) then
              LineImage[X - 1] := ltTopDown
            else
              LineImage[X - 1] := ltNone;
            Run := Run.Parent;
          until False;
        end;

        // Prepare root level. Run points at this stage to a top level node.
        if (toShowRoot in PaintOptions) and ((toShowTreeLines in PaintOptions)) then
        begin
          // Is the top node a root node?
          if Run = Node then
          begin
            // First child gets the bottom-right bitmap if it isn't also the only child.
            if IsFirstVisibleChild(RootNode, Run) then
              // Is it the only child?
              if IsLastVisibleChild(RootNode, Run) then
                LineImage[0] := ltRight
              else
                LineImage[0] := ltBottomRight
            else
              // real last child
              if IsLastVisibleChild(RootNode, Run) then
                LineImage[0] := ltTopRight
              else
                LineImage[0] := ltTopDownRight;
          end
          else
          begin
            // No, top node is not a top level node. So we need different painting.
            if HasVisibleNextSibling(Run) then
              LineImage[0] := ltTopDown
            else
              LineImage[0] := ltNone;
          end;
        end;
    end;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------
//   virtual tree sources
procedure p_DrawDottedHLine(const Canvas : TCanvas; const Left, Right, Top: Integer);
//   virtual tree sources
// Draws a horizontal line with alternating pixels (this style is not supported for pens under Win9x).

var
  R: TRect;

begin
  with Canvas do
  begin
    R := Rect(Min(Left, Right), Top, Max(Left, Right) + 1, Top + 1);
    FillRect( R );
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
//   virtual tree sources
procedure p_DrawDottedVLine(const Canvas : TCanvas; const Top, Bottom, Left: Integer);

// Draws a vertical line with alternating pixels (this style is not supported for pens under Win9x).

var
  R: TRect;

begin
  with Canvas do
  begin
    R := Rect(Left, Min(Top, Bottom), Left + 1, Max(Top, Bottom) + 1);
    FillRect( R );
  end;
end;


//----------------------------------------------------------------------------------------------------------------------
//   virtual tree sources
procedure p_DrawLineImage( const Canvas : TCanvas; const X, Y, H, VAlign, IndentTree : Integer;
                           const Style: TVTLineType;
                           const Reverse: Boolean);

// Draws (depending on Style) one of the 5 line types of the tree.
// If Reverse is True then a right-to-left column is being drawn, hence horizontal lines must be mirrored.
// X and Y describe the left upper corner of the line image rectangle, while H denotes its height (and width).

var
  HalfWidth,
  TargetX: Integer;

begin
  HalfWidth := Integer(IndentTree) div 2;
  if Reverse then
    TargetX := 0
  else
    TargetX := IndentTree;

  case Style of
    ltBottomRight:
      begin
        p_DrawDottedVLine(Canvas,Y + VAlign, Y + H, X + HalfWidth);
        p_DrawDottedHLine(Canvas,X + HalfWidth, X + TargetX, Y + VAlign);
      end;
    ltTopDown:
      p_DrawDottedVLine(Canvas,Y, Y + H, X + HalfWidth);
    ltTopDownRight:
      begin
        p_DrawDottedVLine(Canvas,Y, Y + H, X + HalfWidth);
        p_DrawDottedHLine(Canvas,X + HalfWidth, X + TargetX, Y + VAlign);
      end;
    ltRight:
      p_DrawDottedHLine(Canvas,X + HalfWidth, X + TargetX, Y + VAlign);
    ltTopRight:
      begin
        p_DrawDottedVLine(Canvas,Y, Y + VAlign, X + HalfWidth);
        p_DrawDottedHLine(Canvas,X + HalfWidth, X + TargetX, Y + VAlign);
      end;
    ltLeft: // left can also mean right for RTL context
      if Reverse then
        p_DrawDottedVLine(Canvas,Y, Y + H, X + Integer(IndentTree))
      else
        p_DrawDottedVLine(Canvas,Y, Y + H, X);
    ltLeftBottom:
      if Reverse then
      begin
        p_DrawDottedVLine(Canvas,Y, Y + H, X + Integer(IndentTree));
        p_DrawDottedHLine(Canvas,X, X + Integer(IndentTree), Y + H);
      end
      else
      begin
        p_DrawDottedVLine(Canvas,Y, Y + H, X);
        p_DrawDottedHLine(Canvas,X, X + Integer(IndentTree), Y + H);
      end;
  end;
end;


//----------------------------------------------------------------------------------------------------------------------
//   virtual tree sources
procedure p_PaintTreeLines(const Canvas : TCanvas; const CellRect : TRect; const BidiMode : TBiDiMode; const VAlignment, IndentSize, NodeHeight, IndentTree: Integer;
 const LineImage: TLineImage);

var
  I: Integer;
  XPos,
  Offset: Integer;
  NewStyles: TLineImage;

begin
  NewStyles := nil;

  if BidiMode = bdLeftToRight then
  begin
    XPos := CellRect.Left;
    Offset := IndentTree;
  end
  else
  begin
    Offset := -Integer(IndentTree);
    XPos := CellRect.Right + Offset;
  end;

  for I := 0 to IndentSize - 1 do
    begin
      p_DrawLineImage(Canvas,  XPos, CellRect.Top, NodeHeight, VAlignment, IndentTree, LineImage[I],
        BidiMode <> bdLeftToRight);
      Inc(XPos, Offset);
    end;
end;

// create a report from a virtual tree
procedure p_CreateReportTree ( const AParentReport : TRLReport ; const atree : TCustomVirtualStringTree;const ATempCanvas : TCanvas;const as_Title : String);
var atitleHeight, aSpaceWidth: Integer;
    ARLLabel : TRLLabel;
    AReport : TRLReport;
    ARLImage : TRLCustomImage;
    ARLBand : TRLBand;
    ATreeNodeSigns : TLineImage;
    AImages : TCustomImageList;
    ATreeOptions: TStringTreeOptions;
    ARightReport  : TRLReport;
    ARightPanel  : TRLPanel;
    AHeaderPanel  : TRLPanel;
    ARightReports : array of TRLReport;
    LMinusBM : TBitmap;
    {$IFDEF FPC}
    AText : String;
    {$ELSE}
    AText : UnicodeString;
    {$ENDIF}
    AGhosted : Boolean;
    ATextHeight,ATextWidth : Integer;
    ATreeLevel : Integer;
    AOnGetImage: TVTGetImageEvent;               // Used to retrieve the image index of a given node.
    AOnGetImageEx: TVTGetImageExEvent;           // Used to retrieve the image index of a given node along with a custom
    LIndex : Word;

    const CST_PROPERTY_OnGetImageIndex = 'OnGetImageIndex';
          CST_PROPERTY_OnGetImageIndexEX = 'OnGetImageIndexEx';

    // add header to a report
    function frlp_AddHeader ( const AReport : TRLReport;const aEndOfName : String ):TRLPanel ;
    Begin
      with AReport, Margins, RLLeftTopPage do
       Begin
//        ATempBand := frlc_createBand ( AReport, APanel, 0, Y + atitleHeight + 1, 4, btHeader );
        Result := frlc_createPanel ( AReport, 0, Y + atitleHeight + 1, 0, ClientHeight - round ( BottomMargin + TopMargin ) - 1, 'RLColumnHeader'+aEndOfName, ExtColumnColorBack  );
///        p_DrawBorders(ABand.Borders,ExtColorBorder,ExtColumnRoundedBorders,ExtColumnRoundedBorders,
//                                                   ExtColumnRoundedBorders,ExtColumnRoundedBorders);
       end;
      ARLBand:=frlc_CreateHeader ( AReport, Result, as_Title, ATitleHeight, aEndOfName );

      inc ( atitleHeight, ARLBand.Height );
    end;

    // text can go right out
    procedure p_addEventualRightReport ( const ARealTop : Integer );
    var Aindex      : Integer;
        ADotWidth, ALeftLength      : Integer;
        ARightString : String;
    Begin
       ARightString := '';
       with ARLLabel do
         Begin
          if Width + Left > ARLBand.Width Then
           Begin
             if not Assigned( ARightReport ) Then
              Begin
                ARightReport := frlr_CreateNewReport ( AParentReport );
                AIndex:=High(ARightReports)+1;
                SetLength(ARightReports, AIndex + 1 );
                ARightReports[AIndex]:=ARightReport;
                if AIndex > 0 Then
                 ARightReport.PriorReport := ARightReports [ AIndex - 1 ];
                ARightPanel := frlp_AddHeader ( ARightReport, 'Right' + IntToStr(LIndex) );
                with AReport,Margins do
                  ARLBand:=frlc_createBand ( AReport, ARightPanel, 0, atitleHeight, ClientHeight - atitleHeight - round ( BottomMargin + TopMargin ) - 1,btSummary, 'RLTree' + IntToStr(Aindex) );
              end;
             ADotWidth := ATempCanvas.TextWidth('…')+CST_PRINT_INTERNAL_BAND_MARGIN * 2+Left;
             ALeftLength := Length(Caption);
             while ( Width + ADotWidth > ARLBand.Width )
             and ( ALeftLength > 0 ) do
              Begin
                ARightString := Caption [ ALeftLength ]+ ARightString;
                dec ( ALeftLength );
                Caption:= copy ( Caption, 1, ALeftLength );
              end;
             Caption:= Caption + '…';
             Arlband:=frlc_createBand(AReport,ARightPanel,0,0,30,btheader,'RightBand' + IntToStr(LIndex),ExtColorBorder);
             frlc_createLabel(ARightReport,ARLBand,CST_PRINT_INTERNAL_BAND_MARGIN,ARealTop,ARLBand.Width-CST_PRINT_INTERNAL_BAND_MARGIN,ExtColumnFont,ARightString);
           end;
         end;

    end;

    // New left and eventual right page
    procedure p_BeginPage;
    Begin
      ARLLabel := nil;
      ARightReport := nil;
      ATitleHeight := 0;
      inc ( LIndex );
      AHeaderPanel := frlp_AddHeader ( AReport, IntToStr(LIndex));
      with AReport,Margins do
        ARLBand:=frlc_createBand ( AReport, AHeaderPanel, 0, atitleHeight, ClientHeight - atitleHeight - round ( BottomMargin + TopMargin ) - 1,btSummary, 'RLTree' + IntToStr(Lindex) );
    end;

    // Paint only the tree's main column
    procedure p_paintMainColumn ( const ANode : PVirtualNode );
    var Arect : TRect;
        ARealTop : Integer;
        LLine : TBitmap;
        AIndex : Integer;
        ANodeText:TVTPaintText;
    Begin
      AIndex:= -1;
      with atree, ARect do
       Begin
        Left:=0;
        Top :=0;
        LLine := TBitmap.Create;
         try

          Right:=aSpaceWidth;// right align
          Bottom:= ATextHeight;
          if ARLLabel = nil
           Then ARealTop := 1
           Else ARealTop := ARLLabel.Top+ ATextHeight +1 ;
          ARLImage := frlc_createImage(AReport, ARLBand, 1, ARealTop, aSpaceWidth - 1, ExtColumnColorBack );
          // erase black canvas
          p_SetAndFillBitmap ( LLine, Right, Bottom, ARLImage.Color );
          //   virtual tree sources
          DetermineLineImagesAndSelectLevel( atree, ATreeOptions, ANode, ATreeNodeSigns );
          if (toShowTreeLines in ATreeOptions.PaintOptions) then
            Begin
              LLine.Canvas.Brush.Color := ExtTreeLineColor;
              p_PaintTreeLines(LLine.Canvas,Arect, bdLeftToRight, ATextHeight div 2, ATreeLevel+1, ATextHeight, ATextWidth, ATreeNodeSigns);
            end;
          // place the minus
          LLine.Canvas.Draw(aSpaceWidth - ATextWidth + ( ATextWidth - LMinusBM.Width ) div 2 + 1, (ATextHeight - LMinusBM.Height ) div 2 + 1, LMinusBM);
          ARLImage.Picture.Bitmap.Assign(LLine);
         finally
           LLine.Free;
         end;
        // Main column image
        AImages := fobj_getComponentObjectProperty ( atree, CST_PROPERTY_IMAGES ) as TCustomImageList;
        Left:=aSpaceWidth;
        if (( AImages <> nil ) and  Assigned ( AOnGetImage ))
        or Assigned ( AOnGetImageEx ) Then
         Begin
           // First try the enhanced event to allow for custom image lists.
           if Assigned(AOnGetImageEx) then
             AOnGetImageEx(atree, ANode, ikNormal, -1, AGhosted, AIndex, AImages )
           else
             if Assigned(AOnGetImage) then
               AOnGetImage(atree, ANode, ikNormal, -1, AGhosted, AIndex);
           // report's image
           if ATextWidth < AImages.Width
            Then ARLImage := frlc_createImageList(AReport, ARLBand, AImages, aSpaceWidth, ARealTop, ATextWidth, ATextHeight, AIndex, ExtColumnColorBack )
            Else ARLImage := frlc_createImageList(AReport, ARLBand, AImages, aSpaceWidth, ARealTop+ ( ATextWidth - AImages.Width ) div 2, AImages.Width, AImages.Height, AIndex, ExtColumnColorBack );
           Left:=Left+ARLImage.Width;
           inc ( aSpaceWidth, ARLImage.Width );
         end;
        // Main column text
        GetTextInfo(ANode,-1,ARLBand.Font,ARect,AText);
        with ARect do
          ARLLabel := frlc_createLabel(AReport,ARLBand,aSpaceWidth,ARealTop,0,ExtColumnFont,AText);
        if  Assigned(fmet_getComponentMethodProperty(atree,'OnPaintText').Code)
         Then
          with ARLLabel do
            Begin
              ANodeText:=TVTPaintText(fmet_getComponentMethodProperty(atree,'OnPaintText'));
              ATempCanvas.Brush.Color := Brush.Color;
              ATempCanvas.Font.Assign(Font);
              ANodeText ( atree, ATempCanvas, ANode, 0, ttNormal);
              Font.Assign(ATempCanvas.Font);
              Brush.Color:=fcol_GetPrintedColor (ATempCanvas.Brush.Color);
            end;
        // text can go right out
        p_addEventualRightReport ( ARealTop );
       end;
    end;

    // recursive reports' print procedure
    procedure p_labelNode ( const ANode : PVirtualNode );
    var APriorReport : TRLReport;
    Begin
      ATreeLevel := GetNodeLevel(ANode,atree.RootNode)+1;
      with atree, ANode^ do
       Begin
        if ANode <> RootNode Then
         Begin
          aSpaceWidth := (ATreeLevel-1)*ATextWidth+1;
          p_paintMainColumn ( ANode );
          if  ARLLabel.Top + ( ARLLabel.Height + 1) * 2 > ARLBand.Height Then
            Begin
              APriorReport := AReport;
              areport := frlr_CreateNewReport (AParentReport);
              areport.PriorReport:=APriorReport;
              p_BeginPage;
            end;
         end;
        if FirstChild <> nil  Then p_labelNode(FirstChild);
        if  ( ANode <> RootNode )
        and ( NextSibling <> nil )
          Then p_labelNode(NextSibling);
       end;
    end;
    // draw tree's minus
    procedure p_drawMinus ( const ABitmap : TBitmap );
    var AInterleaving : Integer;
        // shadowed
        procedure p_drawPosition ( const X, Y : Integer ; const AColor1, AColor2 : TColor );
        Begin
          with ABitmap,Canvas do
           Begin
            Pen.Color := AColor1;
            Rectangle(X, Y, Width, Height);
            Pen.Color := AColor2;
            MoveTo(AInterleaving+X, Width div 2+Y);
            LineTo(Width - AInterleaving+X , Width div 2+Y);
           End;
        end;

    Begin
      with ABitmap do
       Begin
        AInterleaving:= round ( ATextWidth / 2.5 ) ;
        p_SetAndFillBitmap(ABitmap,ATextWidth - AInterleaving+1,ATextWidth - AInterleaving+1,clWhite);
        dec ( AInterleaving, ATextWidth div 4 );
        p_drawPosition ( 1, 1, clGray, clGray );
        p_drawPosition ( 0, 0, ExtTreeLineColor, ExtColumnFont.Color );
       End;
    end;

Begin
  LIndex:=0;
  AReport := AParentReport;
  AReport.ForcePrepare:= False;
  LMinusBM := TBitmap.Create;
  ATempCanvas.Font.Assign(ExtColumnFont);
  // Max text height for lines height
  ATextHeight := ATempCanvas.TextHeight('W')+1;
  ATextWidth  := ATempCanvas.TextWidth('W')+1;
  ARLLabel := nil;
  AGhosted := False;
  // from viewed HMI tree
  AOnGetImage   := TVTGetImageEvent   ( fmet_getComponentMethodProperty ( atree, CST_PROPERTY_OnGetImageIndex   ));
  AOnGetImageEx := TVTGetImageExEvent ( fmet_getComponentMethodProperty ( atree, CST_PROPERTY_OnGetImageIndexEX ));
  try
    p_drawMinus ( LMinusBM );  // draw tree's minus
    p_BeginPage;
    ATreeOptions := TStringTreeOptions ( fobj_getComponentObjectProperty(atree,'TreeOptions'));
    ATempCanvas.Font.Assign(ExtColumnFont);
    p_labelNode ( atree.RootNode );
    // add right's reports at the and, so can not print them
    if high ( ARightReports ) > 0 Then
      AReport.NextReport := ARightReports[0];
  finally
    lMinusBM.Free;
  end;
end;

// really show column ?
function fb_IsVisibleAPrintedColumn ( const AItem : TCollectionItem; const ADatasource : TDatasource ) : Boolean;
Begin
  Result :=    fb_getComponentBoolProperty ( AItem, CST_COLUMN_Visible, True )
         and ( fli_getComponentProperty ( AItem, CST_PROPERTY_WIDTH ) > CST_COLUMN_MIN_Width )
         and ( not Assigned(ADatasource) or assigned ( ADatasource.DataSet.FindField(fs_getComponentProperty ( AItem, CST_DBPROPERTY_FIELDNAME ))));

end;


// create a datasource or grid report
function fb_CreateReport ( const AReportComponent : TComponent ; const AReport : TRLReport ; const agrid : TCustomDBGrid; const ADatasource : TDatasource; const AColumns : TCollection; const ATempCanvas : TCanvas;const as_Title : String): Boolean;
var totalgridwidth, aresizecolumns, ALastVisible,
    AlastColumnAddedSize, ALastResizedColumn,
    ATitleHeight, aVisibleColumns, SomeLeft,
    aWidth, ALinesAddedHeader, ALinesAddedColumns : Integer;
    ARLControl : TRLCustomControl;
    ARLBand : TRLBand;
    AHeaderPanel : TRLpanel;
    LImages : TCustomImageList;

  // resize property
  function fi_resize ( const ai_width, aindex : Integer ):Integer;
  Begin
    if  fb_getComponentBoolProperty ( aColumns.Items [ aindex ], CST_COLUMN_Resize, True )
     Then
       Begin
         if aindex = ALastResizedColumn
          Then Result := ai_width+aresizecolumns+AlastColumnAddedSize
          Else Result := ai_width+aresizecolumns;
       end
     Else Result:=ai_Width ;
  end;

  // calculate lines and widths
  procedure PreparePrint;
  var i, ATemp : Integer;
      lcountedcolumn : Boolean;
  Begin
    ALinesAddedHeader:=0;
    ALinesAddedColumns:=0;
    aresizecolumns  := 0 ;
    aVisibleColumns := 0;
    totalgridwidth  := 0;
    lcountedcolumn := True;
    with AReport do
     Begin
      Clear;
      for i := 0 to aColumns.Count - 1 do
       with aColumns do
        Begin
          if fb_IsVisibleAPrintedColumn ( Items [ i ], ADatasource ) Then
           Begin
            if lcountedcolumn Then
             Begin
              // add width and visible to printed columns
              inc ( totalgridwidth, fli_getComponentProperty(Items [ i ], CST_PROPERTY_WIDTH ) );
              inc ( aVisibleColumns );
              if fb_getComponentBoolProperty ( Items [ i ], CST_COLUMN_Resize, True )
                Then
                 Begin
                   ALastResizedColumn := i;
                   inc ( aresizecolumns );
                 end;
             end;
           end;
          // is there a line break on column ?
          ATemp := fli_getComponentProperty ( Items [ i ], CST_PRINT_COLUMN_LINEBREAK, -1 );
          if  ( ATemp > -1 )
          and ( ATemp < i  )
           Then
            Begin
              inc ( ALinesAddedColumns );
              lcountedcolumn := False;
            end;
        end;
     End;
  end;

  // header and prepare report's columns
  procedure CreateHeaderAndPrepare;
  var i, j, AHeight : Integer;
      LIsFirst : Boolean;
      Alines : Integer;
      LString : TStringAArray;
  Begin
   ATempCanvas.font.Assign(ExtColumnFont);
   AHeight := ATempCanvas.TextHeight('W');
   with AReport do
    try
      Alines := 1;
      AHeaderPanel:=frlc_createPanel(AReport,0,0,AReport.ClientWidth,ATitleHeight,'RLHeaderPanel');
      frlc_CreateHeader ( AReport, AHeaderPanel, as_Title, ATitleHeight );
      with RLLeftTopPage do
       Begin
        ARLBand := frlc_createBand ( AReport, AHeaderPanel, 0, Y + atitleHeight, 4, btHeader );
        inc ( atitleHeight, 4 );
        ARLBand := frlc_createBand ( AReport, AHeaderPanel, 0, Y + atitleHeight, 30, btColumnHeader, 'RLColumnHeader', ExtColumnHeaderColorBack  );
        p_DrawBorders(ARLBand.Borders,ExtColorBorder,ExtColumnRoundedBorders,ExtColumnRoundedBorders,
                                                     ExtColumnRoundedBorders,ExtColumnRoundedBorders);
        AHeaderPanel.Height:=ARLBand.Top+ARLBand.Height;
       end;
      ATempCanvas.font.Assign(ExtColumnHeaderFont);
      if aresizecolumns > 0 Then
       Begin
        AlastColumnAddedSize := ( ARLBand.Width - totalgridwidth - 2 ) mod aresizecolumns;
        aresizecolumns:= ( ARLBand.Width - totalgridwidth - 2 ) div aresizecolumns;
       end;
      ALastVisible := -1;
      LIsFirst := True;
      with aColumns do
      for i := 0 to Count - 1 do
       if fb_IsVisibleAPrintedColumn ( Items [ i ], ADatasource ) Then
       Begin
        ALastVisible := i;
        awidth:=fi_resize ( fli_getComponentProperty ( Items [ i ], CST_PROPERTY_WIDTH ), i );
        if agrid = nil
         Then LString := fs_SeparateTextFromWidth(fs_getComponentProperty(Items [ i ], 'DBTitle'),aWidth,ATempCanvas,'| ')
         Else LString := fs_SeparateTextFromWidth((fobj_getComponentObjectProperty(Items [ i ], CST_COLUMN_Title) as {$IFDEF FPC}TGridColumnTitle{$ELSE}TColumnTitle{$ENDIF}).caption,aWidth,ATempCanvas,'| ');
        if high ( LString ) > ALinesAddedHeader Then
         ALinesAddedHeader:=high ( LString );
       end;
      SomeLeft:=1;
      with aColumns do // report's columns preparing
      for i := 0 to Count - 1 do
       Begin
         if fb_IsVisibleAPrintedColumn ( Items [ i ], ADatasource ) Then
           Begin
            if fs_getComponentProperty ( Items [ i ], CST_PRINT_COLUMN_BREAKCAPTION ) <> '' Then
               inc ( SomeLeft, ATempCanvas.TextWidth(fs_getComponentProperty ( Items [ i ], CST_PRINT_COLUMN_BREAKCAPTION )) );
            awidth:=fi_resize ( fli_getComponentProperty ( Items [ i ], CST_PROPERTY_WIDTH ), i );
            if agrid = nil
             Then LString := fs_SeparateTextFromWidth(fs_getComponentProperty(Items [ i ], 'DBTitle'),aWidth,ATempCanvas,'| ')
             Else LString := fs_SeparateTextFromWidth((fobj_getComponentObjectProperty(Items [ i ], CST_COLUMN_Title) as {$IFDEF FPC}TGridColumnTitle{$ELSE}TColumnTitle{$ENDIF}).caption,aWidth,ATempCanvas,'| ');
  //          RLColumnHeaderFont.GetTextSize(LString,Apos,j);
             for j := 0 to ALinesAddedHeader do
              Begin
               if j <= high ( LString )
                Then ARLControl := frlc_createLabel ( AReport, ARLBand,SomeLeft,CST_PRINT_INTERNAL_BAND_MARGIN+j*AHeight,aWidth, ExtColumnHeaderFont, LString [ j ] )
                Else ARLControl := frlc_createLabel ( AReport, ARLBand,SomeLeft,CST_PRINT_INTERNAL_BAND_MARGIN+j*AHeight,aWidth, ExtColumnHeaderFont, '' );
               p_DrawBorders ( ARLControl.Borders, ExtColorBorder, LIsFirst and ExtColumnVBorders and ( not ExtColumnRoundedBorders or ExtColumnHBorders ),
                                                                   ExtColumnVBorders and ( not ExtColumnRoundedBorders or ExtColumnHBorders  or ( ALastVisible <> i )),
                                                                   ExtColumnHBorders, ExtColumnHBorders );
              end;
             if high ( LString ) + 1 > Alines Then
              Alines:= high ( LString )+1;
             inc ( SomeLeft, aWidth );
             LIsFirst := False;
           end;
        if fli_getComponentProperty ( Items [ i ], CST_PRINT_COLUMN_LINEBREAK ) > -1 Then
         Break;
       end;

    finally
    end;
   p_AdaptBands ( ARLBand, AHeight, LIsFirst, Alines );
   ARLBand.Height:=ARLBand.Height+CST_PRINT_INTERNAL_BAND_MARGIN*2;
  end;

  // borders and line break
  procedure p_DesignCell(const ARLControl : TRLCustomControl; const AItemIndex : Integer ;var AIsFirst : Boolean );
  Begin
    p_DrawBorders ( ARLControl.Borders, ExtColorBorder, AIsFirst and ExtColumnVBorders and ( not ExtColumnRoundedBorders or ExtColumnHBorders ),
                                                        ExtColumnVBorders and ( not ExtColumnRoundedBorders or ExtColumnHBorders  or ( ALastVisible <> AItemIndex )),
                                                        False, ExtColumnHBorders );
  end;

  // set a printed field
  procedure p_CreatePrintField ( const AItem : TCollectionItem ; var AIsFirst : Boolean; const ATop, Aheight : Integer ; const AIWidth, AItemIndex : Integer ; const Adataset : TDataset ; const ASBreakCaption : String = '' );
  Begin
    if assigned ( AItem ) Then
     Begin
      LImages:= fobj_getComponentObjectProperty ( AItem,CST_PROPERTY_IMAGES ) as TCustomImageList;
      if assigned(LImages) Then
       Begin
         ARLControl := frlc_createDBImageList ( AReport, ARLBand, ADataSource, SomeLeft,ATop,aiWidth,AHeight, fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME),ExtColumnColorBack,LImages);
         with ARLControl as TRLCustomDBExtImageList do
          Begin
            OnGetImageIndex := TFieldIndexEvent (fmet_getComponentMethodProperty( AItem, 'OnGetImageIndex' ));
            MapImages := fobj_getComponentObjectProperty( AItem, 'MapImages' ) as TExtMapImages;
          end;
         p_DesignCell( ARLControl, AItemIndex, AIsFirst);
       end
      Else
       Begin
        if Adataset.FieldByName(fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME)) is TBlobField
         Then Begin ARLControl := frlc_createDBImage ( AReport, ARLBand, ADataSource, SomeLeft,ATop,aiWidth, AHeight  , fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME), ExtColumnColorBack); p_DesignCell( ARLControl , AItemIndex, AIsFirst ); End
         else if Adataset.FieldByName(fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME)) is TMemoField
         Then Begin ARLControl := frlc_createDBMemo ( AReport, ARLBand, ADataSource, SomeLeft,ATop,aiWidth, ExtColumnFont, fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME)); p_DesignCell( ARLControl, AItemIndex, AIsFirst ); End
         Else Begin ARLControl := frlc_createDBText ( AReport, ARLBand, ADataSource, SomeLeft,ATop,aiWidth, ExtColumnFont, fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME), fs_getComponentProperty( AItem, CST_PROPERTY_DISPLAYFORMAT)); p_DesignCell( ARLControl, AItemIndex, AIsFirst ); End;
         if Adataset.FieldByName(fs_getComponentProperty( AItem, CST_DBPROPERTY_FIELDNAME)) is TFloatField
          Then ( ARLControl as TRLDBText ).DisplayMask := '#9'+{$IFDEF FORMATSETTING}FormatSettings.{$ENDIF}DecimalSeparator+'9';
       end
    End
   Else
     Begin
      ARLControl := frlc_createLabel ( AReport, ARLBand, SomeLeft,ATop,aiWidth, ExtColumnFont, ASBreakCaption );
      p_DesignCell( ARLControl, AItemIndex, AIsFirst );
     end
  end;

  // initing columns
  procedure p_InitList(var ATop, ATextHeight : Integer; var AIsFirst : Boolean );
  var APanel : TRLPanel;
      AHeight : Integer;
  Begin
    SomeLeft:=1;
    with RLLeftTopPage, AReport.ClientRect do
     Begin
      if ExtColumnRoundedBorders
      and ( not ExtColumnHBorders or not ExtColumnVBorders )
       Then
          Begin
           AHeight:=AReport.ClientHeight-ATitleHeight;
           if not ExtColumnHBorders Then
            dec (AHeight,(AHeight  mod (ATextHeight + 1)));
           APanel := frlc_createPanel ( AReport, Left,
                                                 Top+ATitleHeight+2,
                                                 AReport.ClientWidth,
                                                 AHeight,
                                        'RLBorders', ExtColumnColorBack );
           p_DrawBorders ( APanel.Borders, ExtColorBorder, True, True, True, True );
          end
       Else
       APanel := frlc_createPanel ( AReport, Left,
                                             Top+ATitleHeight+2,
                                             AReport.ClientWidth,
                                             AHeight,
                                    'RLBorders' );
      ARLBand := frlc_createBand ( AReport, APanel, X, Y + atitleHeight, 30, btDetail, 'RLColumn', ExtColumnColorBack );
      ARLBand.Parent := APanel;

     end;

    AIsFirst := True;
    ATop := 0;
  end;
  procedure p_Linebreak ( const AItem : TCollectionItem; var AHeight, ATop, Aline, ADecColumn : Integer; var AIsFirst : Boolean );
  var ABreak, j : Integer;
      LSBreakCaption : String;
  Begin
    ABreak := fli_getComponentProperty ( AItem, CST_PRINT_COLUMN_LINEBREAK );
    if assigned ( AItem )
    and ( ABreak > -1 )
    and ( ABreak < ADecColumn )Then
     Begin
       LSBreakCaption:=fs_getComponentProperty(AItem,CST_PRINT_COLUMN_BREAKCAPTION);
       inc ( Atop, AHeight );
       inc(Aline);
       ADecColumn := ABreak - 1;
       SomeLeft:=1;
       aWidth:=0;
       AIsFirst:=True;
       // aligning
       with AColumns do
         for j := 0 to ADecColumn do
           if fb_IsVisibleAPrintedColumn ( items [ j ], ADatasource ) Then
              Begin
                inc(awidth,fi_resize ( fli_getComponentProperty ( Items [ j ], CST_PROPERTY_WIDTH ), j ));
               end;
       if aWidth > 0 Then
          Begin
            p_CreatePrintField ( nil, AIsFirst,ATop,AHeight,AWidth, ADecColumn,ADataSource.DataSet,LSBreakCaption);
            ARLControl.Alignment:=TRLTextAlignment(taRightJustify);
            ARLControl.Font.Assign(ExtColumnHeaderFont);
            inc(SomeLeft,aWidth);
            AIsFirst:=False;
          end;
     end;
  End;

  // Print Grid ( TFWPrintGrid )
  procedure CreateListGrid;
  var i,ATop, Aheight : Integer;
      LIsFirst : Boolean;
  Begin
    AHeight  := ATempCanvas.TextHeight('W');
    ATop     := 0;
    LIsFirst := True;
    with AReport do
     Begin
      p_InitList ( ATop, Aheight, LIsFirst );
      with aColumns do
      for i := 0 to Count - 1 do
         if fb_IsVisibleAPrintedColumn ( Items [ i ], ADatasource ) Then
           Begin
             awidth:=fi_resize ( fli_getComponentProperty ( Items [ i ], CST_PROPERTY_WIDTH ), i );
             p_CreatePrintField ( Items [ i ], LIsFirst,ATop,Aheight,AWidth,i,ADataSource.DataSet);
             inc ( SomeLeft, aWidth );
             LIsFirst := False;
           end;
      End;
    p_AdaptBands ( ARLBand, Aheight, LIsFirst );
  end;
  // Print Datasource ( TFWPrintData )
  procedure CreateListPrint;
  var i,Aline,ATop, AHeight, ADecColumn : Integer;
      LIsFirst : Boolean;
  Begin
    ALine      := 0;
    AHeight    := ATempCanvas.TextHeight('W');
    ADecColumn := -1;
    LIsFirst   := True;
    ATop       := 0;
    with ADatasource.DataSet,AReport do
     Begin
      p_InitList ( ATop, AHeight, LIsFirst );
      with aColumns do
      for i := 0 to Count - 1 do
       Begin
        inc ( ADecColumn ); // for linebreak
        if fb_IsVisibleAPrintedColumn ( Items [ i ], ADatasource ) Then
         Begin
           awidth:=fi_resize ( fli_getComponentProperty ( Items [ ADecColumn ], CST_PROPERTY_WIDTH ), ADecColumn );
           p_CreatePrintField ( Items [ i ], LIsFirst,ATop,Aheight,AWidth,i,ADataSource.DataSet);
           // linebreak ?
           if fli_getComponentProperty ( Items [ i ], CST_PRINT_COLUMN_LINEBREAK ) < 0 Then
            Begin
              inc ( SomeLeft, aWidth );
              LIsFirst := False;
            end;
         end;
        // optional line break
        p_Linebreak ( Items [ i ], AHeight, ATop, Aline , ADecColumn, LIsFirst );
       End;

      End;
    p_AdaptBands ( ARLBand, AHeight, LIsFirst, ALinesAddedColumns + 1 );
  end;
Begin
  if AReport is TExtReport Then
    (AReport as TExtReport).PrintComponent:=AReportComponent;
  Result := False;
  PreparePrint;
  if totalgridwidth = 0 Then Exit;
  CreateHeaderAndPrepare;
  if agrid  = nil
   Then CreateListPrint
   Else CreateListGrid;
  AReport.DataSource:=ADatasource;
end;
//create picture report
function fb_CreateReport ( const AReport : TRLReport ; const aPicture : TPicture; const as_Title : String):Boolean;
var ARLPanel : TRLPanel;
    ARLBand : TRLBand;
    ATitleHeight : Integer;
    ARLImage : TRLImage;
Begin
  ATitleHeight := 0;
  AReport.ForcePrepare:=False;
  if as_Title > '' Then
   Begin
     ARLPanel := frlc_createPanel(AReport,0,0,AReport.ClientWidth,AReport.ClientHeight,'RLHeaderPanel');
     ARLBand:=frlc_CreateHeader ( AReport, ARLPanel, as_Title, ATitleHeight );
   end;
  ARLImage:=frlc_createImage(AReport, AReport, RLLeftTopPage.X+CST_PRINT_INTERNAL_BAND_MARGIN,ATitleHeight+CST_PRINT_INTERNAL_BAND_MARGIN*2,AReport.Width-RLLeftTopPage.X+CST_PRINT_INTERNAL_BAND_MARGIN*2,clWhite);
  with ARLImage do
   Begin
    Align:=faClient;
    Stretch:=True;
    Scaled :=True;
    Picture:=aPicture;
   end;
end;

// create a blank report's form
function fref_CreateReport ( const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil ): TReportForm;
Begin
  Result := TReportForm.create ( Application );
  with Result.RLReport, PageSetup do
   Begin
    DefaultFilter:=acf_filter;
    Orientation:=AOrientation;
    PaperSize:=APaperSize;
   end;
End;

// main create tree report
function fref_CreateReport ( const atree : TCustomVirtualStringTree; const as_Title : String; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil ): TReportForm;
Begin
  Result := fref_CreateReport ( AOrientation, APaperSize, acf_filter );
  p_CreateReportTree ( Result.RLReport, atree, Result.Canvas, as_Title );
end;

// main create image report's form
function fref_CreateReport ( const aReportComponent : TComponent; const aPicture : TPicture; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil): TReportForm;
Begin
  Result := fref_CreateReport ( AOrientation, APaperSize, acf_filter );
  fb_CreateReport ( Result.RLReport, aPicture, as_Title );
end;

// main create grid or data report's form
function fref_CreateReport ( const aReportComponent : TComponent; const agrid : TCustomDBGrid; const ADatasource : TDatasource; const AColumns : TCollection; const as_Title : String ; const AOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} = poPortrait ; const APaperSize   :TRLPaperSize = fpA4; const acf_filter : TRLCustomPrintFilter = nil): TReportForm;
Begin
  Result := fref_CreateReport ( AOrientation, APaperSize, acf_filter );
  fb_CreateReport ( aReportComponent, Result.RLReport, agrid, ADatasource, AColumns, Result.Canvas, as_Title );
end;

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_reports );
{$ENDIF}
  // customized's reports
  ExtHeaderFont       := TFont.create;
  ExtColumnHeaderFont := TFont.Create;
  ExtColumnFont       := TFont.Create;
  ExtColumnFont      .Size  := CST_PRINT_FONT_SIZE;
  ExtColumnHeaderFont.Size  := CST_PRINT_FONT_SIZE;
  ExtHeaderFont      .Size  := CST_PRINT_FONT_SIZE;
  p_ReinitValues;
finalization
  ExtHeaderFont      .Free;
  ExtColumnHeaderFont.Free;
  ExtColumnFont      .Free;
end.
