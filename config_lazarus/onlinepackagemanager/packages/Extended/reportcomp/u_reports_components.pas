unit u_reports_components;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  Classes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  DBGrids, DB, fonctions_reports,
  fonctions_proprietes,
  RLReport, VirtualTrees,
  RLPreview, u_reportform,
  u_reports_rlcomponents,
  Printers, RLTypes,
{$IFDEF RX}
  rxdbgrid,
{$ENDIF}
{$IFDEF JEDI}
  jvDBGrid,
{$ENDIF}
  u_buttons_appli, RLFilters, Graphics;


{$IFDEF VERSIONS}
const
  gVer_reports_components: T_Version = (Component: 'Customized Reports Buttons';
    FileUnit: 'u_reports_components';
    Owner: 'Matthieu Giroux';
    Comment: 'Customized Reports Buttons components.';
    BugsStory: '1.1.3.0 : Print on File.' + #13#10 +
               '1.1.2.1 : Simplifying(need svn fortesreport for PrintData).' + #13#10 +
               '1.1.2.0 : Adding Orientation and PaperSize.' + #13#10 +
               '1.1.1.0 : TFWPrintGrid CreateReport porperty.' + #13#10 +
               '1.1.0.0 : TFWPrintData Component.' + #13#10 +
               '1.0.1.2 : No notification verify on destroy.' + #13#10 +
               '1.0.1.1 : Renaming DBFilter to Filter.' + #13#10 +
      #13#10 + '1.0.1.0 : Putting resize into extdbgrid columns.' +
      #13#10 + '1.0.0.0 : Tested.' + #13#10 +
               '0.9.0.0 : To test.';
    UnitType: 3;
    Major: 1; Minor: 1; Release: 3; Build: 0);
{$ENDIF}


type
  TExtPrintFile = ( pfPrinter, pfPDF, pfRTF );
  IFWPrintComp = interface
    ['{AD143A16-9635-4C81-B064-33BEF0946DA2}']
    procedure CreateAReport( const AReport : TRLReport );
  End;
  TFWPrintData = class;

  { TDataLinkPrint }
  TDataLinkPrint = class(TDataLink)
  private
    FOwner : TFWPrintData;
  protected
    procedure ActiveChanged; override;
  public
    constructor Create ( const AOwner :  TFWPrintData ); virtual;
  End;

  { TFWPrintData }
 // create a report from datasource
  TFWPrintData = class(TComponent,IFWPrintComp)
  private
    FBeforePrintCells : TGetCellPropsEvent;
    FFilter: TRLCustomPrintFilter;
    FDataLink: TDataLinkPrint;
    FDBTitle: string;
    FColumns: TExtPrintColumns;
    FReport : TRLReport;
    FReportForm : TReportForm;
    FPreview : TRLPReview;
    FOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF};
    FPaperSize     :TRLPaperSize;
    FPrinterType : TExtPrintFile;
    FPathOfFile : String;
    function GetFalse: Boolean;
    procedure SetDatasource(const AValue: TDatasource);
    function  GetDatasource: TDatasource;
    procedure SetColumns(const AValue: TExtPrintColumns);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCreateReport ( const AValue : Boolean ); virtual;
    function CreateColumns: TExtPrintColumns; virtual;
    procedure ActiveChanged; virtual;
    procedure AddColumns; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure CreateAReport( const AReport : TRLReport ); virtual;
    procedure Clear; virtual;
    procedure ShowPreview; virtual;
    procedure AddPreview ( const AReport : TRLReport = nil ); virtual;
    property  FormReport : TReportForm read FReportForm;
  published

    property OnGetCellProps: TGetCellPropsEvent read FBeforePrintCells write FBeforePrintCells;
    property Datasource: TDatasource read GetDatasource write SetDatasource;
    property Filter: TRLCustomPrintFilter read FFilter write FFilter;
    property DBTitle: string read FDBTitle write FDBTitle;
    property Columns: TExtPrintColumns read FColumns write SetColumns;
    property Report : TRLReport read FReport write FReport;
    property Preview : TRLPreview read FPreview write FPreview;
    property CreateReport : Boolean read GetFalse write SetCreateReport default False; // design only property
    property Orientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} read FOrientation write FOrientation default poPortrait;
    property PaperSize   :TRLPaperSize read FPaperSize write FPaperSize default fpA4;
    property PrinterType : TExtPrintFile read FPrinterType write FPrinterType default pfPrinter;
    property PathOfFile : String read FPathOfFile write FPathOfFile;
  end;

  { TFWPrintComp }
  // create a report from component ( abstract )
  TFWPrintComp = class(TFWPrint,IFWPrintComp)
  private
    FPathOfFile : String;
    FFilter: TRLCustomPrintFilter;
    FPrinterType : TExtPrintFile;
    FReportForm : TReportForm;
    FReport : TRLReport;
    FDBTitle: string;
    FPreview : TRLPReview;
    FOrientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF};
    FPaperSize     :TRLPaperSize;
  protected
    function GetFalse: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCreateReport ( const AValue : Boolean ); virtual;
  public
    procedure Clear; Virtual;
    constructor Create(AOwner: TComponent); override;
    procedure p_SetReport ( const Areport : TRLReport ); virtual;
    procedure CreateAReport( const AReport : TRLReport );  virtual; abstract;
    property  FormReport : TReportForm read FReportForm write FReportForm;
  published
    property Filter: TRLCustomPrintFilter read FFilter write FFilter;
    property DBTitle: string read FDBTitle write FDBTitle;
    property Preview : TRLPreview read FPreview write FPreview;
    property Report : TRLReport read FReport write FReport;
    property CreateReport : Boolean read GetFalse write SetCreateReport default False; // design only property
    property Orientation : {$IFDEF FPC}TPrinterOrientation{$ELSE}TRLPageOrientation{$ENDIF} read FOrientation write FOrientation default poPortrait;
    property PaperSize   :TRLPaperSize read FPaperSize write FPaperSize default fpA4;
    property PrinterType : TExtPrintFile read FPrinterType write FPrinterType default pfPrinter;
    property PathOfFile : String read FPathOfFile write FPathOfFile;
  end;


  { TFWPrintGrid }
  // create a report from grid
  TFWPrintGrid = class(TFWPrintComp)
  private
    FDBGrid: TCustomDBGrid;
    procedure SetDBGrid( const AValue: TCustomDBGrid);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure CreateAReport( const AReport : TRLReport ); override;
  published
    property DBGrid: TCustomDBGrid read FDBGrid write SetDBGrid;
  end;

  { TFWPrintPicture }

  TFWPrintPicture = class(TFWPrintComp)
  private
    FPicture: TPicture;
    procedure SetPicture( const AValue: TPicture);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure CreateAReport( const AReport : TRLReport ); override;
  published
    property Picture: TPicture read FPicture write SetPicture;
  end;

    { TFWPrintVTree }
    // create a report from virtual tree
  TFWPrintVTree = class(TFWPrintComp)
  private
    FTree : TCustomVirtualStringTree;
    procedure SetTree( const AValue: TCustomVirtualStringTree);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure CreateAReport( const AReport : TRLReport ); override;
  published
    property Tree: TCustomVirtualStringTree read FTree write SetTree;
  end;

procedure p_SetBtnPrint    ( const APrintComp   : TObject; const ATitle, APaperSizeText : String ;const ab_portrait : Boolean; const ai_doctype : Integer; const FBaseDir : String );
procedure p_SetPagePrinter ( const ARLPageSetup : TObject; const APrinter : Integer  );
procedure p_SetFileName    ( const ARLPageSetup : TObject; const AFileName : String  );
procedure p_SetPageSetup   ( const ARLPageSetup : TObject; const APaperSizeText : String  ); overload;
procedure p_SetPageSetup   ( const ARLPageSetup : TObject; const ab_portrait : Boolean ); overload;
procedure p_SetPageSetup ( const ARLPageSetup : TObject; const APaperSizeText : String ;const ab_portrait : Boolean); overload;
procedure p_PrintFile ( const AReport : TRLReport; const as_FilePathWithoutExt, As_Title : String; const apf_FileType : TExtPrintFile );
procedure p_SetButtonSetup ( const ARLPageSetup : TObject; const APaperSizeText : String ;const ab_portrait : Boolean ; const ai_doctype : Integer ; const FBaseDir : String);

implementation

uses Forms, SysUtils, typinfo,Dialogs,
{$IFDEF FPC}
     unite_messages, LazFileUtils,
{$ELSE}
     unite_messages_delphi,
{$ENDIF}
     Controls,
     fonctions_system,
     fonctions_string,
     fonctions_dialogs;

// print fortes report to file
procedure p_PrintFile ( const AReport : TRLReport; const as_FilePathWithoutExt, As_Title : String; const apf_FileType : TExtPrintFile );
var sd_PDFRTF: TSaveDialog;
Begin
  if apf_FileType = pfPrinter
   Then Exit
   Else sd_PDFRTF:=TSaveDialog.Create ( nil );

  with sd_PDFRTF do
   try
    case apf_FileType of
      pfPDF : DefaultExt:='.pdf';
      pfRTF : DefaultExt:='.rtf';
     end;

    filename:=As_Title+DefaultExt;
    InitialDir:=ExtractFileDir(as_FilePathWithoutExt);
    if not Execute Then
      Exit;

    if FileExistsUTF8(FileName)
    and ( MyMessageDlg(fs_RemplaceMsg(GS_DeleteFileNamed,[FileName]),mmtConfirmation,mmbYesNo)= mrNo )
     Then Exit
     Else DeleteFileUTF8(FileName);


    AReport.SaveToFile( sd_PDFRTF.FileName );

    p_OpenFileOrDirectory(sd_PDFRTF.FileName);

  finally
    sd_PDFRTF.Destroy;
  end;
End;
// From interface : setting report button
procedure p_SetBtnPrint  ( const APrintComp   : TObject; const ATitle, APaperSizeText : String ;const ab_portrait : Boolean ; const ai_doctype : Integer ; const FBaseDir : String );
Begin
  SetPropValue( APrintComp, 'DBTitle', ATitle );
  p_SetButtonSetup ( APrintComp, APaperSizeText, ab_portrait, ai_doctype, FBaseDir );
End;
// From interface : setting report button
procedure p_SetPageSetup ( const ARLPageSetup : TObject;const ab_portrait : Boolean );
Begin
  if ab_portrait
   Then SetPropValue( ARLPageSetup, 'Orientation', poPortrait )
   Else SetPropValue( ARLPageSetup, 'Orientation', poLandscape );
End;
// From interface : setting report button
procedure p_SetPageSetup ( const ARLPageSetup : TObject; const APaperSizeText : String  );
Begin
  if APaperSizeText <> '' then
   SetPropValue( ARLPageSetup, 'PaperSize', 'fp' + APaperSizeText );
End;
// From interface : setting report printer
procedure p_SetPagePrinter ( const ARLPageSetup : TObject; const APrinter : Integer  );
Begin
  SetPropValue( ARLPageSetup, 'PrinterType', APrinter );
End;
// From interface : setting report path
procedure p_SetFileName ( const ARLPageSetup : TObject; const AFileName : String  );
Begin
  if AFileName <> '' then
   SetPropValue( ARLPageSetup, 'PathOfFile', AFileName );
End;
// From interface : setting trlreport
procedure p_SetPageSetup ( const ARLPageSetup : TObject; const APaperSizeText : String ;const ab_portrait : Boolean);
Begin
  p_SetPageSetup   ( ARLPageSetup, ab_portrait );
  p_SetPageSetup   ( ARLPageSetup, APaperSizeText );
End;

// From interface : setting report button
procedure p_SetButtonSetup ( const ARLPageSetup : TObject; const APaperSizeText : String ;const ab_portrait : Boolean ; const ai_doctype : Integer ; const FBaseDir : String);
Begin
  p_SetPageSetup   ( ARLPageSetup,  APaperSizeText, ab_portrait);
  p_SetPagePrinter ( ARLPageSetup, ai_doctype );
  p_SetFileName    ( ARLPageSetup, FBaseDir);
End;

{ TFWPrintPicture }

procedure TFWPrintPicture.SetPicture(const AValue: TPicture);
begin
  FPicture.Assign(AValue);
end;

constructor TFWPrintPicture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture:=TPicture.Create;
end;

destructor TFWPrintPicture.Destroy;
begin
  inherited Destroy;
  FPicture.Destroy;
end;

procedure TFWPrintPicture.Click;
begin
  inherited Click;
  if Assigned(FReport)
   Then CreateAReport(FReport)
  Else
  if Assigned(FormReport)
   Then CreateAReport(FormReport.RLReport)
  Else
    FormReport := fref_CreateReport(Self, FPicture, FDBTitle, Orientation, PaperSize, FFilter);
  with FormReport  do
    Begin
      if FPrinterType = pfPrinter
       then RLReport.Preview(FPReview)
       Else p_PrintFile ( RLReport, FPathOfFile, FDBTitle, FPrinterType );

    end;
end;

procedure TFWPrintPicture.CreateAReport(const AReport: TRLReport);
begin
  p_SetReport(AReport);
  while AReport.ControlCount>0 do
    AReport.Controls[0].Free;
  fb_CreateReport(AReport,FPicture,FDBTitle);
end;

{ TFWPrintVTree }

// tree property setter
procedure TFWPrintVTree.SetTree(const AValue: TCustomVirtualStringTree);
begin
  if AValue <> FTree Then
   Begin
     FTree := AValue;
   end;
end;

// auto unlinking
procedure TFWPrintVTree.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) or (csDestroying in ComponentState) then
    exit;
  if (AComponent = FTree) then
    Tree := nil;
end;

// initing
constructor TFWPrintVTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTree := nil;
end;

// on click : report's preview
procedure TFWPrintVTree.Click;
var ADatasource : TDataSource;
begin
  inherited Click;
  if assigned(FTree) then
   Begin
     if Assigned(FReport)
      Then CreateAReport(FReport)
     Else
      Begin
        ADatasource := TDataSource(fobj_getComponentObjectProperty(FTree, CST_DBPROPERTY_DATASOURCE));
        if Assigned(ADatasource) Then ADatasource.DataSet.DisableControls;
        if FormReport = nil Then
          FormReport := fref_CreateReport( FTree, FDBTitle, FOrientation, FPaperSize, FFilter );
        try
          if FPrinterType = pfPrinter
           then FormReport.RLReport.Preview(FPReview)
           Else p_PrintFile ( FormReport.RLReport, FPathOfFile, FDBTitle, FPrinterType );
        finally
          if Assigned(ADatasource) Then ADatasource.DataSet.EnableControls;
        end;
      end;
   end;
end;

// for design
procedure TFWPrintVTree.CreateAReport(const AReport: TRLReport);
begin
  p_SetReport(AReport);
  if Assigned(FTree) Then
   p_CreateReportTree(AReport,FTree,AReport.Background.Picture.Bitmap.Canvas,DBTitle);
end;

{ TFWPrintComp }

// for design
function TFWPrintComp.GetFalse: Boolean;
begin
  Result := False;
end;

// auto unlinking
procedure TFWPrintComp.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) or (csDestroying in ComponentState) then
    exit;
  if (AComponent = FReport) then
    Report := nil;
  if (AComponent = Filter) then
    Filter := nil;
  if (AComponent = FPreview) then
    Preview := nil;
end;

// design report's creating property setter
procedure TFWPrintComp.SetCreateReport(const AValue: Boolean);
begin
  if  ( AValue =  True )
  and ( FReport <> nil ) Then
    CreateAReport ( FReport );
end;

procedure TFWPrintComp.Clear;
begin
  FreeAndNil(FReportForm);
end;

// initing
constructor TFWPrintComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrinterType := pfPrinter;
  FOrientation:=poPortrait;
  FPaperSize  :=fpA4;
  FDBTitle    :='';
  FFilter     := nil;
  FReport     := nil;
  FPreview    := nil;
end;

// properties set to report
procedure TFWPrintComp.p_SetReport(const Areport: TRLReport);
begin
  with Areport.PageSetup do
     Begin
       Orientation:=FOrientation;
       PaperSize  :=FPaperSize;
     end;
end;

{ TDataLinkPrint }

// designing only
procedure TDataLinkPrint.ActiveChanged;
begin
  inherited ActiveChanged;
  FOwner.ActiveChanged;
end;

// initing
constructor TDataLinkPrint.Create(const AOwner: TFWPrintData);
begin
  Inherited Create;
  FOwner:=AOwner;
end;

{ TFWPrintData }

// datasource property setter
procedure TFWPrintData.SetDatasource(const AValue: TDatasource);
begin
  if AValue <> FDataLink.DataSource then
  begin
    FDataLink.DataSource := AValue;
    AddColumns;
    FColumns.SetDatasource;
  end;
end;

// designing only
function TFWPrintData.GetFalse: Boolean;
begin
  Result := False;
end;

// datasource property getter
function TFWPrintData.GetDatasource: TDatasource;
begin
  Result := FDataLink.DataSource;
end;

// columns property setter
procedure TFWPrintData.SetColumns(const AValue: TExtPrintColumns);
begin
  FColumns.Assign(AValue);
end;

// designing only : create report
procedure TFWPrintData.AddColumns;
begin
  if Assigned(FDataLink.DataSet)
  and ( csDesigning in ComponentState )
  or ( FColumns.Count = 0 ) then
    with FDataLink.DataSet.FieldDefs do
      while FColumns.Count < Count do
        FColumns.Add;
end;

// unlinking
procedure TFWPrintData.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) or (csDestroying in ComponentState) then
    exit;
  if (AComponent = Datasource) then Datasource := nil;
  if (AComponent = FFilter   ) then Filter  := nil;
  if (AComponent = FReport   ) then Report  := nil;
  if (AComponent = FPreview  ) then Preview := nil;
end;

// designing only : create report
procedure TFWPrintData.SetCreateReport(const AValue: Boolean);
begin
  if  ( AValue =  True )
  and ( FReport <> nil )  Then
    CreateAReport ( FReport );
end;

// initing
function TFWPrintData.CreateColumns: TExtPrintColumns;
begin
  Result := TExtPrintColumns.Create(Self, TExtPrintColumn);
end;

// initing
constructor TFWPrintData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrientation:=poPortrait;
  FPaperSize  :=fpA4;
  FColumns := CreateColumns;
  FPrinterType := pfPrinter;
  FDataLink := TDataLinkPrint.Create(Self);
  FDBTitle := '';
  FFilter  := nil;
  FReport  := nil;
  FPreview := nil;
  FReportForm := nil;
end;

// free objects
destructor TFWPrintData.Destroy;
begin
  Clear;
  inherited Destroy;
  FDataLink.Free;
end;

// report's preview
procedure TFWPrintData.ShowPreview;
begin
  FDataLink.DataSet.DisableControls;
  try
    AddPreview(FReport);
    if assigned ( FReportForm ) Then
     if FPrinterType = pfPrinter
      then FReportForm.RLReport.Preview(FPreview)
      Else p_PrintFile ( FReportForm.RLReport, FPathOfFile, FDBTitle, FPrinterType );

  Finally
    FDataLink.DataSet.EnableControls;
  End;
end;

// create linked report
procedure TFWPrintData.CreateAReport ( const AReport : TRLReport );
begin
  fb_CreateReport(Self,AReport,nil, FDataLink.DataSource, FColumns, AReport.Background.Picture.Bitmap.Canvas, FDBTitle);
End;

procedure TFWPrintData.Clear;
begin
  FreeAndNil(FReportForm);
end;

// create report
procedure TFWPrintData.AddPreview(const AReport: TRLReport);
begin
  if assigned(FDataLink) then
    if AReport = nil
     Then
      Begin
         if FReportForm = nil Then
           FReportForm := fref_CreateReport(Self,nil, FDataLink.DataSource, FColumns, FDBTitle, FOrientation, FPaperSize, FFilter)
      end
     Else
       CreateAReport ( AReport );
end;

// auto add on design
procedure TFWPrintData.ActiveChanged;
begin
  AddColumns;
end;


{ TFWPrintGrid }

// grid property setter
procedure TFWPrintGrid.SetDBGrid(const AValue: TCustomDBGrid);
begin
  if AValue <> FDBGrid then
  begin
    FDBGrid := AValue;
  end;
end;

// auto unlinking
procedure TFWPrintGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) or (csDestroying in ComponentState) then
    exit;
  if (AComponent = FDBGrid) then
    DBGrid := nil;
end;

// automation , creating a report
procedure TFWPrintGrid.CreateAReport(const AReport: TRLReport);
begin
  p_SetReport(AReport);
  if Assigned(FDBGrid) Then
    fb_CreateReport(FDBGrid,AReport,FDBGrid, fobj_getComponentObjectProperty(FDBGrid,CST_DBPROPERTY_DATASOURCE) as TDataSource,
                                     fobj_getComponentObjectProperty(FDBGrid, CST_PROPERTY_COLUMNS  ) as TCollection,
                                     AReport.Background.Picture.Bitmap.Canvas,
                                     FDBTitle);
end;

// init
constructor TFWPrintGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBGrid     := nil;
end;

// on click : report's preview
procedure TFWPrintGrid.Click;
begin
  inherited Click;
  if assigned(FDBGrid) then
   if Assigned(FReport)
    Then
     Begin
       CreateAReport(FReport);
     end
   Else
    with TDataSource(
        fobj_getComponentObjectProperty(FDBGrid, CST_DBPROPERTY_DATASOURCE)).DataSet do
    begin
      DisableControls;
      if FormReport = nil Then
        FormReport := fref_CreateReport(FDBGrid, FDBGrid, TDataSource(
        fobj_getComponentObjectProperty(FDBGrid, CST_DBPROPERTY_DATASOURCE)), TCollection(
        fobj_getComponentObjectProperty(FDBGrid, CST_PROPERTY_COLUMNS)), FDBTitle, Orientation, PaperSize, FFilter);
      with FormReport  do
        try
          if FPrinterType = pfPrinter
           then RLReport.Preview(FPReview)
           Else p_PrintFile ( RLReport, FPathOfFile, FDBTitle, FPrinterType );

        finally
          EnableControls;
        end;
    end;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion(gVer_reports_components);
{$ENDIF}
end.
