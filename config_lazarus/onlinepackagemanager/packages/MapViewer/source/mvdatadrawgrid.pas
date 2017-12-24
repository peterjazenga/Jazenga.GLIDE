unit mvdatadrawgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,mvgrids,memds,db;

type
  TMinMax = record
             Min : Integer;
             Max : integer;
            End;

  TMVDataDrawGrid = class;

  TMeasureCellWidthEvent = procedure (Sender : TMVDataDrawGrid;ACanvas : TCanvas;Acol,ARow : integer;Var WidthNeeded : integer) of object;
  TLockUnlockDataSourceEvent = Procedure (ToLock : Boolean) of Object;

  { TMVDataDrawGrid }

  TMVDataDrawGrid = class(TCustomDrawGrid)
  private
    FOnLockUnlock: TLockUnlockDataSourceEvent;
    Refreshing : boolean;
    FDescr : Array of String;
    FDataFields : Array of String;
    FDataSource: TMemDataSet;
    FOnMeasureCellWidth: TMeasureCellWidthEvent;
    MinMax : array of TMinMax;
    function GetFieldFor(ACol: Integer): TField;
    procedure SetDataSource(AValue: TMemDataSet);
    Procedure ClearFields;
  protected
    FUpdateCount : integer;
    function DataSourceOk : boolean;
    function  GetCells(ACol, ARow: Integer): string; override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure MeasureCellWidth(ACanvas : TCanvas;ACol,ARow : integer;out WidthNeeded : Integer);
    procedure CalculateAllMinMaxIfNeeded;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure CalcAutoSizeColumn(const Index: Integer; var AMin,AMax,APriority: Integer); override;
    procedure AutoAdjustColumn(aCol: Integer); override;
    procedure SetDataSourceRow(aRecNo : integer);
    procedure VisualChange; override;
    procedure Paint; override;
  public
    Procedure LockDatasource;
    Procedure UnlockDataSource;
    constructor Create(AOwner : TComponent);override;
    procedure RefreshFromDataSource;
    procedure ShowField(const lst: array of string; const Descr: array of string);
    procedure CallBeginUpdate; reintroduce;
    procedure CallEndUpdate; reintroduce;
    Procedure GetSelections(out Selecteds : TGridRectArray);
    Procedure ClearSelection;
  published
    property OnMeasureCellWidth : TMeasureCellWidthEvent read FOnMeasureCellWidth write FOnMeasureCellWidth;
    property DataSource : TMemDataSet read FDataSource write SetDataSource;
    Property OnLockUnlock : TLockUnlockDataSourceEvent Read FOnLockUnlock Write FOnLockUnlock;

    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property AutoFillColumns;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property ColCount;
    property ColumnClickSorts;
    property Columns;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property FixedCols;
    property FixedRows;
    property Flat;
    property Font;
    property GridLineWidth;
    property HeaderHotZones;
    property HeaderPushZones;
    property MouseWheelOption;
    property Options;
    property ParentColor default false;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RangeSelectMode;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabAdvance;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;

    property OnBeforeSelection;
    property OnCheckboxToggled;
    property OnClick;
    property OnColRowDeleted;
    property OnColRowExchanged;
    property OnColRowInserted;
    property OnColRowMoved;
    property OnCompareCells;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnButtonClick;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCheckboxState;
    property OnGetEditMask;
    property OnGetEditText;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnHeaderSizing;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPickListSelect;
    property OnPrepareCanvas;
    property OnSelectEditor;
    property OnSelection;
    property OnSelectCell;
    property OnSetCheckboxState;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopleftChanged;
    property OnUserCheckboxBitmap;
    property OnUTF8KeyPress;
  end;


implementation

const MARGINLEFT = 2 ;
      MARGINRIGHT = 2 ;

{ TMVDataDrawGrid }

procedure TMVDataDrawGrid.SetDataSource(AValue: TMemDataSet);
begin
  if FDataSource=AValue then Exit;
  FDataSource:=AValue;
  SetLength(MinMax,0);
  RefreshFromDataSource;
end;

procedure TMVDataDrawGrid.ClearFields;
begin
   SetLength(FDataFields,0);
   SetLength(FDescr,0);
end;

procedure TMVDataDrawGrid.LockDatasource;
begin
  if Assigned(FOnLockUnlock) then
     FOnLockUnlock(true);
end;

procedure TMVDataDrawGrid.UnlockDataSource;
begin
  if Assigned(FOnLockUnlock) then
     FOnLockUnlock(false);
end;

function TMVDataDrawGrid.DataSourceOk: boolean;
begin
  Result:=Assigned(DataSource) and (DataSource.Active) and (DataSource.Fields.Count>0);
end;

function TMVDataDrawGrid.GetFieldFor(ACol : Integer) : TField;
Begin
  Result:=nil;
  if DataSourceOk then
    if (ACol>=low(FDataFields)) and (ACol<=high(FDataFields)) then
    Begin
       if FDataFields[ACol]<>'' then
         Result:=DataSource.FieldByName(FDataFields[ACol]);
    end;
end;

function TMVDataDrawGrid.GetCells(ACol, ARow: Integer): string;

Procedure ReadFromDescr;
Begin
  if (ACol>=low(FDescr)) and (ACol<=high(FDescr)) then
    Result:=FDescr[aCol];
end;

var aField : TField;
begin
  if FUpdateCount>0 then
  Begin
    Result:=inherited GetCells(ACol,ARow);
    Exit;
  end;
  if DataSourceOk then
  Begin
    aField:=GetFieldFor(aCol);
    if Assigned(aField) then
    Begin
      if ARow=0 then
      Begin
        ReadFromDescr;
        if Result='' then
          Result:=aField.FieldName;
      end
      else
      Begin
        if (DataSource.RecNo<>ARow) then
           DataSource.RecNo:=ARow;
        if AField.IsNull then
          Result:=''
        else
          Result:=AField.AsString;
        if Assigned(AField.OnGetText) then
          AField.OnGetText(AField,Result,true);
      end;
    end;
  end
  Else
    if ARow=0 then
      ReadFromDescr;
  if Result='' then
    Result:=inherited GetCells(ACol, ARow);
end;

procedure TMVDataDrawGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  SetLength(MinMax,0);
  inherited SizeChanged(OldColCount, OldRowCount);
end;

procedure TMVDataDrawGrid.MeasureCellWidth(ACanvas: TCanvas; ACol, ARow: integer; out WidthNeeded: Integer);
var aText : String;
begin
  aText:=GetCells(Acol,ARow);
  WidthNeeded:=ACanvas.TextWidth(aText)+MARGINLEFT+MARGINRIGHT;
  if Assigned(OnMeasureCellWidth) then
     OnMeasureCellWidth(self,ACanvas,ACol,ARow,WidthNeeded);
end;

procedure TMVDataDrawGrid.CalculateAllMinMaxIfNeeded;
var x,y : integer;
    WidthNeeded : integer;
    tmpCanvas : TCanvas;
begin
  //minMax is Erased each time the grid is resized or refreshed
  if Length(MinMax)<>ColCount then
  Begin
    LockDataSource;
    Try
      tmpCanvas := GetWorkingCanvas(Canvas);
      try
        SetLength(MinMax,ColCount);
        For x:=low(MinMax) to high(MinMax) do
        Begin
          MinMax[x].Min:=0;
          MinMax[x].Max:=0;
        end;
        if RowCount>0 then
        Begin
          For x:=Low(MinMax) to high(MinMax) do
          Begin
            MeasureCellWidth(tmpCanvas,X,0,WidthNeeded);
            MinMax[x].Min:=WidthNeeded;
            MinMax[x].Max:=WidthNeeded;
          End;
          For y:=1 to pred(RowCount) do
          Begin
            SetDataSourceRow(y);
            For x:=Low(MinMax) to high(MinMax) do
            Begin
              MeasureCellWidth(tmpCanvas,X,y,WidthNeeded);
              if WidthNeeded<MinMax[x].Min then
                 MinMax[x].Min:=WidthNeeded;
              if WidthNeeded>MinMax[x].Max then
                 MinMax[x].Max:=WidthNeeded;
            end;
          end;
        end;
      finally
        if tmpCanvas<>Canvas then
          FreeWorkingCanvas(tmpCanvas);
      end;
    finally
      UnlockDataSource;
    end;
  End;
end;

procedure TMVDataDrawGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var aTxt : String;
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  aTxt:=GetCells(ACol, ARow);
  Canvas.TextRect(aRect,aRect.Left+MARGINLEFT,ARect.Top,aTxt);
end;

procedure TMVDataDrawGrid.CalcAutoSizeColumn(const Index: Integer; var AMin,
  AMax, APriority: Integer);
begin
   AMin:=0;
   AMax:=0;
   APriority:=0;
   CalculateAllMinMaxIfNeeded;
   if (Index>=low(MinMax)) and (Index<=high(MinMax)) then
   Begin
     AMin:=MinMax[Index].Min;
     AMax:=MinMax[Index].Max;
   end;
end;

procedure TMVDataDrawGrid.AutoAdjustColumn(aCol: Integer);
var aMin,aMax,aPriority : integer;
begin
  if (aCol<0) or (aCol>ColCount-1) then
    Exit;
  CalcAutoSizeColumn(aCol,aMin,aMax,APriority);
  ColWidths[aCol]:=aMax;
end;

procedure TMVDataDrawGrid.SetDataSourceRow(aRecNo: integer);
begin
  if DataSourceOk then
  Begin
    DataSource.RecNo:=aRecNo;
  end;
end;

procedure TMVDataDrawGrid.RefreshFromDataSource;
var i,nb : integer;
begin
  SetLength(MinMax,0);
  if (csDesigning in ComponentState) or (csLoading in ComponentState) or (csCreating in ControlState) then
    Exit;
  Refreshing:=true;
  Try
    if DataSourceOk then
    Begin
       if Length(FDataFields)=0 then
       Begin
         SetLength(FDataFields,DataSource.Fields.Count);
         nb:=0;
         For i:=0 to pred(DataSource.Fields.Count) do
         Begin
           if DataSource.Fields[i].Visible then
           Begin
             FDataFields[nb]:=DataSource.Fields[i].FieldName;
             nb+=1;
           end;
         end;
         SetLength(FDataFields,nb);
       end;
      ColCount:=length(FDataFields);
      RowCount:=DataSource.RecordCount+1
    end
    else
    Begin
      if RowCount<>1 then
        RowCount:=1;
    end;
    FixedRows:=1;
  finally
    Refreshing:=false;
  end;
end;

procedure TMVDataDrawGrid.ShowField(const lst: array of string;const Descr: array of string);
var i : integer;
begin
  SetLength(FDataFields,length(lst));
  for i:=low(lst) to high(lst) do
    FDataFields[i]:=lst[i];
  SetLength(FDescr,length(Descr));
  for i:=low(Descr) to high(Descr) do
    FDescr[i]:=Descr[i];
end;

procedure TMVDataDrawGrid.CallBeginUpdate;
begin
  FUpdateCount+=1;
  BeginUpdate;
end;

procedure TMVDataDrawGrid.CallEndUpdate;
begin
   if FUpdateCount>0 then
   Begin
     FUpdateCount-=1;
     if FUpdateCount=0 then
       RefreshFromDataSource;
     EndUpdate(true);
   end;
end;

procedure TMVDataDrawGrid.GetSelections(out Selecteds: TGridRectArray);
var i : integer;
begin
  SetLength(Selecteds,Length(FSelections));
  For i:=low(FSelections) to high(FSelections) do
   Selecteds[i]:=FSelections[i];
end;

procedure TMVDataDrawGrid.ClearSelection;
begin
  ClearSelections;
end;

procedure TMVDataDrawGrid.VisualChange;
begin
  inherited VisualChange;
end;

procedure TMVDataDrawGrid.Paint;
begin
  LockDataSource;
  Try
    inherited Paint;
  finally
    UnLockDataSource;
  end;
end;

constructor TMVDataDrawGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RangeSelectMode:=rsmMulti;
  FixedRows:=1;
  FixedCols:=0;
  RowCount:=1;
  Options:=Options+[goColSizing,goDblClickAutoSize,goRowSelect];
end;

end.
