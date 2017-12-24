unit u_extdbgrid;

{*********************************************************************}
{                                                                     }
{             TExtDBGrid :                                            }
{             Grille avec couleurs de focus, d'édition,               }
{             permettant d'être éditée avec des composants de données }
{             Créateur : Matthieu Giroux                              }
{             13 Juin 2011                                            }
{                                                                     }
{*********************************************************************}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
   LCLType,
{$ELSE}
   Windows,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes, SysUtils, Grids,
  u_extcomponent, Graphics,
  Messages, DB,
{$IFDEF TNT}
  TntDBGrids,
{$ELSE}
{$IFDEF EXRX}
  ExRXDBGrid,
{$ELSE}
  {$IFDEF JEDI}
   JvDBUltimGrid, JvDBGrid,
  {$ELSE}
    {$IFDEF FPC}
     RxDBGrid,
    {$ELSE}
     RxDBCtrl,
    {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  DBGrids,
  Controls,
  ImgList,
  StdCtrls,
  U_ExtMapImageIndex;

{$IFDEF VERSIONS}
const
    gVer_ExtDBGrid : T_Version = ( Component : 'Grille de données étendue' ;
                                               FileUnit : 'U_ExtDBGrid' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Grille avec fonctions étendues' ;
                                               BugsStory : '1.0.5.1 : GetColumn and SetColumn OK.' + #13#10
                                                         + '1.0.5.0 : Paint Memos due to unknown problem.' + #13#10
                                                         + '1.0.4.0 : Paint Memos.' + #13#10
                                                         + '1.0.3.0 : Column resize property.' + #13#10
                                                         + '1.0.2.2 : Testing.' + #13#10
                                                         + '1.0.2.1 : Testing.' + #13#10
                                                         + '1.0.2.0 : MapImages property and testing.' + #13#10
                                                         + '1.0.1.1 : ImageList''s Event.' + #13#10
                                                         + '1.0.1.0 : ImageList with Field''s Index.' + #13#10
                                                         + '1.0.0.1 : UTF 8.' + #13#10
                                                         + '1.0.0.0 : Tested, making comments.' + #13#10
                                                         + '0.9.9.9 : Tested OK on DELPHI, need new version of LAZARUS to be completed.' + #13#10
                                                         + '0.9.0.0 : Création à partir de u_framework_dbcomponents.' ;
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 5 ; Build : 1 );
{$ENDIF}


   { TExtGridColumn }
type
   TExtOption = ( eoPaintEdits, eoPaintBlobs, eoPaintMemos );
   TExtOptions = set of TExtOption;
   TFieldIndexEvent = function(const Sender: TObject; const Field : TField ) : Integer of Object;

const CST_EXTGRID_DEFAULT_OPTIONS = [ eoPaintEdits{$IFNDEF FPC}, eoPaintMemos {$ENDIF}];
type
   TExtGridColumn = class({$IFDEF TNT}TTntColumn{$ELSE}{$IFDEF FPC}TRxColumn{$ELSE}TColumn{$ENDIF}{$ENDIF})
   private
     FOldControlKeyUp   , FOldControlKeyDown,
     FAfterControlKeyUp , FAfterControlKeyDown : TKeyEvent;
     FOldControlKeyPress   ,
     FAfterControlKeyPress : TKeyPressEvent;
     FControl : TWinControl ;
     FFieldTag : Integer ;
     FSortOrder : TSortMarker;
     FImages : TCustomImageList;
     FMapImages : TExtMapImages;
     FResize : Boolean;
   protected
     procedure SetControl ( const AValue : TWinControl ); virtual;
     procedure SetImages( const AValue : TCustomImageList ); virtual;
     procedure SetMapImages( const AValue : TExtMapImages ); virtual;
     function  fi_getFieldTag:Integer; virtual;
     procedure p_setFieldTag ( const avalue : Integer ); virtual;
   public
     constructor Create(ACollection: TCollection); override;
   published
     procedure ControlKeyUp   ( ASender : TObject ; var Key: Word; Shift: TShiftState ); virtual;
     procedure ControlKeyDown ( ASender : TObject ; var Key: Word; Shift: TShiftState ); virtual;
     procedure ControlKeyPress( ASender : TObject ; var Key: Char ); virtual;
     property SomeEdit : TWinControl read FControl       write SetControl;
     property Images : TCustomImageList read FImages       write SetImages;
     property MapImages : TExtMapImages read FMapImages       write SetMapImages;
     property FieldTag : Integer     read fi_getFieldTag write p_setFieldTag;
     property Resize : Boolean     read FResize write FResize default False;
     property SortOrder : TSortMarker     read FSortOrder write FSortOrder default smNone;
     property AfterControlKeyUp    : TKeyEvent      read FAfterControlKeyUp    write FAfterControlKeyUp;
     property AfterControlKeyDown  : TKeyEvent      read FAfterControlKeyDown  write FAfterControlKeyDown;
     property AfterControlKeyPress : TKeyPressEvent read FAfterControlKeyPress write FAfterControlKeyPress;
   end;

   { TExtDbGridColumns }

   TExtDbGridColumns = class({$IFDEF TNT}TTntDBGridColumns{$ELSE}{$IFDEF FPC}TRxDbGridColumns{$ELSE}TDBGridColumns{$ENDIF}{$ENDIF})
   private
     function GetColumn ( Index: Integer): TExtGridColumn;
     procedure SetColumn( Index: Integer; const Value: TExtGridColumn);
   public
     function Add: TExtGridColumn;
     {$IFDEF FPC}
    published
     {$ENDIF}
     property Items[Index: Integer]: TExtGridColumn read GetColumn write SetColumn; default;
   end;

   { TExtDBGrid }

   TExtDBGrid = class ( {$IFDEF TNT}TTntDBGrid{$ELSE}{$IFDEF EXRX}TExRxDBGrid{$ELSE}{$IFDEF JEDI}TJvDBUltimGrid{$ELSE}TRXDBGrid{$ENDIF}{$ENDIF}{$ENDIF}, IFWComponent )
     private
       FOnPopup : TNotifyEvent;
       FOldOnGetBtnParams: TGetBtnParamsEvent;
       FPaintOptions : TExtOptions;
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FColorEdit     ,
       FColorFocus    ,
       FOldFixedColor : TColor;
       FOnGetImageIndex : TFieldIndexEvent;
       FAlwaysSame : Boolean;
       FMouseFocus : Boolean;
       function GetColumns: TExtDbGridColumns;
       procedure SetColumns(const AValue: TExtDbGridColumns);
       procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
       procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
       procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
       {$IFNDEF FPC}
       procedure WMSize(var Msg: TWMSize); message WM_SIZE;
       {$ENDIF}
       procedure HideColumnControl;
       procedure p_SetPaintOptions ( const AValue : TExtOptions );
      protected
       function ShowControlColumn:Boolean; virtual;
       function IsColumnsStored: boolean; virtual;
       procedure DrawCell(aCol,aRow: {$IFDEF FPC}Integer{$ELSE}Longint{$ENDIF}; aRect: TRect; aState:TGridDrawState); override;
       function CanEditShow: Boolean; override;
       procedure ExtGetBtnParams (Sender: TObject; Field: TField;
        AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
        IsDown: Boolean); virtual;
       procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
       {$IFDEF FPC}
       function  MouseButtonAllowed(Button: TMouseButton): boolean; override;
      protected
       procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); override;
       {$ENDIF}
      public
       procedure KeyDown(var Key: Word; Shift: TShiftState); override;
       procedure KeyUp(var ach_Key: Word; ashi_Shift: TShiftState); override;
       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       function  CreateColumns: {$IFDEF FPC}TGridColumns{$ELSE}TDBGridColumns{$ENDIF}; override;
       {$IFDEF EXRX}
       procedure TitleClick(Column: TColumn); override;
       {$ELSE}
       {$IFDEF RX}
       procedure DoTitleClick(ACol: longint; ACollumn: TRxColumn; Shift: TShiftState); override;
       {$ELSE}
       {$IFDEF FPC}
       procedure DoTitleClick(ACol: longint; AColumn: TRxColumn; Shift: TShiftState); override;
       {$ELSE}
       procedure DoTitleClick(ACol: Longint; AField: TField); dynamic;
       {$ENDIF}
       {$ENDIF}
       {$ENDIF}
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      published
       property Columns: TExtDbGridColumns read GetColumns write SetColumns stored IsColumnsStored;
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property OnMouseEnter;
       property OnMouseLeave;
       property OnGetImageIndex : TFieldIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_GRID_STD ;
       property FixedColor default CST_GRID_STD ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_GRID_SELECT ;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OptionsExt : TExtOptions read FPaintOptions write p_SetPaintOptions default CST_EXTGRID_DEFAULT_OPTIONS;
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
     End;

implementation


uses
{$IFNDEF FPC}
   Forms,
{$ENDIF}
   fonctions_proprietes,
   Menus,
   fonctions_images;

{ TExtGridColumn }

procedure TExtGridColumn.SetMapImages(const AValue: TExtMapImages);
begin
  if FMapImages=AValue then Exit;
  FMapImages:=AValue;
end;

// Procedure SetControl
// Setting control of column
// Parameter : AValue the control of property
procedure TExtGridColumn.SetControl(const AValue: TWinControl);
var lmet_MethodeDistribuee: TMethod;

begin
  If AValue <> FControl Then
   Begin
     if ( FControl <> nil )
     and not ( csDesigning in Grid.ComponentState ) then
       Begin
         p_SetComponentMethodProperty( FControl, 'OnKeyUp'   , TMethod ( FOldControlKeyUp ));
         p_SetComponentMethodProperty( FControl, 'OnKeyDown' , TMethod ( FOldControlKeyDown ));
         p_SetComponentMethodProperty( FControl, 'OnKeyPress', TMethod ( FOldControlKeyPress ));
         FOldControlKeyPress := nil;
         FOldControlKeyDown  := nil;
         FOldControlKeyUp    := nil;
       End;
     FControl := AValue;
     if ( FControl = nil ) Then
       Exit;
     FControl.Parent := Grid.Parent;
     FControl.Visible := False;
     p_SetComponentObjectProperty ( FControl, 'Datasource', (TDBGrid (Grid)).DataSource );
     if not ( csDesigning in Grid.ComponentState ) then
      Begin
       FOldControlKeyUp     := TKeyEvent      ( fmet_getComponentMethodProperty ( FControl, 'OnKeyUp'  ));
       FOldControlKeyDown   := TKeyEvent      ( fmet_getComponentMethodProperty ( FControl, 'OnKeyDown'));
       FOldControlKeyPress  := TKeyPressEvent ( fmet_getComponentMethodProperty ( FControl, 'OnKeyPress'));
       lmet_MethodeDistribuee.Data := Self;
       lmet_MethodeDistribuee.Code := MethodAddress('ControlEnter');
       p_SetComponentMethodProperty( FControl, 'OnEnter', lmet_MethodeDistribuee);
       lmet_MethodeDistribuee.Code := MethodAddress('ControlExit');
       p_SetComponentMethodProperty( FControl, 'OnExit' , lmet_MethodeDistribuee );
       lmet_MethodeDistribuee.Code := MethodAddress('ControlKeyUp');
       p_SetComponentMethodProperty( FControl, 'OnKeyUp' , lmet_MethodeDistribuee );
       lmet_MethodeDistribuee.Code := MethodAddress('ControlKeyDown');
       p_SetComponentMethodProperty( FControl, 'OnKeyDown' , lmet_MethodeDistribuee );
       lmet_MethodeDistribuee.Code := MethodAddress('ControlKeyPress');
       p_SetComponentMethodProperty( FControl, 'OnKeyPress' , lmet_MethodeDistribuee );
      end;
   end;
end;

procedure TExtGridColumn.SetImages(const AValue: TCustomImageList);
begin
  FImages:=AValue;
end;

// function fi_getFieldTag
// Getting the FieldTag Property
// Returns Tag Property
function TExtGridColumn.fi_getFieldTag: Integer;
begin
  Result := FFieldTag;
end;

// procedure p_setFieldTag
// Setting the FieldTag Property
// Parameter : The FieldTag to set
procedure TExtGridColumn.p_setFieldTag( const avalue : Integer );
begin
  FFieldTag := avalue;
end;

// procedure TExtGridColumn.ControlKeyDown
// Sending the control key event to the dbgrid
procedure TExtGridColumn.ControlKeyDown(ASender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if assigned ( FOldControlKeyDown ) Then
     FOldControlKeyDown ( ASender, Key, Shift );
  ( Grid as TExtDBGrid ).KeyDown( Key, Shift );
  if assigned ( FAfterControlKeyDown ) Then
     FAfterControlKeyDown ( ASender, Key, Shift );

end;

// procedure TExtGridColumn.ControlKeyPress
// Sending the control key event to the dbgrid
procedure TExtGridColumn.ControlKeyPress(ASender: TObject; var Key: Char);
begin
  if assigned ( FOldControlKeyPress ) Then
     FOldControlKeyPress ( ASender, Key );
  ( Grid as TExtDBGrid ).KeyPress( Key );
  if assigned ( FAfterControlKeyPress ) Then
     FAfterControlKeyPress ( ASender, Key );

end;

// procedure TExtGridColumn.ControlKeyUp
// Sending the control key event to the dbgrid
procedure TExtGridColumn.ControlKeyUp(ASender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if assigned ( FOldControlKeyUp ) Then
     FOldControlKeyUp ( ASender, Key, Shift );
  ( Grid as TExtDBGrid ).KeyUp( Key, Shift );
  if assigned ( FAfterControlKeyUp ) Then
     FAfterControlKeyUp ( ASender, Key, Shift );
end;

// constructor TExtGridColumn.Create
// Initing the column
constructor TExtGridColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAfterControlKeyDown  := nil;
  FAfterControlKeyUp    := nil;
  FAfterControlKeyPress := nil;
  FOldControlKeyDown    := nil;
  FOldControlKeyPress   := nil;
  FOldControlKeyUp      := nil;
  FSortOrder:=smNone;
  FControl := nil;
  FFieldTag := 0 ;
  FResize:=False;
end;


{ TExtDbGridColumns }

// function TExtDbGridColumns.GetColumn
// Getting the Column property from index
function TExtDbGridColumns.GetColumn( Index: Integer): TExtGridColumn;
begin
  if  ( Index >= 0 )
  and ( Index <  Count )
  Then Result := TExtGridColumn( inherited Items[Index] )
  Else Result := nil;
end;

// procedure TExtDbGridColumns.SetColum
// Setting the Column property with index
procedure TExtDbGridColumns.SetColumn( Index: Integer; const Value: TExtGridColumn);
begin
  if  assigned ( Value )
  and ( Index >= 0 )
  and ( Index <  Count )
   Then
    Items[Index].Assign( Value );
end;

// function TExtDbGridColumns.Add
// Adding a Column
function TExtDbGridColumns.Add: TExtGridColumn;
begin
  result := TExtGridColumn (inherited Add);
end;

{ TExtDBGrid }

procedure TExtDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var i : Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and ( AComponent is TExtMapImages    )
   then
    Begin
      for i:=0 to Columns.Count -1 do with Columns [i] do
        if (AComponent = FMapImages) Then FMapImages := nil;
    end
   else
    if (Operation = opRemove) and ( AComponent is TCustomImageList )
     then for i:=0 to Columns.Count -1 do with Columns [i] do
       if (AComponent = FImages   ) Then FImages := nil;
end;


// Procedure  p_SetPaintEdits
// Setting PaintEdits property to paint edits in grid
procedure TExtDBGrid.p_SetPaintOptions(const AValue: TExtOptions);
begin
  if AValue <> FPaintOptions Then
    Begin
     FPaintOptions:= AValue;
     if not ( csCreating in ControlState ) Then
       Invalidate;
    end;
end;


// procedure TExtDBGrid.HideColumnControl
// Hiding the Column's Control
// Elsewhere the component is painted by the dbgrid to clone it
procedure TExtDBGrid.HideColumnControl;
var i: Integer ;

Begin
  for i := 0 to Columns.Count - 1 do
    if  assigned ( Columns [ i ].SomeEdit )
    and Columns [ i ].SomeEdit.Visible then
      with Columns [ i ].SomeEdit do
       Begin
         Update;
         Visible := False;
       End

End;

// procedure TExtDBGrid.WMHScroll
// Hiding column's control on scroll
procedure TExtDBGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  HideColumnControl;
end;

// procedure TExtDBGrid.WMVScroll
// Hiding column's control on scroll
procedure TExtDBGrid.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  HideColumnControl;
end;

// procedure TExtDBGrid.WMSetFocus
// Hiding column's control on Focus
procedure TExtDBGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  Inherited;
  HideColumnControl;
end;

// Procedure ShowControlColumn
// Shows the control of column if exists
function TExtDBGrid.ShowControlColumn:Boolean;
var Weight, Coord, LCol : Integer ;
{$IFNDEF FPC}
    DrawInfo: TGridDrawInfo;
 function LinePos(const AxisInfo: TGridAxisDrawInfo; Line: Integer): Integer;
  var
    Start, I: Longint;
  begin
    with AxisInfo do
    begin
      Result := 0;
      if Line < FixedCellCount then
        Start := 0
      else
      begin
        if Line >= FirstGridCell then
          Result := FixedBoundary;
        Start := FirstGridCell;
      end;
      for I := Start to Line - 1 do
      begin
        Inc(Result, GetExtent(I) + EffectiveLineWidth);
        if Result > GridExtent then
        begin
          Result := 0;
          Exit;
        end;
      end;
    end;
  end;

  procedure CalcAxis(const AxisInfo: TGridAxisDrawInfo;
    GridRectMin: Integer;
    var ScreenRectMin: Integer);
  begin
    with AxisInfo do
    begin
      if (GridRectMin >= FixedCellCount) and (GridRectMin < FirstGridCell) then
          GridRectMin := FirstGridCell;

      ScreenRectMin := LinePos(AxisInfo, GridRectMin);
    end;
  end;
{$ELSE}
  WidthHeight : Integer ;
{$ENDIF}
begin
  Result:=False;
  {$IFNDEF FPC}
  LCol := RawToDataColumn(Col);
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  {$ELSE}
  LCol := Col;
  {$ENDIF}
    if  ( Row >= FixedRows )
    and ( LCol >= FixedCols )
    and ( LCol  < Columns.Count + FixedCols  )
    and ( Columns [ SelectedIndex ].SomeEdit <> nil )  Then
      with Columns [ SelectedIndex ].SomeEdit do
        Begin
          Coord  := 0 ;
          Weight := 0 ;
          {$IFNDEF FPC}
          if Self.Ctl3D then
            inc ( Weight, 1 );
          if Self.BorderStyle <> bsNone then
            inc ( Weight, 1 );
          {$ENDIF}
          {$IFDEF FPC}
          WidthHeight := 0 ;
          ColRowToOffset ( True, True, Col, Coord, WidthHeight);
          {$ELSE}
          CalcAxis(Horz,Selection.Left, Coord);
          {$ENDIF}
          Left := Coord + Self.ClientRect.Left + Self.Left + Weight ;
          Width := ColWidths [ Col ];
          {$IFDEF FPC}
          ColRowToOffset ( False, True, Row, Coord, WidthHeight);
          {$ELSE}
          CalcAxis(Vert,Selection.Bottom, Coord);
          {$ENDIF}
          Top  := Coord  + Self.ClientRect.Top  + Self.Top  + Weight;
          Height := RowHeights[Row];
          Visible := True;
          SetFocus;
          Result:=True;
        End;
//  with DrawInfo do
//     ShowMessage(IntToStr(InvalidRect.Left)+' '+IntToStr(InvalidRect.Top)+' '+IntToStr(Vert.FirstGridCell)+' '+IntToStr(Horz.FirstGridCell) + ' ' + Inttostr ( SelectedIndex )  + ' ' +Inttostr(Selection.Left) + ' ' +Inttostr(Selection.Top));

end;


function TExtDBGrid.CanEditShow: Boolean;
begin
  if  ( SelectedIndex >= 0 )
  and ( SelectedIndex < Columns.Count )
  and assigned ( Columns [ SelectedIndex ].SomeEdit )
  and ( Columns [ SelectedIndex ].SomeEdit.Visible ) then
    Begin
     Result := False;
    End
  Else
   Result := inherited CanEditShow;

end;

procedure TExtDBGrid.ExtGetBtnParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
  IsDown: Boolean);
var li_i : Integer;
begin
  for li_i := 0 to Columns.Count - 1  do
    if Columns [ li_i ].FieldName = Field.FieldName Then
      Begin
        SortMarker:=Columns [ li_i ].SortOrder;
        Break;
      end;
  if Assigned(FOldOnGetBtnParams) Then
    FOldOnGetBtnParams ( Self, Field, AFont,Background,SortMarker,IsDown);
end;

// constructor TExtDBGrid.Create
// Initing the dbgrid
constructor TExtDBGrid.Create( AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldOnGetBtnParams := nil;
  FPaintOptions:=[eoPaintEdits];
  FAlwaysSame := True;
  FColorFocus := CST_GRID_SELECT;
  FColorEdit  := CST_GRID_STD;
  FixedColor  := CST_GRID_STD;
  FWBeforeEnter:=nil;
  FWBeforeExit :=nil;
{$IFDEF FPC}
  ScrollBars:=ssAutoBoth;
{$ENDIF}
end;

// procedure TExtDBGrid.Loaded
// Finishing the Init after the dbgrid is fully loaded
procedure TExtDBGrid.Loaded;
begin
  inherited Loaded;
  FOldFixedColor := FixedColor;
  if  FAlwaysSame
   Then
    FixedColor := gCol_Grid ;
  if not ( csDesigning in ComponentState ) Then
   Begin
    FOldOnGetBtnParams := OnGetBtnParams;
    OnGetBtnParams:=ExtGetBtnParams;
   end;
End;

// procedure TExtDBGrid.MouseDown
// On MouseDown show eventually column control
procedure TExtDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var APopupMenu : TPopupMenu;
    Continue : Boolean;
begin
  inherited;
  ShowControlColumn;
  if  (Button = mbRight)
  and ( Shift = [] ) Then
    fb_ShowPopup(self,PopupMenu,OnContextPopup,FOnPopup);
end;
{$IFDEF FPC}
function TExtDBGrid.MouseButtonAllowed(Button: TMouseButton): boolean;
begin
  Result:=inherited MouseButtonAllowed(Button) or ( Assigned ( PopupMenu ) and ( Button = mbRight )) ;
end;
{$ENDIF}

// procedure TExtDBGrid.ChangeBounds
// Resize columns with resize property to true
{$IFDEF FPC}
procedure TExtDBGrid.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
{$ELSE}
procedure TExtDBGrid.WMSize(var Msg: TWMSize);
var AWidth,AHeight:Integer;
{$ENDIF}
var AResizedColumns, AWidthResize, i, AWidthMarge : Integer;
    APassed : Boolean;
begin
{$IFNDEF FPC}
  with msg do
    Begin
      AWidth:=Width;
      AHeight:=Height;
    End;
{$ENDIF}
  if  {$IFDEF FPC}( AutoSizingLockCount = 0 )
  and {$ENDIF}( aWidth <> Width ) then
   Begin
     AResizedColumns  := 0;
     for i := 0 to Columns.Count - 1 do
      with Columns [ i ] do
       if Resize Then
        Begin
          inc ( AResizedColumns );
        end;
     AWidthMarge := aWidth - Width;
     inherited;
     if AResizedColumns = 0 Then Exit;
     AWidthResize:= AWidthMarge div AResizedColumns;
     APassed := False;
     for i := 0 to Columns.Count - 1 do
      with Columns [ i ] do
       if Resize Then
        Begin
          if APassed
           Then Width := Width+AWidthResize
           Else
            Begin
             Width := Width+AWidthResize+AWidthMarge - ( AWidthResize * AResizedColumns );
             APassed := True;
            end;
        end;
   end
  Else
   inherited;
end;

// function TExtDBGrid.GetColumns
// This Dbgrid uses TExtDbGridColumns
function TExtDBGrid.GetColumns: TExtDbGridColumns;
begin
  Result := inherited Columns as TExtDBGridColumns;
end;

// procedure TExtDBGrid.SetColumns
// This Dbgrid uses TExtDbGridColumns
procedure TExtDBGrid.SetColumns(const AValue: TExtDbGridColumns);
begin
  inherited Columns := AValue;
end;


// procedure TExtDBGrid.DrawCel
// Cloning the Column Control on cell drawing
procedure TExtDBGrid.DrawCell(aCol, aRow: {$IFDEF FPC}Integer{$ELSE}Longint{$ENDIF}; aRect: TRect;
  aState: TGridDrawState);
var Aindex, LCol : Integer;
{$IFDEF FPC}
    FBackground : TColor;
{$ENDIF}
    FPainted : Boolean;
    FBitmap : TBitmap;

   procedure PrepareCell;
   Begin
    {$IFDEF FPC}
    PrepareCanvas(LCol, aRow, aState);
    if Assigned(OnGetCellProps) and not (gdSelected in aState) then
    begin
      FBackground:=Canvas.Brush.Color;
      OnGetCellProps(Self, GetFieldFromGridColumn(LCol), Canvas.Font, FBackground);
      Canvas.Brush.Color:=FBackground;
    end;
    {$ELSE}
    Canvas.Brush.Color := Color;
    {$ENDIF}
    Canvas.FillRect(aRect);
   end;
           (*
   procedure p_FieldMemo;
   var AImageIndex:Integer;
       RxColumn : TExtGridColumn;
   Begin
    if ( eoPaintMemos in FPaintOptions) Then
     Begin
      RxColumn:= TExtGridColumn ( Columns [ LCol - FixedCols ]);
      with RxColumn do
      if ( Field is TMemoField )  Then
       Begin
         PrepareCell;
         {$IFDEF FPC}DrawCellGrid{$ELSE}DoDrawCell{$ENDIF}(ACol,aRow, aRect, aState);
    {$IFDEF FPC}
         if Assigned(ImageList) then
         begin
           AImageIndex := StrToIntDef(KeyList.Values[Field.AsString],
             NotInKeyListIndex);
           if (AImageIndex > -1) and (AImageIndex < ImageList.Count) then
             DrawCellBitmap(RxColumn, aRect, aState, AImageIndex);
         end;
    {$ENDIF}
       with Canvas, Field as TMemoField do
        Begin
         Brush.Color:=clWhite;
         Pen  .Color:=clWhite;
         Rectangle(aRect.Left+2,aRect.Top+2,aRect.Right-2,aRect.Bottom-2);
         if BlobSize > 0 then
           Begin
             Font.Assign(Self.Font);
             Pen  .Color:=font.Color;
             TextRect(aRect,aRect.Left+2,arect.top+2, AsString );
           end;
         FPainted := True;
        end;
       End;
     end;

   end;
        *)
   procedure p_FieldBlob;
   Begin
    if ( eoPaintBlobs in FPaintOptions) Then
    with ( TExtGridColumn ( Columns [ LCol - FixedCols ])) do
    if ( Field is TBlobField ) and ( ( Field as TBlobField ).BlobType = ftBlob )  Then
     Begin
       FBitmap := TBitmap.Create;
       try
         p_FieldToImage ( Field, FBitmap, aRect.Right-aRect.Left,arect.Bottom-aRect.Top,True,False);
         PrepareCell;
         Canvas.Draw(aRect.Left,aRect.Top, FBitmap );
         FPainted := True;
       Finally
         with FBitmap do
          Begin
           {$IFNDEF FPC}
           Dormant;
           {$ENDIF}
           FreeImage;
           Free;
          End;
       End
     End;

   end;

   procedure p_paintEdits;
   Begin
    if eoPaintEdits in FPaintOptions Then
    with ( TExtGridColumn ( Columns [ LCol - FixedCols ])) do
     Begin
      if assigned ( SomeEdit ) Then
      with SomeEdit do
        Begin
          PrepareCell;
          Aindex := Datalink.ActiveRecord;
          Datalink.ActiveRecord := ARow {$IFDEF FPC}-FixedRows{$ELSE}-IndicatorOffset{$ENDIF};
          Width  := aRect.Right - aRect.Left;
          Height := ARect.Bottom - aRect.Top;
          ControlState := ControlState + [csPaintCopy];
          PaintTo(Self.Canvas.Handle,aRect.Left,aRect.Top);
          ControlState := ControlState - [csPaintCopy];
          Datalink.ActiveRecord := Aindex;
          FPainted := True;
        end
      else
      if assigned ( FImages )
      and (( Field is TNumericField ) or Assigned(FOnGetImageIndex) or Assigned(FMapImages))
      and not ( Datalink.DataSet.State in [dsEdit, dsInsert]) then
       Begin
         if Assigned(FOnGetImageIndex)
          Then Aindex := FOnGetImageIndex ( Self, Field )
          Else if assigned ( FMapImages )
          Then AIndex := FMapImages.ImageIndexOf(Field.AsString)
          Else Aindex := Field.AsInteger;
          if  ( Aindex < FImages.Count)
          and ( Aindex >= 0 ) then
           begin
             PrepareCell;
             {$IFDEF FPC}DrawCellGrid{$ELSE}DoDrawCell{$ENDIF}(ACol,aRow, aRect, aState);
             FBitmap := TBitmap.Create;
             FImages.GetBitmap(Aindex, FBitmap);
             with FBitmap do
              try
                //Modified:=True;
                p_ChangeTailleBitmap(FBitmap,aRect.Right-aRect.Left,arect.Bottom-aRect.Top,True);
                TransparentMode:=tmAuto;
                Transparent := True;
                ControlState := ControlState + [csPaintCopy];
               // Self.Canvas.FillRect(aRect.Left,arect.Top,arect.Right,arect.Bottom);
                Self.Canvas.Draw(aRect.Left+( aRect.Right - aRect.Left - FBitmap.Width ) div 2,aRect.Top+( aRect.Bottom - aRect.Top - FBitmap.Height ) div 2, FBitmap );
                ControlState := ControlState - [csPaintCopy];
              Finally
                {$IFNDEF FPC}
                Dormant;
                {$ENDIF}
                FreeImage;
                Free;
              end;
           end;
          FPainted := True;
        end;
     end;

   end;

begin
  {$IFNDEF FPC}
  LCol := RawToDataColumn ( ACol );
  {$ELSE}
  LCol := ACol;
  {$ENDIF}
  if  ( FPaintOptions <> [] )
  and ( LCol >= FixedCols  )
  and ( LCol < Columns.Count + FixedCols )
  and ( ARow >= {$IFDEF FPC}FixedRows{$ELSE}IndicatorOffset{$ENDIF} ) then
   Begin
     FPainted := False;
     p_paintEdits;
//     p_FieldMemo; // not the problem
     p_FieldBlob;
     if not FPainted Then
      inherited DrawCell(aCol, aRow, aRect, aState);
   end
  Else
    inherited DrawCell(aCol, aRow, aRect, aState);
end;

// function TExtDBGrid.IsColumnsStored
//  Columns are stored
function TExtDBGrid.IsColumnsStored: boolean;
begin
  result := True;
end;


// procedure TExtDBGrid.KeyDown
// Show Column control on KeyDown
procedure TExtDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  ShowControlColumn;
end;

// procedure TExtDBGrid.KeyUp
// DELETING on DELETE key
procedure TExtDBGrid.KeyUp(var ach_Key: Word; ashi_Shift: TShiftState);
begin
  if  ( ach_Key = VK_DELETE )
  and ( ashi_Shift = [ssCtrl] )
  and assigned ( DataSource )
  and assigned ( Datasource.DataSet ) Then
    Begin
      Datasource.DataSet.Delete;
      ach_Key := 0 ;
    End;
  inherited KeyUp(ach_Key, ashi_Shift);
end;

// function TExtDBGrid.CreateColumns
// This Dbgrid uses TExtDbGridColumns
function TExtDBGrid.CreateColumns: {$IFDEF FPC}TGridColumns{$ELSE}TDBGridColumns{$ENDIF};
begin
  Result := TExtDbGridColumns.Create(Self, TExtGridColumn);
end;

// procedure TExtDBGrid.DoEnter
// Setting the Focus color
procedure TExtDBGrid.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Rétablit le focus si on l'a perdu
  if FAlwaysSame
   Then
    FixedColor := gCol_GridSelect
   else
    FixedColor := FColorFocus ;
  inherited DoEnter;
end;

// procedure TExtDBGrid.DoEnter
// Setting the editing or readonly color
procedure TExtDBGrid.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  // Rétablit la couleur du focus
  if FAlwaysSame
   Then
    FixedColor := gCol_Grid
   else
    FixedColor := FOldFixedColor ;
end;

// procedure TExtDBGrid.(Do)TitleClick
// Gestion du click sur le titre, sorting
// Overriding The ancestor Title Click procedure
{$IFDEF EXRX}
procedure TExtDBGrid.TitleClick(Column: TColumn);
{$ELSE}
{$IFDEF RX}
procedure TExtDBGrid.DoTitleClick(ACol: longint; ACollumn: TRxColumn; Shift: TShiftState);
{$ELSE}
{$IFDEF FPC}
procedure TExtDBGrid.DoTitleClick(ACol: longint; AColumn: TRxColumn; Shift: TShiftState);
{$ELSE}
procedure TExtDBGrid.DoTitleClick(ACol: Longint; AField: TField);
{$ENDIF}
{$ENDIF}
{$ENDIF}

var li_Tag , li_i : Integer ;
begin
  // Phase d'initialisation
 li_Tag :=(TExtGridColumn ( {$IFDEF EXRX} Column {$ELSE} Columns [ ACol ]{$ENDIF})).FieldTag ;
 if li_tag > 0 then
   for li_i :=0 to ComponentCount -1 do
      if  ( li_Tag = Components [ li_i ].Tag )
      and Supports( Components [ li_i ], IFWComponentEdit )
      and ( Components [ li_i ] as TWinControl ).Visible  Then
        Begin
          ( Components [ li_i ] as IFWComponentEdit).SetOrder;
        End ;
  SetFocus;
end;

{$IFDEF VERSIONS}
initialization
  // Gestion de version
  p_ConcatVersion(gVer_ExtDBGrid);
{$ENDIF}
end.

