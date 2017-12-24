unit fonctions_search_edit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses Controls, Classes,
     {$IFDEF FPC}
     LCLType,WSMenus,
     {$ELSE}
     Messages, Windows,
     {$ENDIF}
     DBCtrls,
     Graphics, DB,
     {$IFDEF VERSIONS}
     fonctions_version,
     {$ENDIF}
     Forms,
     fonctions_string,
     Menus,
     DBGrids, StdCtrls,
     ExtCtrls;

const
{$IFDEF VERSIONS}
    gVer_Tfonctions_SearchEdit : T_Version = ( Component : 'Composant TExtSearchDBEdit' ;
                                          FileUnit : 'U_TExtSearchDBEdit' ;
                                          Owner : 'Matthieu Giroux' ;
                                          Comment : 'Searching in a dbedit.' ;
                                          BugsStory : '1.3.1.1 : Empty bug.'
                                                    + '1.3.1.0 : DBGrid on a form.'
                                                    + '1.3.0.1 : Searching not working correctly with not list or on Windows 8.'
                                                    + '1.3.0.0 : Integrating rxpopupform.'
                                                    + '1.2.0.1 : Testing on Delphi.'
                                                    + '1.2.0.0 : Multiple searchs and TListPopupEdit import.'
                                                    + '1.1.0.0 : Adding fb_KeyUp.'
                                                    + '1.0.0.0 : Creating fb_SearchText.';
                                          UnitType : 1 ;
                                          Major : 1 ; Minor : 3 ; Release : 1 ; Build : 1);

{$ENDIF}
  SEARCHEDIT_GRID_DEFAULTS = [dgColumnResize, dgRowSelect, dgColLines, dgConfirmDelete, dgCancelOnExit, dgTabs, dgAlwaysShowSelection];
  SEARCHEDIT_GRID_DEFAULT_SCROLL = {$IFDEF FPC}ssAutoBoth{$ELSE}ssBoth{$ENDIF};
  SEARCHEDIT_GRID_DEFAULT_OPTIONS = [dgColLines, dgRowLines];
  SEARCHEDIT_DEFAULT_FIELD_SEPARATOR = ';';
  SEARCHEDIT_DEFAULT_COUNT = 7;

type ISearchEdit = interface
      ['{34886DAB-F444-41A9-9F76-347109C99273}']
      procedure Locating;
      procedure NotFound;
      procedure p_SearchText;
      procedure ValidateSearch;
      function CanModify:Boolean;
      procedure  SelectKeyValue(ListValue:String);
      procedure ClosePopupEvent;
      procedure FreePopup;
      procedure SetEvent ;
      function GetFieldSearch: String;
     End;

  { TExtPopupDBGrid }

  TExtPopupDBGrid = class ( TDBGrid )
   protected
    WControl:TWinControl;
    procedure p_Close; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   public
    procedure CellClick({$IFDEF FPC}const aCol,aRow: Integer; const Button:TMouseButton{$ELSE}AColumn : TColumn{$ENDIF}); override;

  end;

  { TExtPopupGrid }

  TExtPopupGrid = class(TCustomForm)
   private
    WControl:TWinControl;
    FDBGrid : TExtPopupDBGrid;
    FLookupDisplayIndex: integer;
    FLookupDisplayField:string;
    procedure SetLookupDisplayIndex(const AValue: integer);
  protected
    property  Control:TWinControl read WControl;
  public
    constructor CreatePopUp(const AForm: Tcustomform; const AControl : TWinControl;
      const AOptions : TDBGridOptions;const ARowCount,AWidth:word); virtual;
    procedure DoSetFieldsFromString(FL,FWidths:string;const AFieldSeparator:Char); virtual;
    property DBGrid : TExtPopupDBGrid read FDBGrid;
    property LookupDisplayIndex:integer read FLookupDisplayIndex write SetLookupDisplayIndex;
  end;


  { TExtDBPopup }

  { TExtPopUpForm }

  TExtPopUpForm = class (TForm)
  private
    FGrid:TExtPopupGrid;
  end;

function fb_KeyUp ( const AEdit : TCustomEdit ;var Key : Word ; var Alocated, ASet : Boolean; const APopup : TCustomForm ):Boolean;
function fb_SearchLocating(var FPopup : TExtPopupGrid; var FSearchVisible : Boolean;
                           const AControl : TCustomEdit ; const FSearchSource : TFieldDataLink;
                           const FTextSeparator : String ; const AOptions : TDBGridOptions;
                           const ALookupDisplayIndex : Integer; const ARowCount,AWidth :Word;
                           const FSearchList,FWidths : String ;const FFieldSeparator:Char ):Boolean;
function fb_SearchText(const AEdit : TCustomEdit ; const FSearchSource : TFieldDataLink;
                       const FSearchFiltered : Boolean; const FTextSeparator : String ):Boolean;
procedure p_ShowPopup(var FPopup : TExtPopupGrid;const AControl : TWinControl;
                      const FSearchSource : TFieldDataLink;const FSearchList,FWidths : String;
                      const ALookupDisplayIndex : Integer; const ARowCount,AWidth :Word;
                      const AOptions : TDBGridOptions ;const FFieldSeparator:Char);
implementation

uses fonctions_db,
     rxdbutils,
     {$IFDEF FPC}
     LazUTF8,
     {$ELSE}
     {$ENDIF}
     fonctions_dbobjects,
     fonctions_proprietes,
     Variants,
     sysutils;

// show popup
procedure p_ShowPopup(var FPopup : TExtPopupGrid;const AControl : TWinControl;
                      const FSearchSource : TFieldDataLink;const FSearchList,FWidths : String;
                      const ALookupDisplayIndex : Integer; const ARowCount,AWidth :Word;
                      const AOptions : TDBGridOptions ;const FFieldSeparator:Char);
var i : Integer;
    ABookmark:TBookmark;
  PopupOrigin,
  APoint  :TPoint;
Begin
  with FSearchSource.DataSet,AControl do
  if  ( FSearchList > '' )
  and ( RecordCount > 1 ) // RecordCount must be at end of if tests, long test
   Then
    Begin
     ABookmark:=FSearchSource.DataSet.GetBookmark;
     try
       if not Assigned( FPopup ) Then
        Begin
          FPopup:=TExtPopupGrid.CreatePopUp(Owner as TCustomForm, AControl,AOptions,ARowCount,AWidth);
          FPopup.DBGrid.Datasource:= FSearchSource.Datasource;

          FPopup.LookupDisplayIndex:=ALookupDisplayIndex;

          FPopup.DBGrid.ParentColor:=True;
          FPopup.DBGrid.ParentFont:=True;

          FPopup.DoSetFieldsFromString(FSearchList,FWidths,FFieldSeparator);
          FPopup.Loaded;

          with FPopup,PopupOrigin do
           Begin
             if Owner=AControl.Parent
              Then
               Begin
                x:=AControl.Left;
                y:=AControl.Height + AControl.Top;
               end
              Else
               PopupOrigin:=AControl.Parent.{$IFDEF FPC}ControlToScreen{$ELSE}ClientToScreen{$ENDIF}(Point(AControl.Left, AControl.Height + AControl.Top));
             APoint.X:=X;
             APoint.Y:=y+Height;
             if APoint.Y>Screen.Height Then
              Begin
                y:=Screen.Height-Height;
                Anchors:=[akLeft, akBottom];
              end;
             if APoint.x>Screen.Width Then
              Begin
                x:=Screen.Width-Width;
                if akBottom in Anchors
                 Then Anchors:=[akRight, akBottom]
                 Else Anchors:=[akRight, akTop];
              end;
             Left:=X;
             Top:=Y;
           end;
        end;
       FPopup.Show;
       
     finally
       FSearchSource.DataSet.GotoBookmark(ABookmark);
       FreeBookmark(ABookmark);
     end;
    End;
End;
{ TExtPopupDBGrid }


procedure TExtPopupDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:Visible:=False;
    VK_RETURN:begin
                p_close;
                exit;{In that case we need to exit away.}
              end;
  else
    if  (Key = VK_UP)
    and (Row = FixedRows)
     Then
      Begin
       WControl.SetFocus;
       
      end
     Else inherited KeyDown(Key, Shift);
  end;
end;

// click event of datasearch popup
procedure TExtPopupDBGrid.CellClick({$IFDEF FPC}const aCol,aRow: Integer; const Button:TMouseButton{$ELSE}AColumn : TColumn{$ENDIF});
begin
  p_close;
end;

// click event of datasearch popup
procedure TExtPopupDBGrid.p_Close;
begin
 (WControl as ISearchEdit).ClosePopupEvent;
 Parent.Visible:=False;
end;

{ TExtPopupGrid }

procedure TExtPopupGrid.DoSetFieldsFromString(FL,FWidths: string;const AFieldSeparator:Char);
var
  FieldName:string;
  GK:TColumn;
  K:integer;
  ANumber : String;
  gb_autowidths : Boolean;
  gi_count : Integer;
begin
  gb_autowidths:=Fwidths='';
  gi_count:=fi_CharCounter ( FL, AFieldSeparator )+1;
  while (FL<>'') do
  begin
    K:=Pos(AFieldSeparator, FL);
    if K>0 then
    begin
      FieldName:=Copy(FL, 1, K-1);
      Delete(FL, 1, K);
      K:=Pos(AFieldSeparator, FWidths);
      if K>0 then
      begin
        ANumber:=Copy(FWidths, 1, K-1);
        Delete(FWidths, 1, K);
      end;
    end
    else
    begin
      FieldName:=FL;
      Anumber:=FWidths;
      FL:='';
      FWidths:='';
    end;
    with FDBGrid do
     Begin
      GK:=Columns.Add;
      GK.FieldName:=FieldName;
      if gb_autowidths
       Then GK.Width:=ClientWidth div gi_count
       Else
        if ANumber > '' Then
          try
            GK.Width:=StrToInt(ANumber);
          Except
          end;
      GK.Visible:=True;
     end;
  end;
end;

constructor TExtPopupGrid.CreatePopUp(const AForm: Tcustomform; const AControl : TWinControl;
      const AOptions : TDBGridOptions;const ARowCount,AWidth:word);
begin
  inherited Create(AForm);
  WControl:=AControl;
  Caption:='';
  Anchors:=[akLeft, akTop];
  WindowState:=wsNormal;
  FormStyle:=fsStayOnTop;
  PopupMode:=pmAuto;
  BorderStyle:=bsNone;
  PopupParent:=AForm;
  if AWidth > 0 Then
    Width:=AWidth;
  FDBGrid := TExtPopupDBGrid.Create(Self);
  ActiveControl:=FDBGrid;
  with FDBGrid do
   Begin
    WControl:=AControl;
    ReadOnly:=true;
    Parent:=Self;
    Caption:='ExtPopUp';
    Options:=AOptions;
    Options:=Options - [dgEditing];
    Height:=DefaultRowHeight * ARowCount;
    Visible:=True;
   end;
  AutoSize:=True;
end;

procedure TExtPopupGrid.SetLookupDisplayIndex(const AValue: integer);
begin
  FLookupDisplayIndex:=AValue;
  if FLookupDisplayIndex > -1
   Then FLookupDisplayField:=FDBGrid.Columns[FLookupDisplayIndex].FieldName
   Else FLookupDisplayField:='';
end;

{ functions }


// return partially or fully located
function fb_SearchText(const AEdit : TCustomEdit ; const FSearchSource : TFieldDataLink; const FSearchFiltered : Boolean; const FTextSeparator : String ):Boolean;
var LText : String;
    li_pos : Integer;
begin
  with AEdit,FSearchSource,Dataset do
    Begin
      Ltext := Text;
      Open ;
      if IsEmpty Then Exit;
      li_pos := fs_LastString ( FTextSeparator, LText );
      if li_pos>0
        Then
         Begin
          inc ( li_pos, length ( FTextSeparator ));
          LText := copy (LText,li_pos,length ( LText ) - li_pos +1 );
         End;
       // Trouvé ?
      if not assigned ( FindField ( FieldName )) Then Exit;
      if FSearchFiltered Then
       Begin
        Filter := 'LOWER('+ FieldName+') LIKE ''' + LowerCase(fs_stringDbQuote(LText)) +'%''';
        Filtered:=True;
       End;
      if not IsEmpty Then
       Result := fb_Locate ( DataSet, FieldName, LText, [loCaseInsensitive, loPartialKey], True ); // not found : no popup
    end
end;

function fb_SearchLocating(var FPopup : TExtPopupGrid; var FSearchVisible : Boolean;
                           const AControl : TCustomEdit ; const FSearchSource : TFieldDataLink;
                           const FTextSeparator : String ; const AOptions : TDBGridOptions;
                           const ALookupDisplayIndex : Integer; const ARowCount,AWidth :Word;
                           const FSearchList,FWidths : String ;const FFieldSeparator:Char ):Boolean;
var li_pos : Integer;
    ls_temp : String;
begin
  if not assigned ( FPopup )
  or not FPopup.Focused Then
  with FSearchSource.Dataset.FieldByName ( FSearchSource.FieldName ), AControl do
   if AsString > '' then
    Begin
      ls_temp := Text ; // c'est en affectant le texte que l'on passe en mode édition
      li_pos := fs_LastString ( FTextSeparator, Ls_temp );
      if li_pos>0
        Then
         Begin
          inc ( li_pos, length ( FTextSeparator ));
          ls_temp := copy ( ls_temp, 1, li_pos -1 );
         End
        Else ls_temp := '' ;
      li_pos    := SelStart ;
    end;
  if not FSearchVisible Then
   Begin
    p_ShowPopup(FPopup,AControl,FSearchSource,FSearchList,FWidths,ALookupDisplayIndex,ARowCount,AWidth,AOptions,FFieldSeparator);
    FSearchVisible:=assigned ( FPopup ) and FPopup.Visible;
    
   end;
  if not assigned ( FPopup )
  or not FPopup.Focused Then
  with FSearchSource.Dataset.FieldByName ( FSearchSource.FieldName ), AControl do
   if AsString > '' then
    Begin
      SetFocus;
      
      ls_temp   := ls_temp + AsString;
      Text      := ls_temp; // c'est en affectant le texte que l'on passe en mode édition
      SelStart  := li_pos ;
      SelLength := length ( ls_temp ) - li_pos ;
      Result    := length ( ls_temp )=li_pos;
    end;
end;



// return Continue
function fb_KeyUp ( const AEdit : TCustomEdit ;var Key : Word ; var Alocated, ASet : Boolean; const APopup : TCustomForm):Boolean;
Begin
  Result:=True;
  with AEdit do
  case Key of
    VK_ESCAPE:
        SelectAll;
    VK_DELETE :
    Begin
      Alocated:=False;
      ASet := False;
      SelText:='';
      Result:=False;
    end;
    VK_RETURN:
    Begin
      (AEdit as ISearchEdit).ValidateSearch;
      Result:=False;
    End;
    VK_DOWN:
    Begin
      if assigned ( APopup )
      and APopup.Visible Then
       Begin
        APopup.SetFocus;
        
       end;
      Result:=False;
    End;
  end;
End;

end.

