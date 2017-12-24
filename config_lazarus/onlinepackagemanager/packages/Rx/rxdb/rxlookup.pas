{ rxlookup unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxlookup;

{$I rx.inc}

interface

uses
  LCLType, LCLProc, LCLIntf, Classes, SysUtils, LResources, Forms, types,
  Controls, Graphics, Dialogs, DB, EditBtn, DBGrids, Buttons,
  LMessages, DbCtrls, GraphType, rxdbutils, RxDbGrid, rxpopupunit, Themes;

const
  TextMargin = 5;

type
  TRxCustomDBLookupCombo = class;
  TRxCustomDBLookupEdit = class;

  TClosePopup = procedure(Sender: TObject; SearchResult:boolean) of object;

  {For deciding, what we need to show in combobox in case we cannot find curvalue in lookup table.}
  TRxDBValueVariant = (rxufNone, rxufLastSuccessful, rxufOriginal);

  { TLookupSourceLink }
  TDataSourceLink = class(TDataLink)
  private
    FDataControl:TRxCustomDBLookupCombo;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  end;

  { TLookupSourceLink }

  TLookupSourceLink = class(TDataLink)
  private
    FOnActiveChanged:TNotifyEvent;
    FOnLayoutChanged:TNotifyEvent;
    FOnDataSetChanged:TNotifyEvent;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetChanged; override;
  end;

  { TRxCustomDBLookupEdit }

  TRxCustomDBLookupEdit = class(TEditButton)
  private
    FLookupDisplayIndex: Integer;
    FLookupField: string;
    FLookupDisplay: string;
    FKeyField:TField;
    //
    FLookupDataLink:TLookupSourceLink;
    FLocateObject:TLocateObject;
    FOnClosePopup: TClosePopup;
    //
    FRxPopUpForm:TPopUpForm;

    FFieldList:TStringList;
    FPopUpFormOptions:TPopUpFormOptions;
    function GetDropDownCount: Integer;
    function GetDropDownWidth: Integer;
    function GetLookupSource: TDataSource;
    function GetPopupVisible: boolean;
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetDropDownWidth(const AValue: Integer);
    procedure SetLookupDisplay(const AValue: string);
    procedure SetLookupDisplayIndex(const AValue: Integer);
    procedure SetLookupField(const AValue: string);
    procedure SetLookupSource(AValue: TDataSource);
    procedure SetPopUpFormOptions(const AValue: TPopUpFormOptions);
    //
    procedure ShowList;
    procedure HideList;
    procedure ShowPopUp;
    procedure UpdateKeyValue;
  protected
    property PopUpFormOptions:TPopUpFormOptions read FPopUpFormOptions write SetPopUpFormOptions;
    procedure ButtonClick; override;
    function GetDefaultGlyphName: String; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure InternalClosePopup(AResult:boolean);virtual;
    //
    procedure LookupDataSetChanged(Sender: TObject);  virtual;
    procedure ListLinkActiveChanged(Sender: TObject); virtual;
    //
    property OnClosePopup:TClosePopup read FOnClosePopup write FOnClosePopup;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read GetDropDownWidth write SetDropDownWidth default 0;
    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read FLookupDisplayIndex write SetLookupDisplayIndex default 0;
    property LookupField: string read FLookupField write SetLookupField;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property PopupVisible:boolean read GetPopupVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Spacing default 0;
  end;

  TRxLookupEdit = class(TRxCustomDBLookupEdit)
  published
    property DropDownCount;
    property DropDownWidth;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupField;
    property LookupSource;
    property PopUpFormOptions;
    property OnClosePopup;
  end;

  { TRxCustomDBLookupCombo }
  TRxCustomDBLookupCombo = class (TCustomControl)
  private
    FOnChangeData: TNotifyEvent;
    //
    FStopClick:boolean;
    //FDataLink:TFieldDataLink;
    FDataLink:TDataSourceLink;
    FDataFieldName: string;
    FDataField :TField;
    //
    FLookupDataLink:TLookupSourceLink;
    FLocateObject:TLocateObject;
    FLookupField: string;
    FLookupDisplay: string;
    FDisplayField:TField;
    FKeyField:TField;
    FLookupDisplayIndex: Integer;
    FListActive:boolean;
    //
    FEmptyItemColor: TColor;
    FEmptyValue: string;
    FOnChange: TNotifyEvent;
    FOnClosePopup: TClosePopup;
    FPopUpFormOptions: TPopUpFormOptions;
    //
    FRxPopUpForm:TPopUpForm;
    FFieldList:TStringList;
    FValuesList:TStringList;
    FValue:string;
    //Visual
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput : Boolean;
    FOnButtonClick : TNotifyEvent;
    FReadOnly: boolean;
    FDisplayAll: boolean;
    FUnfindedValue: TRxDBValueVariant;
    FSuccesfullyFind : boolean;

    FOnSelect : TNotifyEvent;
    procedure SetValue(const Value: string);
    function GetKeyValue: Variant;
    procedure SetKeyValue(const Value: Variant);

    function GetDataSource: TDataSource;
    function GetDisplayAll: Boolean;
    function GetDropDownCount: Integer;
    function GetDropDownWidth: Integer;
    function GetLookupSource: TDataSource;
    function GetMinHeight: Integer;
    function GetBorderSize: Integer;
    procedure CheckButtonVisible;
    function GetButtonWidth: Integer;
    function GetFlat: Boolean;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: Integer;
    function GetOnGetGridCellProps: TGetCellPropsEvent;
    function GetPopupVisible: boolean;
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetDataFieldName(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetDisplayAll(const AValue: Boolean);
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetDropDownWidth(const AValue: Integer);
    procedure SetEmptyItemColor(const AValue: TColor);
    procedure SetEmptyValue(const AValue: string);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetLookupDisplay(const AValue: string);
    procedure SetLookupDisplayIndex(const AValue: Integer);
    procedure SetLookupField(const AValue: string);
    procedure SetLookupSource(const AValue: TDataSource);
    procedure SetNumGlyphs(const AValue: Integer);
    procedure SetOnGetGridCellProps(const AValue: TGetCellPropsEvent);
    procedure SetPopUpFormOptions(const AValue: TPopUpFormOptions);
    procedure SetReadOnly(const AValue: boolean);
    function StoreEmpty: boolean;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure CMExit(var Message:TLMessage); message CM_EXIT;
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure PaintDisplayValues(ACanvas: TCanvas; R: TRect; ALeft: Integer);
    procedure CheckNotCircular;
    procedure DisplayValueChanged;
    procedure DataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    procedure UpdateFieldValues;
    procedure SetValueKey(const Value: string);
    procedure UpdateKeyValue;
    procedure KeyValueChanged;
    procedure UpdateData;
    procedure NeedUpdateData;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    {procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean = false;
                               WithThemeSpace: boolean = true); override;
    class function GetControlClassDefaultSize: TSize; override;}
    procedure ShowList; virtual;
    procedure OnInternalClosePopup(AResult:boolean);virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoPositionButton; virtual;
    procedure DoChange; virtual;
    procedure DoChangeData; virtual;
    procedure DoButtonClick(Sender: TObject); virtual;
    Procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure Click; override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;

    procedure Paint; override;

    procedure LookupDataSetChanged(Sender: TObject);  virtual;
    procedure ListLinkActiveChanged(Sender: TObject); virtual;

    //
    property Button: TSpeedButton read FButton;
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property ButtonOnlyWhenFocused : Boolean Read FButtonNeedsFocus Write SetButtonNeedsFocus;
    property DirectInput : Boolean read FDirectInput write FDirectInput Default True;
    property DisplayAllFields: Boolean read GetDisplayAll write SetDisplayAll default False;
    property Flat : Boolean read GetFlat write SetFlat;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnChangeData : TNotifyEvent read FOnChangeData write FOnChangeData;
    property ReadOnly:boolean read FReadOnly write SetReadOnly;
    property EmptyValue: string read FEmptyValue write SetEmptyValue stored StoreEmpty;
    property EmptyItemColor: TColor read FEmptyItemColor write SetEmptyItemColor default clWindow;
    //data
    property PopUpFormOptions:TPopUpFormOptions read FPopUpFormOptions write SetPopUpFormOptions;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read GetDropDownWidth write SetDropDownWidth default 0;
    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read FLookupDisplayIndex write SetLookupDisplayIndex default 0;
    property LookupField: string read FLookupField write SetLookupField;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property OnGetGridCellProps: TGetCellPropsEvent read GetOnGetGridCellProps
      write SetOnGetGridCellProps;

    property Value: string read FValue write SetValue stored False;
    property KeyValue: Variant read GetKeyValue write SetKeyValue stored False;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnClosePopup:TClosePopup read FOnClosePopup write FOnClosePopup;

    property UnfindedValue : TRxDBValueVariant read FUnfindedValue write FUnfindedValue default rxufNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PopupVisible:boolean read GetPopupVisible;
  end;
  
  { TRxDBLookupCombo }
  TRxDBLookupCombo = class(TRxCustomDBLookupCombo)
  protected
    procedure OnInternalClosePopup(AResult:boolean);override;
  public
    property Value;
    property KeyValue;
    property Text;
  published
    property AutoSize;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    Property ButtonWidth;
    property Color;
    property DataField;
    property DataSource;
    Property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property PopUpFormOptions;
    Property Flat;
    property Font;
    property Glyph;
    property EmptyValue;
    property EmptyItemColor;
//    property MaxLength;
    property NumGlyphs;
    Property OnButtonClick;
    property OnChange;
    property OnChangeData;
    property OnClick;
    property OnClosePopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnGetGridCellProps;
    property OnSelect;

    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{    property Width default 100;
    property Height default 23;}

    property DisplayAllFields;
    property DropDownCount;
    property DropDownWidth;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupField;
    property LookupSource;
    property UnfindedValue;
  end;
  
implementation
uses rxVCLUtils, Math, rxdconst;

type
  TDbGridAccess = class(TDbGrid)
  end;
  TPopUpFormAccess = class(TPopUpForm)
  end;


{ TRxCustomDBLookupEdit }

function TRxCustomDBLookupEdit.GetLookupSource: TDataSource;
begin
  Result:=FLookupDataLink.DataSource;
end;

function TRxCustomDBLookupEdit.GetPopupVisible: boolean;
begin
  Result:=Assigned(FRxPopUpForm);
end;

function TRxCustomDBLookupEdit.GetDropDownCount: Integer;
begin
  Result:=FPopUpFormOptions.DropDownCount;
end;

function TRxCustomDBLookupEdit.GetDropDownWidth: Integer;
begin
  Result:=FPopUpFormOptions.DropDownWidth;
end;

procedure TRxCustomDBLookupEdit.SetDropDownCount(const AValue: Integer);
begin
  FPopUpFormOptions.DropDownCount:=AValue;
end;

procedure TRxCustomDBLookupEdit.SetDropDownWidth(const AValue: Integer);
begin
  FPopUpFormOptions.DropDownWidth:=AValue;
end;

procedure TRxCustomDBLookupEdit.SetLookupDisplay(const AValue: string);
var
  S1, S2:string;
  K:integer;
begin
  if FLookupDisplay=AValue then exit;
  FLookupDisplay:=AValue;
  FFieldList.Clear;
  S2:=AValue;
  while S2<>'' do
  begin
    K:=Pos(';', S2);
    if K>0 then
    begin
      S1:=Copy(S2, 1, K-1);
      Delete(S2, 1, K);
    end
    else
    begin
      S1:=S2;
      S2:='';
    end;
    FFieldList.Add(S1);
  end;
end;

procedure TRxCustomDBLookupEdit.SetLookupDisplayIndex(const AValue: Integer);
begin
  if FLookupDisplayIndex=AValue then exit;
  FLookupDisplayIndex:=AValue;
end;

procedure TRxCustomDBLookupEdit.SetLookupField(const AValue: string);
begin
  if FLookupField = AValue then exit;
  FLookupField:=AValue;
end;

procedure TRxCustomDBLookupEdit.SetLookupSource(AValue: TDataSource);
begin
  FLookupDataLink.DataSource:=AValue;
end;

procedure TRxCustomDBLookupEdit.SetPopUpFormOptions(
  const AValue: TPopUpFormOptions);
begin
  FPopUpFormOptions.Assign(AValue);
end;

procedure TRxCustomDBLookupEdit.ShowList;
{var
  i,W:integer;
  GC:TColumn;}
begin
  if FLookupDataLink.Active and not PopupVisible then
  begin
    ShowPopUp;
  end;
end;

procedure TRxCustomDBLookupEdit.HideList;
begin

end;

procedure TRxCustomDBLookupEdit.ShowPopUp;
var
  R:TPoint;
  FValue:string;
  {$IFDEF LINUX}
  TempF:TPopUpForm;
  {$ENDIF}
begin

  if FLookupDataLink.Active then
    if not PopupVisible then
    begin

      FValue := Text;

      FLocateObject.Locate(FLookupField, FValue, true, false);

(*     FRxPopUpForm:=ShowRxDBPopUpForm(Self, FLookupDataLink.DataSet, @OnClosePopup,
        FPopUpFormOptions, FLookupDisplay, LookupDisplayIndex, 0 {ButtonWidth}, Font);*)

      FRxPopUpForm:=ShowRxDBPopUpForm(Self, FLookupDataLink.DataSet, @InternalClosePopup,
        FPopUpFormOptions, FLookupDisplay, LookupDisplayIndex, 0 {ButtonWidth}, Font);
  {$IFDEF LINUX}
      TempF:=FRxPopUpForm;
      if FRxPopUpForm.ShowModal = mrOk then
        InternalClosePopup(true);
      TempF.Free;
      FRxPopUpForm:=nil
  {$ENDIF}

    end
end;


procedure TRxCustomDBLookupEdit.UpdateKeyValue;
var
  S:string;
begin
  S:=FFieldList[FLookupDisplayIndex];
  if FLookupDataLink.Active then
    Text:=FLookupDataLink.DataSet.FieldByName(S).AsString;
end;

procedure TRxCustomDBLookupEdit.ButtonClick;
begin
  inherited ButtonClick;
  if PopupVisible then
    HideList
  else
    ShowList;
end;

function TRxCustomDBLookupEdit.GetDefaultGlyphName: String;
begin
  Result:='rxbtn_downarrow';
end;

procedure TRxCustomDBLookupEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_RETURN]) and PopupVisible then
  begin
{    if Key=VK_RETURN then HideList
    else
      TDbGridAccess(Flist).KeyDown(Key, Shift);
    Key := 0;}
  end
  else
  if (Key = VK_DOWN) and ((ssAlt in Shift) or (ssCtrl in Shift)) then
  begin
    ShowList;
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
{  FIgnoreChange := (SelLength > 0) or (Key = VK_BACK);}
  if not (PopupVisible or ReadOnly) and (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
  begin
    case Key of
      VK_UP: if not FLookupDataLink.DataSet.BOF then FLookupDataLink.DataSet.Prior;
      VK_DOWN: if not FLookupDataLink.DataSet.EOF then FLookupDataLink.DataSet.Next;
    end;
    Text:=FLookupDataLink.DataSet.FieldByName(FFieldList[FLookupDisplayIndex]).AsString;
    Key:=0;
  end;
end;

procedure TRxCustomDBLookupEdit.InternalClosePopup(AResult: boolean);
begin
  if Assigned(FOnClosePopup) then
    FOnClosePopup(Self, AResult);

{$IFDEF WINDOWS}
  FRxPopUpForm:=nil;
{$ENDIF}
end;

procedure TRxCustomDBLookupEdit.LookupDataSetChanged(Sender: TObject);
begin
  UpdateKeyValue;
  Invalidate;
end;

procedure TRxCustomDBLookupEdit.ListLinkActiveChanged(Sender: TObject);
var
  DataSet: TDataSet;
begin
  FKeyField := nil;
  DataSet:=nil;
  if FLookupDataLink.Active and (FLookupField <> '') then
  begin
    DataSet := FLookupDataLink.DataSet;
    FKeyField := DataSet.FieldByName(FLookupField);
   end;
  FLocateObject.DataSet := DataSet;

  UpdateKeyValue
end;

constructor TRxCustomDBLookupEdit.Create(AOwner: TComponent);
var
  P:TBitmap;
begin
  inherited Create(AOwner);
  Spacing:=0;
  FLocateObject:=CreateLocate(nil);

  //Lookup
  FLookupDataLink:=TLookupSourceLink.Create;
  FLookupDataLink.FOnActiveChanged:=@ListLinkActiveChanged;
  FLookupDataLink.FOnLayoutChanged:=@ListLinkActiveChanged;
  FLookupDataLink.FOnDataSetChanged:=@LookupDataSetChanged;

  FFieldList:=TStringList.Create;
  ButtonWidth:=15;
  FPopUpFormOptions:=TPopUpFormOptions.Create(Self);
end;

destructor TRxCustomDBLookupEdit.Destroy;
begin
  FreeAndNil(FLocateObject);
  FreeAndNil(FPopUpFormOptions);
  FFieldList.Clear;
  FreeAndNil(FFieldList);
  FreeAndNil(FLookupDataLink);
  inherited Destroy;
end;


{ TRxCustomDBLookupCombo }

function TRxCustomDBLookupCombo.GetMinHeight: Integer;
begin
  Result := 15{DefaultTextHeight} + GetBorderSize + 3;
end;

function TRxCustomDBLookupCombo.GetDisplayAll: Boolean;
begin
  Result := FDisplayAll;
end;

function TRxCustomDBLookupCombo.GetDropDownCount: Integer;
begin
  Result:=FPopUpFormOptions.DropDownCount
end;

function TRxCustomDBLookupCombo.GetDropDownWidth: Integer;
begin
  Result:=FPopUpFormOptions.DropDownWidth;
end;

function TRxCustomDBLookupCombo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TRxCustomDBLookupCombo.GetLookupSource: TDataSource;
begin
  Result:=FLookupDataLink.DataSource;
end;

function TRxCustomDBLookupCombo.GetBorderSize: Integer;
{var
  Params: TCreateParams;
  R: TRect;}
begin
{  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;}
  Result := 3;
end;

procedure TRxCustomDBLookupCombo.CheckButtonVisible;
begin
  if Assigned(FButton) then
    FButton.Visible:=(csdesigning in ComponentState) or
                     (Visible and (Focused or not FButtonNeedsFocus));
end;

function TRxCustomDBLookupCombo.GetButtonWidth: Integer;
begin
  if Assigned(FButton) then Result:=FButton.Width
  else Result:=0;
end;

function TRxCustomDBLookupCombo.GetFlat: Boolean;
begin
  if Assigned(FButton) then Result:=FButton.Flat
  else Result:=false;
end;

function TRxCustomDBLookupCombo.GetGlyph: TBitmap;
begin
  if Assigned(FButton) then Result:=FButton.Glyph
  else Result:=nil;
end;

function TRxCustomDBLookupCombo.GetNumGlyphs: Integer;
begin
  if Assigned(FButton) then Result:=FButton.NumGlyphs
  else Result:=0;
end;

function TRxCustomDBLookupCombo.GetOnGetGridCellProps: TGetCellPropsEvent;
begin
  Result:=FPopUpFormOptions.OnGetCellProps;
end;

function TRxCustomDBLookupCombo.GetPopupVisible: boolean;
begin
  Result:=Assigned(FRxPopUpForm);
end;

procedure TRxCustomDBLookupCombo.SetButtonNeedsFocus(const AValue: Boolean);
begin
  if FButtonNeedsFocus<>AValue then
  begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
  end;
end;

procedure TRxCustomDBLookupCombo.SetButtonWidth(const AValue: Integer);
begin
  if Assigned(FButton) then
    FButton.Width:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetDataFieldName(const AValue: string);
begin
  if FDataFieldName <> AValue then
  begin
    FDataFieldName := AValue;
    DataLinkActiveChanged;
  end;
end;

procedure TRxCustomDBLookupCombo.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  if AValue <> nil then AValue.FreeNotification(Self);
end;

procedure TRxCustomDBLookupCombo.SetDisplayAll(const AValue: Boolean);
begin
  if FDisplayAll <> AValue then
  begin
    FDisplayAll := AValue;
    Invalidate;
  end;
end;

procedure TRxCustomDBLookupCombo.SetDropDownCount(const AValue: Integer);
begin
  FPopUpFormOptions.DropDownCount:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetDropDownWidth(const AValue: Integer);
begin
  FPopUpFormOptions.DropDownWidth:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetEmptyItemColor(const AValue: TColor);
begin
  if FEmptyItemColor=AValue then exit;
  FEmptyItemColor:=AValue;
  if not (csReading in ComponentState) then
    Invalidate;
end;

procedure TRxCustomDBLookupCombo.SetEmptyValue(const AValue: string);
begin
  if FEmptyValue=AValue then exit;
  FEmptyValue:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetFlat(const AValue: Boolean);
begin
  if Assigned(FButton) then
    FButton.Flat:=AValue;
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.SetGlyph(const AValue: TBitmap);
begin
  if Assigned(FButton) then
    FButton.Glyph:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetLookupDisplay(const AValue: string);
var
  S1, S2:string;
  K:integer;
begin
  if FLookupDisplay=AValue then exit;
  FLookupDisplay:=AValue;
  FFieldList.Clear;
  S2:=AValue;
  while S2<>'' do
  begin
    K:=Pos(';', S2);
    if K>0 then
    begin
      S1:=Copy(S2, 1, K-1);
      Delete(S2, 1, K);
    end
    else
    begin
      S1:=S2;
      S2:='';
    end;
    FFieldList.Add(S1);
  end;
  DisplayValueChanged;
end;

procedure TRxCustomDBLookupCombo.SetLookupDisplayIndex(const AValue: Integer);
begin
  if FLookupDisplayIndex=AValue then exit;
  FLookupDisplayIndex:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetLookupField(const AValue: string);
begin
  FLookupField:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetLookupSource(const AValue: TDataSource);
begin
  FLookupDataLink.DataSource:=AValue;
  FLocateObject.DataSet:=FLookupDataLink.DataSet;
  FPopUpFormOptions.DataSource:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetNumGlyphs(const AValue: Integer);
begin
  if Assigned(FButton) then
    FButton.NumGlyphs:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetOnGetGridCellProps(
  const AValue: TGetCellPropsEvent);
begin
  FPopUpFormOptions.OnGetCellProps:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetPopUpFormOptions(
  const AValue: TPopUpFormOptions);
begin
  FPopUpFormOptions.Assign(AValue);
end;

procedure TRxCustomDBLookupCombo.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
end;

function TRxCustomDBLookupCombo.StoreEmpty: boolean;
begin
  Result:=true;
end;

procedure TRxCustomDBLookupCombo.WMSetFocus(var Message: TLMSetFocus);
begin
  FButton.Visible:=True;
  inherited WMSetFocus(Message);
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.WMKillFocus(var Message: TLMKillFocus);
begin
  if FButtonNeedsFocus then
    FButton.Visible:=false;
  inherited WMKillFocus(Message);
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.CMExit(var Message: TLMessage);
begin
  inherited;
end;

procedure TRxCustomDBLookupCombo.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TRxCustomDBLookupCombo.PaintDisplayValues(ACanvas: TCanvas; R: TRect;
  ALeft: Integer);
var
  I, LastIndex, TxtWidth: Integer;
  X, W, ATop, ARight: Integer;
  S: string;
  F:TField;
begin
  if (FValuesList.Count=0) or (not LookupSource.DataSet.Active) then exit;
  if ColorToRGB(Self.Color) <> ColorToRGB(clBtnFace) then
    ACanvas.Pen.Color := clBtnFace
  else
    ACanvas.Pen.Color := clBtnShadow;
  LastIndex := FValuesList.Count-1;
  TxtWidth := ACanvas.TextWidth('W');
  ATop := Max(0, (HeightOf(R) - ACanvas.TextHeight('Xy')) div 2);
  ARight := R.Right;
  Inc(R.Left, ALeft);
  for I := 0 to LastIndex do
  begin
    F:=LookupSource.DataSet.FieldByName(FFieldList[i]);
    S := FValuesList[i];

    if (FPopUpFormOptions.Columns.Count>i) and (I<LastIndex) then
      W := FPopUpFormOptions.Columns[i].Width
    else
    begin
      W :=  F.DisplayWidth;
      if I < LastIndex then
        W := W * TxtWidth + 4
      else
        W := ARight - R.Left;
    end;
    
    X := 2;
    R.Right := R.Left + W;
    case F.AlignMent of
      taRightJustify: X := W - ACanvas.TextWidth(S) - 3;
      taCenter: X := (W - ACanvas.TextWidth(S)) div 2;
    end;
    ACanvas.TextRect(R, R.Left + Max(0, X), ATop, S);
    Inc(R.Left, W);
    if I < LastIndex then
    begin
      ACanvas.MoveTo(R.Right, R.Top);
      ACanvas.LineTo(R.Right, R.Bottom);
      Inc(R.Left);
    end;
    if R.Left >= ARight then
      Break;
  end;
end;

procedure TRxCustomDBLookupCombo.CheckNotCircular;
begin
  if FDataLink.Active and ((DataSource = LookupSource) or
    (FDataLink.DataSet = FLookupDataLink.DataSet)) then
    _DBError(SCircularDataLink);
end;

procedure TRxCustomDBLookupCombo.DisplayValueChanged;
begin
  FDisplayField:=nil;
  if FLookupDataLink.Active and (FLookupDisplay <> '') then
  begin
    FDisplayField := FLookupDataLink.DataSet.FieldByName(FFieldList[FLookupDisplayIndex]);
    if PopupVisible then
    begin
//      UpdateData;
      UpdateFieldValues;
    end;
  end;
end;


procedure TRxCustomDBLookupCombo.DataLinkActiveChanged;
begin
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := FDataLink.DataSet.FieldByName(FDataFieldName);
  end
  else
  begin
    FDataField := nil;
  end;
  DataLinkRecordChanged(nil);
end;

procedure TRxCustomDBLookupCombo.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FDataField) then
  begin
    if FDataField <> nil then
    begin
      SetValueKey(FDataField.AsString);
    end
    else
      SetValueKey(FEmptyValue);
  end
end;

procedure TRxCustomDBLookupCombo.UpdateFieldValues;
var
  i, k:integer;
  F:TField;
begin
  FValuesList.Clear;
  if not Assigned(FDataField) then
  begin
    if FLookupDataLink.Active then
      if (Self.FSuccesfullyFind) or (Self.UnfindedValue = rxufLastSuccessful) then
      begin
        for i:=0 to FFieldList.Count-1 do
        begin
          F:=FLookupDataLink.DataSet.FieldByName(FFieldList[i]);
          k:=FValuesList.Add(F.DisplayText);
          FValuesList.Objects[k]:=TObject(PtrInt(F.DisplayWidth));
        end;
      end
      else
      case Self.UnfindedValue of
        rxufNone : {Do nothing};
        rxufOriginal : FValuesList.Add(FValue);//Show original field value...
      end;
  end
  else
  if Assigned(FDataField) then
  begin
    if FDataField.IsNull then
      FValuesList.Add(FEmptyValue)
    else
    if FLookupDataLink.Active then
      if (Self.FSuccesfullyFind) or (Self.UnfindedValue = rxufLastSuccessful) then
      begin
        for i:=0 to FFieldList.Count-1 do
        begin
          F:=FLookupDataLink.DataSet.FieldByName(FFieldList[i]);
          k:=FValuesList.Add(F.DisplayText);
          FValuesList.Objects[k]:=TObject(PtrInt(F.DisplayWidth));
        end;
      end
      else
        case Self.UnfindedValue of
          rxufNone : {Do nothing};
          rxufOriginal : FValuesList.Add(FValue);//Show original field value...
        end;
  end;
end;

procedure TRxCustomDBLookupCombo.ShowList;
{$IFDEF LINUX}
var
  TempF:TPopUpForm;
{$ENDIF}
begin
  if Assigned(FLookupDataLink.DataSet) and (FLookupDataLink.DataSet.Active) then
    if not PopupVisible then
    begin
      if FDataField<>nil then
        if FDataField <> nil then
          FValue := FDataField.AsString
        else
          FValue := FEmptyValue;

      if not Assigned(FDataField) then
      begin
        if not FLocateObject.Locate(FLookupField, FValue, true, false) then
  	FLookupDataLink.DataSet.First;
      end
      else
      if Assigned(FDataField) and not FDataField.IsNull then
      begin
       if not FLocateObject.Locate(FLookupField, FValue, true, false) then
         FLookupDataLink.DataSet.First;//In case we cannot find curvalue...
      end
      else
      if FLookupDataLink.Active then
        FLookupDataLink.DataSet.First;

      FRxPopUpForm:=ShowRxDBPopUpForm(Self, FLookupDataLink.DataSet, @OnInternalClosePopup,
        FPopUpFormOptions, FLookupDisplay, LookupDisplayIndex, 0 {ButtonWidth}, Font);
{$IFDEF LINUX}
      TempF:=FRxPopUpForm;
      if FRxPopUpForm.ShowModal = mrOk then
        {OnInternalClosePopup(true)};
      TempF.Free;
      FRxPopUpForm:=nil
{$ENDIF}
    end
end;


procedure TRxCustomDBLookupCombo.SetValueKey(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(FLookupDataLink.DataSet) and (FLookupDataLink.DataSet.Active) then
    begin
      FSuccesfullyFind := FLocateObject.Locate(FLookupField, FValue, true, false);
      KeyValueChanged;
    end;
  end;
end;

procedure TRxCustomDBLookupCombo.UpdateKeyValue;
begin
  if Assigned(FDataField) then
    if FDataField <> nil then
      FValue := FDataField.AsString
    else
      FValue := FEmptyValue;

  if not Assigned(FDataField) then
  begin
    if FValue=FEmptyValue then
      FSuccesfullyFind := false
    else
      FSuccesfullyFind := FLocateObject.Locate(FLookupField, FValue, true, false);
  end
  else
  if FDataField.IsNull then
     FSuccesfullyFind := false
  else
  if not FDataField.IsNull then
     FSuccesfullyFind := FLocateObject.Locate(FLookupField, FValue, true, false);
  KeyValueChanged;
end;

procedure TRxCustomDBLookupCombo.KeyValueChanged;
begin
  UpdateFieldValues;
  Invalidate;
  DoChange;
end;

procedure TRxCustomDBLookupCombo.UpdateData;
begin
  //We have nothing to do here...
end;

procedure TRxCustomDBLookupCombo.NeedUpdateData;
begin
  if FLookupDataLink.Active and Assigned(FDataField) and Assigned(FKeyField) then
  begin
    if FKeyField.IsNull then FDataField.Clear
    else FDataField.AsString:=FKeyField.AsString;
    DoChangeData;
  end;
end;

procedure TRxCustomDBLookupCombo.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  tmpCanvas: TCanvas;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  // ignore width
  PreferredWidth:=0;
  tmpCanvas := GetWorkingCanvas(Canvas);
  try
    PreferredHeight:=Canvas.TextHeight('Wg')+12;
    //PreferredWidth:=Canvas.TextWidth('W')*12;
  finally
    if TmpCanvas<>Canvas then
      FreeWorkingCanvas(tmpCanvas);
  end;
end;

{procedure TRxCustomDBLookupCombo.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, Raw,
    WithThemeSpace);
end;


class function TRxCustomDBLookupCombo.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 50;
end;
}
procedure TRxCustomDBLookupCombo.OnInternalClosePopup(AResult: boolean);
begin
  if Assigned(FRxPopUpForm) and AResult and (pfgColumnResize in FPopUpFormOptions.Options) then
    FillPopupWidth(FPopUpFormOptions, FRxPopUpForm);
    
  if Assigned(FOnClosePopup) then
    FOnClosePopup(Self, AResult);

  if FRxPopUpForm=nil then
  begin
    SetFocus;
    Exit;
  end;

  FRxPopUpForm:=nil;
  if not AResult then
    UpdateKeyValue
  else
  if AResult and not Assigned(FDataLink.DataSource) and (FLookupDataLink.Active) then
  begin
    if FKeyField.IsNull then
      SetValueKey(FEmptyValue)
    else
      SetValueKey(FKeyField.AsString);
  end
  else

  if AResult and Assigned(FDataLink.DataSource) then
  begin
    FDataLink.Edit;
    Visible:=true;
    NeedUpdateData;//We need to update DataField;
  end;

  SetFocus;
  if (AResult) and (Assigned(FOnSelect)) then
    FOnSelect(Self);
end;

procedure TRxCustomDBLookupCombo.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_RETURN, VK_HOME, VK_END]) and PopupVisible then
  begin
    TPopUpFormAccess(FRxPopUpForm).KeyDown(Key, Shift);
  end
  else
  if not PopupVisible then
  begin
    if (Key = VK_DOWN) and ((ssAlt in Shift) or (ssCtrl in Shift)) then
    begin
      ShowList;
      Key := 0;
    end
    else
    if (Key = VK_ESCAPE) and not (Assigned(FDataField)) then
    begin
      SetValueKey(FEmptyValue);
      if Assigned(FOnSelect) then
        FOnSelect(Self);
      Key:=0;
    end
    else
    if (Key = VK_ESCAPE) and (not FDataField.IsNull) and (FDataLink.Edit) then
    begin
      FDataField.Clear;
      UpdateKeyValue;
      if Assigned(FOnSelect) then
        FOnSelect(Self);
      DoChangeData;
      Key:=0;
    end;
  end;
  inherited KeyDown(Key, Shift);
  if FLookupDataLink.Active and FDataLink.Active and not (PopupVisible or ReadOnly) then
  begin
    if (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
    begin
      FDataLink.Edit;
      if not FDataField.IsNull then
      begin
        //FLocateObject.Locate(FLookupField, FDataField.AsString, true, false);
        If not FLocateObject.Locate(FLookupField, FDataField.AsString, true, false) then FLookupDataLink.DataSet.First;
        case Key of
          VK_UP: if not FLookupDataLink.DataSet.BOF then
                     FLookupDataLink.DataSet.Prior;
          VK_DOWN: if not FLookupDataLink.DataSet.EOF then
                     FLookupDataLink.DataSet.Next;
        end;
      end;
      //FDataLink.UpdateRecord; -- no need more...
      Self.NeedUpdateData;
      if Assigned(FOnSelect) then
        FOnSelect(Self);
      KeyValueChanged;
      Key:=0;
    end
  end
  else
  if FLookupDataLink.Active and not (PopupVisible or ReadOnly) then
  begin
    if (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
    begin
      case Key of
        VK_UP: if not FLookupDataLink.DataSet.BOF then
                   FLookupDataLink.DataSet.Prior;
        VK_DOWN: if not FLookupDataLink.DataSet.EOF then
                   FLookupDataLink.DataSet.Next;
      end;
      SetValueKey(FKeyField.AsString);
      if Assigned(FOnSelect) then
        FOnSelect(Self);
      Key:=0;
    end
  end;

end;

procedure TRxCustomDBLookupCombo.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  if not (PopupVisible) and ((UTF8Key >= #32) or (UTF8Key = #8)) then
    ShowList;
  inherited UTF8KeyPress(UTF8Key);
  if PopupVisible then
    FRxPopUpForm.UTF8KeyPress(UTF8Key);
end;

procedure TRxCustomDBLookupCombo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
    CheckButtonVisible;
end;

procedure TRxCustomDBLookupCombo.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if not (csReading in ComponentState) and (Height < GetMinHeight) then
    AHeight := GetMinHeight
  else
  begin
    if (csDesigning in ComponentState) then
      if (Height < GetMinHeight) then
        AHeight := GetMinHeight;
  end;

  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
//  DoPositionButton;
end;

procedure TRxCustomDBLookupCombo.DoPositionButton;
begin
  if FButton <> nil then
    FButton.SetBounds(Left+Width, Top, FButton.Width, Height);
end;

procedure TRxCustomDBLookupCombo.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRxCustomDBLookupCombo.DoChangeData;
begin
  if Assigned(FOnChangeData) then
    FOnChangeData(Self)
end;

procedure TRxCustomDBLookupCombo.DoButtonClick(Sender: TObject);
begin
  if (not FReadOnly) and (not FStopClick) then//We can do something if and only if that's not ReadOnly field...
  begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
    ShowList;
  end;
  FStopClick:=false;
end;

procedure TRxCustomDBLookupCombo.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
end;

procedure TRxCustomDBLookupCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TRxCustomDBLookupCombo.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);
  CheckButtonVisible;
end;

procedure TRxCustomDBLookupCombo.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);
  if FButton<>nil then
    FButton.Enabled:=Enabled;
end;

procedure TRxCustomDBLookupCombo.Click;
begin
 inherited Click;
 If not Self.PopupVisible then
   DoButtonClick(Self);
 FStopClick:=false;
end;

function TRxCustomDBLookupCombo.RealGetText: TCaption;
begin
  if PopupVisible then
    Result:=inherited RealGetText
  else
  if (FLookupDisplayIndex>=0) and (FLookupDisplayIndex < FValuesList.Count) then
    Result:=FValuesList[FLookupDisplayIndex]
  else
    Result:='';
end;

procedure TRxCustomDBLookupCombo.RealSetText(const Value: TCaption);
var
  LookFieldName:string;
begin
  inherited RealSetText(Value);
  if not PopupVisible then
  begin
    if Assigned(FLookupDataLink.DataSet) and (FLookupDataLink.DataSet.Active) then
    begin
      if (FLookupDisplayIndex>=0) and (FLookupDisplayIndex<FFieldList.Count) then
      begin
        LookFieldName:=FFieldList[FLookupDisplayIndex];
        FSuccesfullyFind := FLocateObject.Locate(LookFieldName, Value, true, false);
        if FSuccesfullyFind and Assigned(FKeyField) then
          SetValue(FKeyField.AsString);
        KeyValueChanged;
      end;
    end;
  end;
end;

procedure TRxCustomDBLookupCombo.Paint;
var
  Selected:boolean;
  R, R1: TRect;
  AText: string;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := Focused and (not (csPaintCopy in ControlState)) and  (not PopupVisible);
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end
  else
  if not Enabled {and NewStyleControls }then
  begin
    Canvas.Font.Color := clInactiveCaption;
  end;

  SetRect(R, 0, 0, ClientWidth, ClientHeight);
  if Flat then
  begin
    Canvas.Frame3d(R, 3, bvLowered);
  end
  else
  begin
    RxFrame3D(Canvas, R, clWindowFrame, clBtnHighlight, 1);
    RxFrame3D(Canvas, R, clBtnShadow, clBtnFace, 1);
  end;

  if ClientWidth > 6 then
  begin
    SetRect(R1, 3, 3, ClientWidth - 3, ClientHeight - 3);
    Canvas.FillRect(R1);
    R.Right:=R.Right - GetButtonWidth;
    if PopupVisible and (Caption<>'') then
    begin
      AText:=Caption;
      Canvas.TextRect(R, TextMargin, Max(0, (HeightOf(R) - Canvas.TextHeight('Wg')) div 2), AText);
    end
    else
    if FDisplayAll then
      PaintDisplayValues(Canvas, R, TextMargin)
    else
    begin
      if Assigned(FDataField) and FDataField.IsNull then
      begin
        SetRect(R1, 6, 6, ClientWidth - 6 - GetButtonWidth, ClientHeight - 6);
        Canvas.Brush.Color:=FEmptyItemColor;
        Canvas.FillRect(R1);
        AText:=FEmptyValue
      end
      else
      if FValuesList.Count>0 then
        AText:=FValuesList[FLookupDisplayIndex]
      else
        AText:='';
      Canvas.TextRect(R, TextMargin, Max(0, (HeightOf(R) - Canvas.TextHeight('Wg')) div 2), AText);
    end
  end;
end;

procedure TRxCustomDBLookupCombo.LookupDataSetChanged(Sender: TObject);
begin
  if PopupVisible then
  begin
    FSuccesfullyFind := true;
    UpdateFieldValues;
    Invalidate;
  end;
end;

procedure TRxCustomDBLookupCombo.ListLinkActiveChanged(Sender: TObject);
var
  DataSet: TDataSet;
begin
  FListActive := False;
  FKeyField := nil;
  FDisplayField := nil;
  DataSet:=nil;
  if FLookupDataLink.Active and (FLookupField <> '') and (FFieldList.Count>FLookupDisplayIndex) and (FLookupDisplayIndex>=0) then
  begin
    CheckNotCircular;
    DataSet := FLookupDataLink.DataSet;
    FKeyField := DataSet.FieldByName(FLookupField);
    FListActive := True;
    FDisplayField := FLookupDataLink.DataSet.FieldByName(FFieldList[FLookupDisplayIndex]);
  end;
  FLocateObject.DataSet := DataSet;

  if not (csDestroying in ComponentState) then
  begin
    if FListActive and Assigned(FDataField) then UpdateKeyValue
//    else KeyValueChanged;
  end;
end;

procedure TRxCustomDBLookupCombo.SetValue(const Value: string);
begin
  if (Value <> FValue) then
  begin
    if FListActive and not ReadOnly and (FDataLink.DataSource <> nil) and FDataLink.Edit then
    begin
      FDataField.AsString := Value;
      DoChangeData;
    end
    else
      SetValueKey(Value);
    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;
end;

function TRxCustomDBLookupCombo.GetKeyValue: Variant;
begin
  if Value = FEmptyValue then
    Result := null
  else
    Result := Value;
end;

procedure TRxCustomDBLookupCombo.SetKeyValue(const Value: Variant);
begin
  Self.Value := Value;
end;

constructor TRxCustomDBLookupCombo.Create(AOwner: TComponent);
var
  ArrowBmp:TBitmap;
begin
  inherited Create(AOwner);
  FStopClick:=false;
  Width := 100;
  AutoSize:=true;
  FUnfindedValue:=rxufNone;
  FFieldList := TStringList.Create;
  FValuesList:= TStringList.Create;
  FLocateObject:=CreateLocate(nil);
  FPopUpFormOptions:=TPopUpFormOptions.Create(Self);
  //Lookup
  FLookupDataLink:=TLookupSourceLink.Create;
  FLookupDataLink.FOnActiveChanged:=@ListLinkActiveChanged;
  FLookupDataLink.FOnLayoutChanged:=@ListLinkActiveChanged;
  FLookupDataLink.FOnDataSetChanged:=@LookupDataSetChanged;

  //Data
  FDataLink:=TDataSourceLink.Create;
  FDataLink.FDataControl:=Self;


  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  FButton.Parent:=Self;
  CheckButtonVisible;
  FButton.OnClick := @DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  FButton.Align:=alRight;
  FButton.BorderSpacing.Around:=2;

  ControlStyle := ControlStyle - [csSetCaption];
  FDirectInput := True;
  ParentColor:=false;
  //
  Color:=clWindow;
  FEmptyItemColor:=clWindow;
//  Glyph:=CreateArrowBitmap;
  ArrowBmp:=CreateArrowBitmap;
  Glyph:=ArrowBmp;
  FreeAndNil(ArrowBmp); //free bitmap as TSpeedButton setter takes a copy of bitmap

  ButtonWidth:=15;
  TabStop:=true;
end;

destructor TRxCustomDBLookupCombo.Destroy;
begin
  FreeAndNil(FLocateObject);
  FreeAndNil(FDataLink);
  FreeAndNil(FLookupDataLink);
  FreeAndNil(FButton);
  FFieldList.Clear;
  FreeAndNil(FFieldList);
  FreeAndNil(FValuesList);
  FreeAndNil(FPopUpFormOptions);
  inherited Destroy;
end;


{ TDataSourceLink }

procedure TDataSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then
    FDataControl.DataLinkActiveChanged;
end;

procedure TDataSourceLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

procedure TDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and (FDataControl.FDataField = Field^) then
    if FDataControl.CanFocus then
    begin
      Field^ := nil;
      FDataControl.SetFocus;
    end;
end;

procedure TDataSourceLink.RecordChanged(Field: TField);
begin
  if FDataControl <> nil then
    FDataControl.DataLinkRecordChanged(Field);
end;

procedure TDataSourceLink.UpdateData;
begin
  if FDataControl <> nil then
     FDataControl.UpdateData;
end;

{ TLookupSourceLink }

procedure TLookupSourceLink.ActiveChanged;
begin
{  if FDataControl <> nil then
    FDataControl.ListLinkActiveChanged;}
  if Assigned(FOnActiveChanged) then
    FOnActiveChanged(DataSet);
end;

procedure TLookupSourceLink.LayoutChanged;
begin
{  if FDataControl <> nil then
    FDataControl.ListLinkActiveChanged;}
  if Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(DataSet);
end;

procedure TLookupSourceLink.DataSetChanged;
begin
{  if FDataControl <> nil then
    FDataControl.LookupDataSetChanged;}
  if Assigned(FOnDataSetChanged) then
    FOnDataSetChanged(DataSet);
end;

{ TRxDBLookupCombo }

procedure TRxDBLookupCombo.OnInternalClosePopup(AResult: boolean);
begin
  inherited OnInternalClosePopup(AResult);
  if MouseEntered or FButton.MouseEntered then
    FStopClick:=true;
end;

end.

