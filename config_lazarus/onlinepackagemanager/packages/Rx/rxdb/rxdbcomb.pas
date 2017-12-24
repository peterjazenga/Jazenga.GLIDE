{ rxdbcomb unit

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

unit rxdbcomb;

{$I rx.inc}

interface

uses LCLType, LCLProc, LCLIntf, LMessages, Menus, Graphics, Classes, Controls,
  sysutils, DB, StdCtrls, DbCtrls;

type

{ TCustomDBComboBox }

  TCustomDBComboBox = class(TCustomComboBox)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetItems(const Value: TStrings);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    function GetComboText: string; virtual;
    procedure SetComboText(const Value: string); virtual;
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure EditingDone; override;
    procedure Change; override;
    procedure Click; override;
    procedure CreateWnd; override;
    procedure DropDown; override;
    function GetPaintText: string; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure WndProc(var Message: TLMessage); override;
    property ComboText: string read GetComboText write SetComboText;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean;
    property Field: TField read GetField;
    property Items write SetItems;
    property Text;
  end;

{ TRxDBComboBox }

  TRxDBComboBox = class(TCustomDBComboBox)
  private
    FValues: TStrings;
    FEnableValues: Boolean;
    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
  protected
    procedure SetStyle(Value: TComboBoxStyle); override;
    function GetComboText: string; override;
    function GetPaintText: string; override;
    procedure SetComboText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoSize;
    property BorderSpacing;
    property Style; { must be published before Items }
    property Color;
    property DataField;
    property DataSource;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property EnableValues: Boolean read FEnableValues write SetEnableValues;
    property Font;
    property Constraints;
    property DragKind;
    property ItemHeight;
    property Items;
    property ItemWidth;
    property MaxLength default -1;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Visible;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses rxDBUtils, rxdconst;

{ TCustomDBComboBox }

constructor TCustomDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnEditingChange := @EditingChange;
end;

destructor TCustomDBComboBox.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TCustomDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TCustomDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TCustomDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

procedure TCustomDBComboBox.DataChange(Sender: TObject);
begin
  if DroppedDown then Exit;
  if FDataLink.Field <> nil then ComboText := FDataLink.Field.AsString
  else if csDesigning in ComponentState then ComboText := Name
  else ComboText := '';
end;

procedure TCustomDBComboBox.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    FDataLink.Field.AsString := ComboText
  else
    raise Exception.CreateFmt(SDBComboBoxFieldNotAssigned, [Name]);
end;

procedure TCustomDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> ComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
//      if Redraw then SendMessage(Handle, LM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1 else I := Items.IndexOf(Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
//          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

procedure TCustomDBComboBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

function TCustomDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text
  else
  begin
    I := ItemIndex;
    if I < 0 then Result := '' else Result := Items[I];
  end;
end;

procedure TCustomDBComboBox.Change;
begin
  FDataLink.Edit;
  FDataLink.Modified;
  FDataLink.UpdateRecord;
  inherited Change;
end;

procedure TCustomDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

procedure TCustomDBComboBox.DropDown;
begin
  FDataLink.Edit;
  inherited DropDown;
end;

function TCustomDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TCustomDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TCustomDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCustomDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_BACK) or (Key = VK_DELETE) or (Key = VK_UP) or
     (Key = VK_DOWN) or (Key in [32..255]) then
  begin
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

procedure TCustomDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
//    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TCustomDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TCustomDBComboBox.SetEditReadOnly;
begin
(*  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage({$IFDEF WIN32} EditHandle {$ELSE} FEditHandle {$ENDIF},
      EM_SETREADONLY, Ord(not FDataLink.Editing), 0); *)
end;


procedure TCustomDBComboBox.WndProc(var Message: TLMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      LM_COMMAND:
        if TLMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then begin
{            if Style <> csSimple then
              PostMessage(Handle, LB_SHOWDROPDOWN, 0, 0);}
            Exit;
          end;
{      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then FDataLink.Edit
        else if not FDataLink.Editing then DataChange(Self); }{Restore text}
{$IFDEF WIN32}
{      LM_CREATE,
      WM_WINDOWPOSCHANGED,
      CM_FONTCHANGED:
        FPaintControl.DestroyHandle;}
{$ENDIF}
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBComboBox.EditingDone;
begin
  if Assigned(FDataLink.DataSet) and (FDataLink.DataSet.State in [dsinsert,dsedit]) then
  begin
    try
      FDataLink.UpdateRecord;
    except
      SelectAll;
      if CanFocus then SetFocus;
      raise;
    end;
    inherited EditingDone;
  end;
end;


function TCustomDBComboBox.GetPaintText: string;
begin
  if FDataLink.Field <> nil then Result := FDataLink.Field.Text
  else Result := '';
end;

procedure TCustomDBComboBox.SetItems(const Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TCustomDBComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if (Value = csSimple) and Assigned(FDatalink) and FDatalink.DatasourceFixed then
    _DBError('SNotReplicatable');
  inherited SetStyle(Value);
end;

function TCustomDBComboBox.UseRightToLeftAlignment: Boolean;
begin
//  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TCustomDBComboBox.ExecuteAction(AAction: TBasicAction): Boolean;
begin
{  Result := inherited ExecuteAction(AAction) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(AAction);}
end;

function TCustomDBComboBox.UpdateAction(AAction: TBasicAction): Boolean;
begin
{  Result := inherited UpdateAction(AAction) or (FDataLink <> nil) and
    FDataLink.UpdateAction(AAction);}
end;

{ TRxDBComboBox }

constructor TRxDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValues := TStringList.Create;
  TStringList(FValues).OnChange := @ValuesChanged;
  EnableValues := False;
end;

destructor TRxDBComboBox.Destroy;
begin
  TStringList(FValues).OnChange := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TRxDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

function TRxDBComboBox.GetPaintText: string;
var
  I: Integer;
begin
  Result := '';
  if FDataLink.Field <> nil then begin
    if FEnableValues then begin
      I := Values.IndexOf(FDataLink.Field.Text);
      if I >= 0 then Result := Items.Strings[I]
    end
    else Result := FDataLink.Field.Text;
  end;
end;

function TRxDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if (Style in [csDropDown, csSimple]) and (not FEnableValues) then
    Result := Text
  else begin
    I := ItemIndex;
    if (I < 0) or (FEnableValues and (FValues.Count < I + 1)) then
      Result := ''
    else
      if FEnableValues then Result := FValues[I]
      else Result := Items[I];
  end;
end;

procedure TRxDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> ComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
//      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1
        else
          if FEnableValues then I := Values.IndexOf(Value)
          else I := Items.IndexOf(Value);
        if I >= Items.Count then I := -1;
        ItemIndex := I;
      finally
        if Redraw then
        begin
//          SendMessage(Handle, WM_SETREDRAW, 1, 0);
//          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
    Invalidate;
  end;
end;

procedure TRxDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
  begin
    if Value and (Style in [csDropDown, csSimple]) then
      Style := csDropDownList;
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TRxDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

procedure TRxDBComboBox.SetStyle(Value: TComboboxStyle);
begin
  if (Value in [csSimple, csDropDown]) and FEnableValues then
    Value := csDropDownList;
  inherited SetStyle(Value);
end;

end.
