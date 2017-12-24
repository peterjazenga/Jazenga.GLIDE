(******************************************************************************
 * ZedDBTreeView                                                              *
 *                                                                            *
 * Author: Momchil Rangelov                                                   *
 *                                                                            *
 * You can report bugs at ms_rangelov@hotmail.com                             *
 *                                                                            *
 * This source is free software; you can redistribute it and/or modify it     *
 * under the terms of the GNU General Public License as published by the      *
 * Free Software Foundation at <http://www.gnu.org/copyleft/gpl.html>         *
 *                                                                            *
 * This code is distributed in the hope that it will be useful, but           *
 * WITHOUT ANY WARRANTY; without even the implied warranty of                 *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       *
 *                                                                            *
 * Abstract:                                                                  *
 * TZedDBTreeView is Data Aware TreeView. It is designed to load              *
 * self-referenced hierarchical data sets. It is fast and loads all the data  *
 * when it is built. There's no need to rebuild the tree for every            *
 * modification. Data's nature independent, so you can connect it to database *
 * on your choice. Supports browsing and modifications both ways              *
 * (Tree -> Dataset, dataset -> Tree).                                        *
 ******************************************************************************)
 
{**********************************************************************
 Package pl_ExDatabase.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplDBTreeviewUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Dialogs, Controls,
  DB, DBGrids, DBCtrls,
  TplDBTreeviewBaseUnit;

type
  TplDBTreeState = set of (tsSelecting, tsDeleting, tsBrowse, tsMovingNode,
    tsChangingNode, tsRefreshNodes, tsChangingParent,
    tsDestroying, tsInternalAdd);

type

  TplDBTreeView = class(TplDBTreeViewBase)

  private
    FIDField: string;
    FTextDataLink: TFieldDataLink;
    FParentDataLink: TFieldDataLink;
    FUpdating: boolean;
    FDeleting: boolean;
    FNodes: TList;
    FState: TplDBTreeState;
    FPreviousDataSetState: TDataSetState;
    FPrevSelectedID: integer;
    FHasBeenDrawn: boolean;
    FGridSelectedRows: TBookmarkList;
    function GetDataSource: TDataSource;
    function GetTextField: string;
    function GetParentField: string;
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetGridSelectedRows(const AValue: TBookmarkList);
    procedure SetIDField(const AValue: string);
    procedure SetTextField(const AValue: string);
    procedure SetParentField(const AValue: string);
    procedure BuildTree;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure ParentDataChange(Sender: TObject);
    procedure DeleteFromDB(Node: TplDBTreeNode);
    procedure ActiveChange(Sender: TObject);
    procedure RefreshNodes;
    function HasToProceedTree: boolean;

  protected
    function GetParentIndex(aParentID: integer): integer;
    function DataSetAvailable: boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Added(Node: TplDBTreeNode); override;
    procedure Edit(aText: string); override;
    procedure Change(Node: TplDBTreeNode); override;
    procedure Delete(Node: TplDBTreeNode); override;
    procedure Moved(aDestNode, aSourceNode: TplDBTreeNode); override;
    procedure StartMoving(aDestNode, aSourceNode: TplDBTreeNode); override;
    procedure NodeAfterPost;
    procedure CheckDelete;
    procedure CheckAppend;
    procedure SetParent(AParent: TWinControl); override;
    procedure DoPaint; override;
    procedure SetDestroyingState; override;
    procedure ClearDestroyingState; override;
    procedure SetInternalAdd; override;
    procedure ClearInternalAdd; override;
    procedure BeginReload;
    procedure EndReload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeByID(ID: integer): TplDBTreeNode;
    procedure SelectNodeByID(ID: integer);
    procedure Rebuild;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RemoveBookmark(aNode: TplDBTreeNode);
    procedure SetBookmark(aNode: TplDBTreeNode);
    property GridSelectedRows: TBookmarkList read FGridSelectedRows write SetGridSelectedRows;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DBKeyField: string read FIDField write SetIDField;
    property DBTextField: string read GetTextField write SetTextField;
    property DBParentField: string read GetParentField write SetParentField;
  end;


implementation


//==================== TplDBTreeView ============================================

constructor TplDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HideSelection := False;
  FHasBeenDrawn := False;
  FTextDataLink := TFieldDataLink.Create;
  FTextDataLink.Control := Self;
  FTextDataLink.OnDataChange := @DataChange;
  FTextDataLink.OnEditingChange := @EditingChange;
  FTextDataLink.OnUpdateData := @UpdateData;
  FTextDataLink.OnActiveChange := @ActiveChange;
  FParentDataLink := TFieldDataLink.Create;
  FParentDataLink.Control := Self;
  FParentDataLink.OnDataChange := @ParentDataChange;
  FState := [tsBrowse];
end;

destructor TplDBTreeView.Destroy;
begin
  DataSource := nil;
  FTextDataLink.Free;
  FTextDataLink := nil;
  FParentDataLink.Free;
  FParentDataLink := nil;
  inherited Destroy;
end;

procedure TplDBTreeView.BuildTree;
var
  ParentIndex, i: integer;
  Node, NewNode: TplDBTreeNode;
begin
  if HasToProceedTree then
  begin
    Self.BeginUpdate;
    try
      Self.Items.Clear;
      FTextDataLink.DataSet.Close;
      FTextDataLink.DataSet.Open;
      FTextDataLink.DataSet.First;
      FNodes := TList.Create;
      for i := 1 to FTextDataLink.DataSet.RecordCount do
      begin
        if (FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).IsNull) then
        begin
          NewNode := Self.Items.AddNewChild(nil, FTextDataLink.Field.AsString);
          NewNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
          NewNode.ParentID := FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger;
        end
        else
        begin
          ParentIndex := GetParentIndex(FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).AsInteger);
          if ParentIndex > -1 then
          begin
            Node := Self.Items[ParentIndex];
            NewNode := Self.Items.AddNewChild(Node, FTextDataLink.Field.AsString);
            NewNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
            NewNode.ParentID := FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger;
          end
          else
          begin
            NewNode := Self.Items.AddNewChild(nil, FTextDataLink.Field.AsString);
            NewNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
            NewNode.ParentID := FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger;
            FNodes.Add(NewNode);
          end;
        end;
        FTextDataLink.DataSet.Next;
      end;
      RefreshNodes;
      FNodes.Free;
    finally
      FTextDataLink.DataSet.First;
      Self.EndUpdate;
    end;
  end;
end;

procedure TplDBTreeView.DataChange(Sender: TObject);
begin
  if (DatasetAvailable) and (not Self.FUpdating) then
  begin
    CheckDelete;
    NodeAfterPost;
    CheckAppend;
    if (Self.FIDField <> EmptyStr) and (not FDeleting) and (not (tsMovingNode in FState)) then
      SelectNodeByID(Self.FTextDataLink.DataSet.FieldByName(Self.FIDField).AsInteger);
    if (Self.FTextDataLink.BOF) and (not (csDesigning in self.ComponentState)) then
      BuildTree;
  end;
end;

function TplDBTreeView.GetDataSource: TDataSource;
begin
  Result := FTextDataLink.DataSource;
end;

function TplDBTreeView.GetParentField: string;
begin
  Result := FParentDataLink.FieldName;
end;

function TplDBTreeView.GetTextField: string;
begin
  Result := FTextDataLink.FieldName;
end;

procedure TplDBTreeView.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FTextDataLink.Datasource then
    Exit;
  if not (FTextDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    FTextDataLink.DataSource := AValue;
    FParentDataLink.DataSource := AValue;
  end;
  BuildTree;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TplDBTreeView.SetGridSelectedRows(const AValue: TBookmarkList);
begin
  if FGridSelectedRows = AValue then
    exit;
  FGridSelectedRows := AValue;
end;

procedure TplDBTreeView.SetIDField(const AValue: string);
begin
  if FIDField <> AValue then
    FIDField := AValue;
  BuildTree;
end;

procedure TplDBTreeView.SetParentField(const AValue: string);
begin
  if AValue <> FParentDataLink.FieldName then
    FParentDataLink.FieldName := AValue;
  BuildTree;
end;

procedure TplDBTreeView.SetTextField(const AValue: string);
begin
  if AValue <> FTextDataLink.FieldName then
    FTextDataLink.FieldName := AValue;
  BuildTree;
end;

procedure TplDBTreeView.RefreshNodes;
var
  i: integer;
  ParentNode: TplDBTreeNode;
begin
  for i := 0 to FNodes.Count - 1 do
  begin
    ParentNode := GetNodeByID(TplDBTreeNode(FNodes[i]).ParentID);
    if Assigned(ParentNode) and (ParentNode <> TplDBTreeNode(FNodes[i])) then
    begin
      FState := FState + [tsRefreshNodes];
      TplDBTreeNode(FNodes[i]).MoveTo(ParentNode, naAddChild);
      FState := FState - [tsRefreshNodes];
    end;
  end;
end;

procedure TplDBTreeView.EditingChange(Sender: TObject);
begin
  if DatasetAvailable and (FTextDataLink.DataSet.State in [dsBrowse]) and (not Self.FUpdating) then
  begin
    if FTextDataLink.Field.IsNull and (FPreviousDataSetState in [dsEdit, dsInsert]) then
      FPreviousDataSetState := dsBrowse;
  end;
end;

procedure TplDBTreeView.UpdateData(Sender: TObject);
begin
  Self.Selected.Text := Self.FTextDataLink.Field.AsString;
end;

procedure TplDBTreeView.ParentDataChange(Sender: TObject);
var
  Node, ParentNode: TplDBTreeNode;
  ParentID: integer;
begin
  if DatasetAvailable and not (tsMovingNode in FState) then
    if Assigned(FParentDataLink.DataSet) and (FParentDataLink.DataSet.Modified) and (not Self.FUpdating) then
    begin
      if not FParentDataLink.Field.IsNull then
      begin
        Node := GetNodeByID(FTextDataLink.DataSet.FieldByName(FIDField).AsInteger);
        if Assigned(Node) then
        begin
          ParentNode := GetNodeByID(FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger);
          ParentID := -1;
          if Assigned(ParentNode) then
            ParentID := ParentNode.ID;
          if Node.ParentID <> ParentID then
            if Assigned(ParentNode) then
              Node.MoveTo(ParentNode, naAddChild)
            else
              Node.MoveTo(nil, naAdd);
        end;
      end;
    end;
end;

procedure TplDBTreeView.DeleteFromDB(Node: TplDBTreeNode);
begin
  if DatasetAvailable and Node.Deleting and (not Node.DeletedFromDB) then
  begin
    if FTextDataLink.DataSet.Locate(FIDField, Node.ID, []) then
      FTextDataLink.DataSet.Delete;
  end;
end;

procedure TplDBTreeView.ActiveChange(Sender: TObject);
begin
  if DataSetAvailable then
    BuildTree
  else
    Self.Items.Clear;
end;

function TplDBTreeView.DataSetAvailable: boolean;
begin
  Result := Assigned(Self.FTextDataLink.DataSet) and Assigned(Self.FParentDataLink.DataSet) and
    Self.FTextDataLink.DataSet.Active and (FIDField <> EmptyStr);
end;

procedure TplDBTreeView.SetParent(AParent: TWinControl);
begin
  FState := FState + [tsChangingParent];
  inherited SetParent(AParent);
  FState := FState - [tsChangingParent];
end;

function TplDBTreeView.GetParentIndex(aParentID: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items[i].ID = aParentID then
    begin
      Result := i;
      Break;
    end;
end;

procedure TplDBTreeView.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TplDBTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FTextDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TplDBTreeView.Added(Node: TplDBTreeNode);
begin
  if DatasetAvailable and (not Self.FUpdating) and (not (tsChangingParent in FState)) and (not (tsInternalAdd in FState)) then
  begin
    Self.BeginUpdate;
    try
      Self.FTextDataLink.DataSet.Insert;
      Self.FTextDataLink.Field.AsString := Node.Text;
      if Assigned(Node.Parent) then
      begin
        Self.FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).AsInteger := Node.Parent.ID;
        Node.ParentID := Node.Parent.ID;
      end
      else
      begin
        Self.FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).AsInteger := -1;
        Node.ParentID := -1;
      end;
      Self.FTextDataLink.DataSet.Post;
      Node.ID := Self.FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
      Self.Selected := Node;
    finally
      Self.EndUpdate;
    end;
  end;
  inherited Added(Node);
end;

procedure TplDBTreeView.Change(Node: TplDBTreeNode);
begin
  if DatasetAvailable and (not (tsSelecting in FState)) and (not (tsMovingNode in FState)) then
  begin
    if Assigned(Node) then
    begin
      FState := FState + [tsChangingNode];
      if (FTextDataLink.DataSet.RecordCount > 0) and FTextDataLink.DataSet.Locate(FIDField, Node.ID, []) then
      begin
        if self.RowSelect then
          Node.DBBookmark := FTextDataLink.DataSet.GetBookmark;
        if Assigned(GridSelectedRows) then
          GridSelectedRows.CurrentRowSelected := True;
      end;
      FState := FState - [tsChangingNode];
    end;
  end;
  inherited Change(Node);
end;

procedure TplDBTreeView.Delete(Node: TplDBTreeNode);
begin
  FDeleting := Node.Deleting;
  if not ((csDestroying in Node.TreeView.ComponentState) or (csDesigning in Node.TreeView.ComponentState) or
    (tsDestroying in FState) or Self.FUpdating or (tsChangingParent in FState)) then
  begin
    DeleteFromDB(Node);
  end;
  FDeleting := False;
  inherited Delete(Node);
end;

procedure TplDBTreeView.Moved(aDestNode, aSourceNode: TplDBTreeNode);
begin
  if DatasetAvailable and (not (tsRefreshNodes in FState)) then
  begin
    if (not Assigned(aDestNode)) or (not aDestNode.HasAsParent(aSourceNode)) then
    begin
      Self.FParentDataLink.DataSet.Edit;
      if Assigned(aSourceNode.Parent) then
      begin
        Self.FParentDataLink.Field.AsInteger := aSourceNode.Parent.ID;
        aSourceNode.ParentID := aSourceNode.Parent.ID;
      end
      else
      begin
        Self.FParentDataLink.Field.AsVariant := -1;
        aSourceNode.ParentID := -1;
      end;
    end
    else
      Self.FParentDataLink.DataSet.Cancel;
  end;
  FState := FState - [tsMovingNode];
  inherited Moved(aDestNode, aSourceNode);
  if not (tsRefreshNodes in FState) then
    Self.EndUpdate;
end;

procedure TplDBTreeView.StartMoving(aDestNode, aSourceNode: TplDBTreeNode);
begin
  FState := FState + [tsMovingNode];
  if not (tsRefreshNodes in FState) then
    Self.BeginUpdate;
  inherited StartMoving(aDestNode, aSourceNode);
end;

procedure TplDBTreeView.CheckDelete;
var
  Node: TplDBTreeNode;
begin
  if (DatasetAvailable) and ((Self.Items.Count - FTextDataLink.DataSet.RecordCount) = 1) and (not FDeleting) then
  begin
    Node := GetNodeByID(FPrevSelectedID);
    if Assigned(Node) then
    begin
      Node.DeletedFromDB := True;
      Node.Delete;
    end;
  end;
  if DatasetAvailable and not (tsMovingNode in FState) then
    FPrevSelectedID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
end;

procedure TplDBTreeView.NodeAfterPost;
var
  Node: TplDBTreeNode;
begin
  if DatasetAvailable then
  begin
    if (FTextDataLink.DataSet.State in [dsBrowse]) and (not Self.FUpdating) and (not (tsMovingNode in FState)) then
    begin
      if (FPreviousDataSetState in [dsEdit]) and not FTextDataLink.Field.IsNull then
      begin
        Self.Selected.Text := FTextDataLink.Field.AsString;
      end
      else
      if (FPreviousDataSetState in [dsInsert]) and not FTextDataLink.Field.IsNull then
      begin
        Self.BeginUpdate;
        try
          Node := Self.Items.AddNewChild(GetNodeByID(FParentDataLink.Field.AsInteger), FTextDataLink.Field.AsString);
          Node.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
          Node.ParentID := FParentDataLink.Field.AsInteger;
        finally
          Self.EndUpdate;
        end;
      end;
    end;
    FPreviousDataSetState := FTextDataLink.DataSet.State;
  end;
end;

procedure TplDBTreeView.DoPaint;
begin
  FHasBeenDrawn := True;
  inherited DoPaint;
end;

procedure TplDBTreeView.SetDestroyingState;
begin
  Include(FState, tsDestroying);
end;

procedure TplDBTreeView.ClearDestroyingState;
begin
  Exclude(FState, tsDestroying);
end;

procedure TplDBTreeView.SetInternalAdd;
begin
  Include(FState, tsInternalAdd);
end;

procedure TplDBTreeView.ClearInternalAdd;
begin
  Exclude(FState, tsInternalAdd);
end;

procedure TplDBTreeView.BeginReload;
begin
  if Self.FHasBeenDrawn then
    FTextDataLink.DataSet.DisableControls;
  if Self.FHasBeenDrawn then
    FParentDataLink.DataSet.DisableControls;
  Self.Items.BeginUpdate;
end;

procedure TplDBTreeView.EndReload;
begin
  if FTextDataLink.DataSet.ControlsDisabled then
    FTextDataLink.DataSet.EnableControls;
  if FParentDataLink.DataSet.ControlsDisabled then
    FParentDataLink.DataSet.EnableControls;
  Self.Items.EndUpdate;
end;

function TplDBTreeView.HasToProceedTree: boolean;
begin
  Result := True;
  if FTextDataLink.DataSource = nil then
    Result := False;
  if FParentDataLink.DataSource = nil then
    Result := False;
  if FParentDataLink.FieldName = EmptyStr then
    Result := False;
  if FIDField = EmptyStr then
    Result := False;
  if FTextDataLink.FieldName = EmptyStr then
    Result := False;

  if Self.FUpdating then
    Result := False;
  if not DatasetAvailable then
    Result := False;
end;

function TplDBTreeView.GetNodeByID(ID: integer): TplDBTreeNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if Items[i].ID = ID then
    begin
      Result := Items[i];
      Break;
    end;
end;

procedure TplDBTreeView.SelectNodeByID(ID: integer);
var
  Index: integer;
begin
  if (not (tsMovingNode in FState)) and (not (tsChangingNode in FState)) then
  begin
    Index := Self.Items.IndexOf(ID);
    if Index > -1 then
    begin
      FState := FState + [tsSelecting];
      Self.Items[Index].Selected := True;
      FState := FState - [tsSelecting];
    end;
  end;
end;

procedure TplDBTreeView.Rebuild;
begin
  Self.BeginReload;
  BuildTree;
  Self.EndReload;
end;

procedure TplDBTreeView.BeginUpdate;
begin
  if Self.FHasBeenDrawn then
    FTextDataLink.DataSet.DisableControls;
  if Self.FHasBeenDrawn then
    FParentDataLink.DataSet.DisableControls;
  Self.Items.BeginUpdate;
  FUpdating := True;
end;

procedure TplDBTreeView.EndUpdate;
begin
  if FTextDataLink.DataSet.ControlsDisabled then
    FTextDataLink.DataSet.EnableControls;
  if FParentDataLink.DataSet.ControlsDisabled then
    FParentDataLink.DataSet.EnableControls;
  Self.Items.EndUpdate;
  FUpdating := False;
end;

procedure TplDBTreeView.Edit(aText: string);
var
  tmpStr: string;
begin
  if DataSetAvailable then
  begin
    Self.FTextDataLink.DataSet.Edit;
    Self.FTextDataLink.Field.AsString := aText;
    if Self.FTextDataLink.DataSet.State in [dsEdit] then
      Self.FTextDataLink.DataSet.Post;
  end;
end;

procedure TplDBTreeView.RemoveBookmark(aNode: TplDBTreeNode);
begin
  if DatasetAvailable then
  begin
    FTextDataLink.DataSet.FreeBookmark(aNode.DBBookmark);
    aNode.DBBookmark := nil;
  end;
end;

procedure TplDBTreeView.SetBookmark(aNode: TplDBTreeNode);
begin
  if DatasetAvailable then
  begin
    aNode.DBBookmark := FTextDataLink.DataSet.GetBookmark;
  end;
end;
{
procedure TplDBTreeView.CheckAppend;
var
  Node: TplDBTreeNode;
begin
  if (DatasetAvailable) and ((FTextDataLink.DataSet.RecordCount - Self.Items.Count) = 1) then
  begin
    Self.BeginUpdate;
    try
      Node := Self.Items.AddNewChild(GetNodeByID(FParentDataLink.Field.AsInteger), FTextDataLink.Field.AsString);
      Node.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
      Node.ParentID := FParentDataLink.Field.AsInteger;
    finally
      Self.EndUpdate;
    end;
  end;
end;
}


procedure TplDBTreeView.CheckAppend;
var
  xNode, xParentNode: TplDBTreeNode;
begin
  if (DatasetAvailable) and ((FTextDataLink.DataSet.RecordCount - Self.Items.Count) = 1) then
    begin
      Self.BeginUpdate;
      try

        xParentNode := nil;
        if Assigned(FParentDataLink.Field) then
        begin
              xParentNode := GetNodeByID(FParentDataLink.Field.AsInteger);
        end;

        xNode := Self.Items.AddNewChild(xParentNode, FTextDataLink.Field.AsString);
        xNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;

        xNode.ParentID := -1;
        if Assigned(FParentDataLink.Field) then
        begin
              xNode.ParentID := FParentDataLink.Field.AsInteger;
        end;
      finally
        Self.EndUpdate;
      end;
    end;
end;   


end.
