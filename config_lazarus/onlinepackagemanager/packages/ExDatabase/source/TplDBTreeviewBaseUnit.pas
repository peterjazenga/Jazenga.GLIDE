
{**********************************************************************
 Package pl_ExDatabase.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplDBTreeviewBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, StdCtrls, DB;

type


  TplDBTreeNode = class(TTreeNode)
  private
    FID: integer;
    FParentID: integer;
    FDeletedFromDB: boolean;
    FDBBookmark: TBookmark;
    function GetParent: TplDBTreeNode;
    procedure SetDBBookmark(const AValue: TBookmark);
    procedure SetID(const AValue: integer);
    procedure SetParentID(const AValue: integer);

  public
    destructor Destroy; override;
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); override;
    property Parent: TplDBTreeNode read GetParent;
    property ID: integer read FID write SetID;
    property ParentID: integer read FParentID write SetParentID;
    property DeletedFromDB: boolean read FDeletedFromDB write FDeletedFromDB;
    property DBBookmark: TBookmark read FDBBookmark write SetDBBookmark;
  end;


  TplDBTreeNodes = class(TTreeNodes)
  private
    function GetNodeFromIndex(Index: integer): TplDBTreeNode;
  public
    function AddNewChild(ParentNode: TplDBTreeNode; const S: string): TplDBTreeNode;
    function IndexOf(anID: integer): integer;
    property Item[Index: integer]: TplDBTreeNode read GetNodeFromIndex; default;
  end;


  TplDBTreeViewBase = class(TTreeView)
  private
    FSelecting, FSelectChanged: boolean;
    procedure SeTplDBTreeNodes(const AValue: TplDBTreeNodes);
    function GeTplDBTreeNodes: TplDBTreeNodes;
    function GetSelection: TplDBTreeNode;
    procedure SetSelection(const AValue: TplDBTreeNode);
  protected
    procedure Change(Node: TTreeNode); override;
    procedure Change(Node: TplDBTreeNode); dynamic;
    procedure Delete(Node: TTreeNode); override;
    procedure Delete(Node: TplDBTreeNode); dynamic;
    procedure EditorEditingDone(Sender: TObject); override;
    procedure Edit(aText: string); dynamic;
    procedure Added(Node: TplDBTreeNode); dynamic;
    procedure Moved(aDestNode, aSourceNode: TplDBTreeNode); dynamic;
    procedure StartMoving(aDestNode, aSourceNode: TplDBTreeNode); dynamic;
    procedure SetDestroyingState; dynamic;
    procedure ClearDestroyingState; dynamic;
    procedure SetInternalAdd; dynamic;
    procedure ClearInternalAdd; dynamic;
    function CreateNode: TplDBTreeNode; override;
  public
    property Items: TplDBTreeNodes read GeTplDBTreeNodes write SeTplDBTreeNodes;
    constructor Create(AnOwner: TComponent); override;
    property Selected: TplDBTreeNode read GetSelection write SetSelection;
  end;

implementation


//===================== TplDBTreeNode =======================================

destructor TplDBTreeNode.Destroy;
begin
  inherited Destroy;
end;

function TplDBTreeNode.GetParent: TplDBTreeNode;
begin
  Result := TplDBTreeNode(inherited Parent);
end;

procedure TplDBTreeNode.SetParentID(const AValue: integer);
begin
  if FParentID = AValue then
    exit;
  FParentID := AValue;
end;

procedure TplDBTreeNode.SetID(const AValue: integer);
begin
  if FID = AValue then
    exit;
  FID := AValue;
end;

procedure TplDBTreeNode.SetDBBookmark(const AValue: TBookmark);
begin
  if FDBBookmark = AValue then
    exit;
  FDBBookmark := AValue;
end;

procedure TplDBTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
begin
  if not Deleting then
  begin
    (TreeView as TplDBTreeViewBase).StartMoving(Destination as TplDBTreeNode, self);
    inherited MoveTo(Destination, Mode);
    (TreeView as TplDBTreeViewBase).Moved(Destination as TplDBTreeNode, self);
  end;
end;

//============ TplDBTreeViewBase ===================================

function TplDBTreeViewBase.GetSelection: TplDBTreeNode;
begin
  Result := TplDBTreeNode(inherited Selected);
end;

procedure TplDBTreeViewBase.SetSelection(const AValue: TplDBTreeNode);
begin
  inherited Selected := AValue;
end;

procedure TplDBTreeViewBase.Change(Node: TTreeNode);
begin
  Change(TplDBTreeNode(Node));
  inherited Change(Node);
end;

procedure TplDBTreeViewBase.Change(Node: TplDBTreeNode);
begin
  FSelectChanged := True;
end;

procedure TplDBTreeViewBase.Delete(Node: TTreeNode);
begin
  Delete(TplDBTreeNode(Node));
  inherited Delete(Node);
end;

procedure TplDBTreeViewBase.EditorEditingDone(Sender: TObject);
var
  aNewText: string;
  hasToProceed: boolean;
begin
  hasToProceed := False;
  if Assigned(FEditor) then
  begin
    aNewText := FEditor.Text;
    hasToProceed := True;
  end;
  inherited EditorEditingDone(Sender);
  if hasToProceed then
    Edit(aNewText);
end;

function TplDBTreeViewBase.CreateNode: TplDBTreeNode;
begin
  Result := nil;
  if Assigned(OnCustomCreateItem) then
    OnCustomCreateItem(Self, Result);
  if Result = nil then
    Result := TplDBTreeNode.Create(Items);

  Result.DeletedFromDB := False;
end;

procedure TplDBTreeViewBase.SeTplDBTreeNodes(const AValue: TplDBTreeNodes);
begin
  inherited Items := AValue;
end;

function TplDBTreeViewBase.GeTplDBTreeNodes: TplDBTreeNodes;
begin
  Result := TplDBTreeNodes(inherited Items);
end;

constructor TplDBTreeViewBase.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  inherited Items := TplDBTreeNodes.Create(self);
end;

procedure TplDBTreeViewBase.Delete(Node: TplDBTreeNode);
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.Edit(aText: string);
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.Added(Node: TplDBTreeNode);
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.Moved(aDestNode, aSourceNode: TplDBTreeNode);
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.StartMoving(aDestNode, aSourceNode: TplDBTreeNode);
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.SetDestroyingState;
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.ClearDestroyingState;
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.SetInternalAdd;
begin
  // nothing yet
end;

procedure TplDBTreeViewBase.ClearInternalAdd;
begin
  // nothing yet
end;

//=================== TplDBTreeNodes ====================================

function TplDBTreeNodes.AddNewChild(ParentNode: TplDBTreeNode; const S: string): TplDBTreeNode;
begin
  Result := TplDBTreeNode(inherited AddChild(ParentNode, S));
  TplDBTreeViewBase(Result.TreeView).Added(Result);
end;

function TplDBTreeNodes.GetNodeFromIndex(Index: integer): TplDBTreeNode;
begin
  Result := TplDBTreeNode(inherited Item[Index]);
end;

function TplDBTreeNodes.IndexOf(anID: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Item[i].ID = anID then
    begin
      Result := i;
      Break;
    end;
end;

end.
