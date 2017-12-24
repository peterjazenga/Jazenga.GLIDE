unit OWExtCollection;

{$MODE DELPHI}{$H+}

interface

uses Classes;

type
  TOWExtCollectionItem = class(TCollectionItem)
  protected
    CurrentEditorPtr: ^TCollectionItem;
    FInitialized: boolean;
  public
    destructor Destroy(); override;
  end;

  TOWExtCollection = class(TCollection)
  protected
    LastIndicatedCount: integer;
    FOwner: TPersistent;
    FNotifyList: TInterfaceList;    
    procedure AfterNewItemCreated(Item: TCollectionItem); virtual;
    procedure AfterNewItemDestroyed(Item: TCollectionItem); virtual;  
    procedure NewItemCreated(Item: TCollectionItem); virtual;
    procedure SetItemName(Item: TCollectionItem); override;
  public
    function GetOwner(): TPersistent; override;
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    destructor Destroy(); override;
  end;

type
  IOWCollectionNotifier = interface
    ['{55437AD4-9F90-42AC-9146-5F93F8A325EF}']
    procedure ItemCreated(Collection: TCollection; Item: TCollectionItem);
    procedure ItemDestroyed(Collection: TCollection; Item: TCollectionItem);

  end;

procedure OWAddCollectionNotifier(ACollection: TOWExtCollection; ANotifyItem: IOWCollectionNotifier);
procedure OWRemoveCollectionNotifier(ACollection: TOWExtCollection; ANotifyItem: IOWCollectionNotifier);

implementation


procedure OWAddCollectionNotifier(ACollection: TOWExtCollection; ANotifyItem: IOWCollectionNotifier);
begin
  if (ACollection = nil) then
    Exit;

  if (ACollection.FNotifyList = nil) then
    ACollection.FNotifyList := TInterfaceList.Create();

  ACollection.FNotifyList.Add(ANotifyItem);
end;

procedure OWRemoveCollectionNotifier(ACollection: TOWExtCollection; ANotifyItem: IOWCollectionNotifier);
begin
  if (ACollection = nil) then
    Exit;

  if (ACollection.FNotifyList = nil) then
    ACollection.FNotifyList := TInterfaceList.Create();

  ACollection.FNotifyList.Remove(ANotifyItem);
end;

destructor TOWExtCollectionItem.Destroy();
begin
  inherited;
  if (CurrentEditorPtr <> nil) then
    CurrentEditorPtr^ := nil;

end;

constructor TOWExtCollection.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

destructor TOWExtCollection.Destroy();
begin
  if (FNotifyList <> nil) then
    FNotifyList.Free();

  FNotifyList := nil;

  inherited;
end;

function TOWExtCollection.GetOwner(): TPersistent;
begin
  Result := FOwner;
end;

procedure TOWExtCollection.AfterNewItemCreated(Item: TCollectionItem);
var
  I: integer;

begin
  if (FNotifyList = nil) then
    Exit;

  for I := 0 to FNotifyList.Count - 1 do
    try
      IOWCollectionNotifier(FNotifyList.Items[I]).ItemCreated(Self, Item);

    except;
    end;

end;

procedure TOWExtCollection.AfterNewItemDestroyed(Item: TCollectionItem);
var
  I: integer;

begin
  if (FNotifyList = nil) then
    Exit;

  for I := 0 to FNotifyList.Count - 1 do
    try
      IOWCollectionNotifier(FNotifyList.Items[I]).ItemDestroyed(Self, Item);

    except;
    end;

end;

procedure TOWExtCollection.NewItemCreated(Item: TCollectionItem);
begin
end;

procedure TOWExtCollection.SetItemName(Item: TCollectionItem);
begin
  inherited SetItemName(Item);
  if (not TOWExtCollectionItem(Item).FInitialized) then
  begin
    TOWExtCollectionItem(Item).FInitialized := True;
    NewItemCreated(Item);
    AfterNewItemCreated(Item);
  end;

end;

end.

