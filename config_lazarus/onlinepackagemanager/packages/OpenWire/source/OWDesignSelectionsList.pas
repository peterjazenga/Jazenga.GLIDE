unit OWDesignSelectionsList;

interface
  uses
    LazIDEIntf, PropEdits, ComponentEditors , Classes;

{   Used to transport component selections between property editors }
type

TOWDesignerSelectionList = class(TInterfacedObject)

  private
    FList: TList;
    function Intf_Add(const Item: TPersistent): Integer;
    function Intf_Get(Index: Integer): TPersistent;
    function Get(Index: Integer): TPersistent;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TPersistent): Integer;
    function ListEquals(List: TOWDesignerSelectionList): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPersistent read Get; default;
  end;

implementation

constructor TOWDesignerSelectionList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TOWDesignerSelectionList.Destroy;
begin
  FList.Free();
  inherited Destroy;
end;

function TOWDesignerSelectionList.Get(Index: Integer): TPersistent;
begin
  Result := TPersistent(FList[Index]);
end;

function TOWDesignerSelectionList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TOWDesignerSelectionList.Add(Item: TPersistent): Integer;
begin
  Result := FList.Add(Item);
end;

function TOWDesignerSelectionList.ListEquals(List: TOWDesignerSelectionList): Boolean;
begin
  Result := False;
end;

function TOWDesignerSelectionList.Intf_Add(const Item: TPersistent): Integer;
begin
    Result := Add( Item );
end;

function TOWDesignerSelectionList.Intf_Get(Index: Integer): TPersistent;
begin
    Result := TPersistent( FList[ Index ] );
end;

end.

 
