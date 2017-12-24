
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ImageList;

{$MODE Delphi}

interface
uses

  LCLIntf, LCLType,
  Controls, Graphics,
  Classes, SysUtils, GR32;

type


  TGRPictureItem = class(TCollectionItem)
  private
    FName: string;
    FPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TGRPictureItemClass = class of TGRPictureItem;

  { Summary A collection of TGRGraphicItem objects }
  TGRPictureCollection = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TGRPictureItem;
    procedure SetItem(Index: Integer; Value: TGRPictureItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TGRPictureItemClass);
    function Add: TGRPictureItem;
    function Find(const aName: string): TGRPictureItem;
    property Items[Index: Integer]: TGRPictureItem read GetItem write SetItem; default;
  end;
  
  { Summary A component that stores TGRGraphicCollection }
  TGRPictureList = class(TComponent)
  private
    FPictureCollection: TGRPictureCollection;
    function GetPicture(Index: Integer): TPicture;
    procedure SetPicture(Index: Integer; Value: TPicture);
    procedure SetPictures(const Value: TGRPictureCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Picture[Index: Integer]: TPicture read GetPicture write SetPicture;
      default;
  published
    property Pictures: TGRPictureCollection read FPictureCollection write SetPictures;
  end;
  

implementation

uses TypInfo, GR32_System;

type
  TBitmap32Access = class(TBitmap32);

constructor TGRPictureItem.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TPicture.Create;
end;

destructor TGRPictureItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TGRPictureItem.Assign(Source: TPersistent);
begin
  if Source is TGRPictureItem then
  begin
    Name := TGRPictureItem(Source).Name;
    Picture := TGRPictureItem(Source).Picture;
  end
  else
    inherited;
end;

function TGRPictureItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TGRPictureItem.SetPicture(const Value: TPicture);
begin
  if FPicture <> Value then
    FPicture.Assign(Value);
end;

constructor TGRPictureCollection.Create(AOwner: TPersistent; ItemClass: TGRPictureItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TGRPictureCollection.Add: TGRPictureItem;
begin
  Result := TGRPictureItem(inherited Add);
end;

function TGRPictureCollection.Find(const aName: string): TGRPictureItem;
var
  I: Integer;
begin
  Result := nil;
  
  For i := 0 to Count -1 do
  begin
    if Items[i].Name = aName then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TGRPictureCollection.GetItem(Index: Integer): TGRPictureItem;
begin
  Result := TGRPictureItem(inherited GetItem(Index));
end;

function TGRPictureCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGRPictureCollection.SetItem(Index: Integer; Value: TGRPictureItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TGRPictureList.Create(AOwner: TComponent);
begin
  inherited;
  FPictureCollection := TGRPictureCollection.Create(Self, TGRPictureItem);
end;

destructor TGRPictureList.Destroy;
begin
  FPictureCollection.Free;
  inherited;
end;

function TGRPictureList.GetPicture(Index: Integer): TPicture;
begin
  Result := FPictureCollection.Items[Index].Picture;
end;

procedure TGRPictureList.SetPicture(Index: Integer; Value: TPicture);
begin
  FPictureCollection.Items[Index].Picture := Value;
end;

procedure TGRPictureList.SetPictures(const Value: TGRPictureCollection);
begin
  if FPictureCollection <> Value then
    FPictureCollection.Assign(Value);
end;


end.
