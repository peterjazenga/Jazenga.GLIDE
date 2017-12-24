unit spkt_Items;

{$mode delphi}
{.$Define EnhancedRecordSupport}

(*******************************************************************************
*                                                                              *
*  Plik: spkt_Items.pas                                                        *
*  Opis: Modu³ zawiera klasê kolekcji elementów tafli.                         *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*******************************************************************************)

interface

uses Classes, Controls, SysUtils, Dialogs,
     spkt_Appearance, spkt_Dispatch, spkt_BaseItem, spkt_Types,
     spkt_Buttons, spkt_Checkboxes;

type TSpkItems = class(TSpkCollection)
     private
       FToolbarDispatch : TSpkBaseToolbarDispatch;
       FAppearance : TSpkToolbarAppearance;
       FImages : TImageList;
       FDisabledImages : TImageList;
       FLargeImages : TImageList;
       FDisabledLargeImages : TImageList;

     // *** Gettery i settery ***
       procedure SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);
       function GetItems(index: integer): TSpkBaseItem; reintroduce;
       procedure SetAppearance(const Value: TSpkToolbarAppearance);
       procedure SetImages(const Value: TImageList);
       procedure SetDisabledImages(const Value : TImageList);
       procedure SetLargeImages(const Value : TImageList);
       procedure SetDisabledLargeImages(const Value : TImageList);
     public
       function AddLargeButton : TSpkLargeButton;
       function AddSmallButton : TSpkSmallButton;
       function AddCheckbox: TSpkCheckbox;
       function AddRadioButton: TSpkRadioButton;

     // *** Reakcja na zmiany listy ***
       procedure Notify(Item: TComponent; Operation : TOperation); override;
       procedure Update; override;

       property Items[index : integer] : TSpkBaseItem read GetItems; default;
       property ToolbarDispatch : TSpkBaseToolbarDispatch read FToolbarDispatch write SetToolbarDispatch;
       property Appearance : TSpkToolbarAppearance read FAppearance write SetAppearance;
       property Images : TImageList read FImages write SetImages;
       property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
       property LargeImages : TImageList read FLargeImages write SetLargeImages;
       property DisabledLargeImages : TImageList read FDisabledLargeImages write SetDisabledLargeImages;
     end;

implementation

{ TSpkItems }

function TSpkItems.AddLargeButton: TSpkLargeButton;
begin
  Result := TSpkLargeButton.Create(FRootComponent);
  Result.Parent := FRootComponent;
  AddItem(Result);
end;

function TSpkItems.AddSmallButton: TSpkSmallButton;
begin
  Result := TSpkSmallButton.Create(FRootComponent);
  Result.Parent := FRootComponent;
  AddItem(Result);
end;

function TSpkItems.AddCheckbox: TSpkCheckbox;
begin
  Result := TSpkCheckbox.Create(FRootComponent);
  Result.Parent := FRootComponent;
  AddItem(Result);
end;

function TSpkItems.AddRadioButton: TSpkRadioButton;
begin
  Result := TSpkRadioButton.Create(FRootComponent);
  Result.Parent := FRootComponent;
  AddItem(Result);
end;

function TSpkItems.GetItems(index: integer): TSpkBaseItem;
begin
result:=TSpkBaseItem(inherited Items[index]);
end;

procedure TSpkItems.Notify(Item: TComponent;
  Operation : TOperation);
begin
  inherited Notify(Item, Operation);

  case Operation of
       opInsert: begin
                 // Ustawienie dyspozytora na nil spowoduje, ¿e podczas
                 // przypisywania w³asnoœci nie bêd¹ wo³ane metody Notify*
                 TSpkBaseItem(Item).ToolbarDispatch:=nil;

                 TSpkBaseItem(Item).Appearance:=FAppearance;
                 TSpkBaseItem(Item).Images:=FImages;
                 TSpkBaseItem(Item).DisabledImages:=FDisabledImages;
                 TSpkBaseItem(Item).LargeImages:=FLargeImages;
                 TSpkBaseItem(Item).DisabledLargeImages:=FDisabledLargeImages;
                 TSpkBaseItem(Item).ToolbarDispatch:=FToolbarDispatch;
                 end;
       opRemove: begin
                 if not(csDestroying in Item.ComponentState) then
                    begin
                    TSpkBaseItem(Item).ToolbarDispatch:=nil;
                    TSpkBaseItem(Item).Appearance:=nil;
                    TSpkBaseItem(Item).Images:=nil;
                    TSpkBaseItem(Item).DisabledImages:=nil;
                    TSpkBaseItem(Item).LargeImages:=nil;
                    TSpkBaseItem(Item).DisabledLargeImages:=nil;
                    end;
                 end;
  end;
end;

procedure TSpkItems.SetAppearance(const Value: TSpkToolbarAppearance);

var i: Integer;

begin
  FAppearance := Value;
  for i := 0 to Count - 1 do
    Items[i].Appearance := Value;
end;

procedure TSpkItems.SetDisabledImages(const Value: TImageList);

var i: Integer;

begin
  FDisabledImages := Value;
  for i := 0 to Count - 1 do
    Items[i].DisabledImages := Value;
end;

procedure TSpkItems.SetDisabledLargeImages(const Value: TImageList);

var i: Integer;

begin
  FDisabledLargeImages := Value;
  for i := 0 to Count - 1 do
    Items[i].DisabledLargeImages := Value;
end;

procedure TSpkItems.SetImages(const Value: TImageList);

var i: Integer;

begin
  FImages := Value;
  for i := 0 to Count - 1 do
    Items[i].Images := Value;
end;

procedure TSpkItems.SetLargeImages(const Value: TImageList);

var i: Integer;

begin
  FLargeImages := Value;
  for i := 0 to Count - 1 do
    Items[i].LargeImages := Value;
end;

procedure TSpkItems.SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);

var i : integer;

begin
  FToolbarDispatch := Value;
  for i := 0 to Count - 1 do
    Items[i].ToolbarDispatch := Value;
end;

procedure TSpkItems.Update;
begin
  inherited Update;

  if assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyItemsChanged;
end;

end.
