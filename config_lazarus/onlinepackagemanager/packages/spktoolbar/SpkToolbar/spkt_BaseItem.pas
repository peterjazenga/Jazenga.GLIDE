unit spkt_BaseItem;

(*******************************************************************************
*                                                                              *
*  Plik: spkt_BaseItem.pas                                                     *
*  Opis: Modu³ zawieraj¹cy bazow¹ klasê dla elementu tafli.                    *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*******************************************************************************)

{$mode delphi}
{.$Define EnhancedRecordSupport}

interface

uses Graphics, Classes, Controls,
     SpkMath, spkt_Appearance, spkt_Dispatch, spkt_Types;

type TSpkItemSize = (isLarge, isNormal);
     TSpkItemTableBehaviour = (tbBeginsRow, tbBeginsColumn, tbContinuesRow);
     TSpkItemGroupBehaviour = (gbSingleItem, gbBeginsGroup, gbContinuesGroup, gbEndsGroup);

     TSpkBaseItem = class abstract(TSpkComponent)
     private
     protected
       FRect : T2DIntRect;
       FToolbarDispatch : TSpkBaseToolbarDispatch;
       FAppearance : TSpkToolbarAppearance;
       FImages : TImageList;
       FDisabledImages : TImageList;
       FLargeImages : TImageList;
       FDisabledLargeImages : TImageList;
       FVisible : boolean;
       FEnabled : boolean;

       procedure SetVisible(const Value: boolean); virtual;
       procedure SetEnabled(const Value: boolean); virtual;
       procedure SetRect(const Value: T2DIntRect); virtual;
       procedure SetImages(const Value: TImageList); virtual;
       procedure SetDisabledImages(const Value : TImageList); virtual;
       procedure SetLargeImages(const Value: TImageList); virtual;
       procedure SetDisabledLargeImages(const Value: TImageList); virtual;
       procedure SetAppearance(const Value: TSpkToolbarAppearance);
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;

       procedure MouseLeave; virtual; abstract;
       procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer); virtual; abstract;
       procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
       procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer); virtual; abstract;

       function GetWidth : integer; virtual; abstract;
       function GetTableBehaviour : TSpkItemTableBehaviour; virtual; abstract;
       function GetGroupBehaviour : TSpkItemGroupBehaviour; virtual; abstract;
       function GetSize : TSpkItemSize; virtual; abstract;
       procedure Draw(ABuffer : TBitmap; ClipRect : T2DIntRect); virtual; abstract;

       property ToolbarDispatch : TSpkBaseToolbarDispatch read FToolbarDispatch write FToolbarDispatch;
       property Appearance : TSpkToolbarAppearance read FAppearance write SetAppearance;
       property Images : TImageList read FImages write SetImages;
       property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
       property LargeImages : TImageList read FLargeImages write SetLargeImages;
       property DisabledLargeImages : TImageList read FDisabledLargeImages write SetDisabledLargeImages;

       property Rect : T2DIntRect read FRect write SetRect;
     published
       property Visible : boolean read FVisible write SetVisible;
       property Enabled : boolean read FEnabled write SetEnabled;
     end;

type TSpkBaseItemClass = class of TSpkBaseItem;

implementation

{ TSpkBaseItem }

constructor TSpkBaseItem.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF EnhancedRecordSupport}
  FRect:=T2DIntRect.create(0, 0, 0, 0);
  {$ELSE}
  FRect.create(0, 0, 0, 0);
  {$ENDIF}

  FToolbarDispatch:=nil;
  FAppearance:=nil;
  FImages:=nil;
  FDisabledImages:=nil;
  FLargeImages:=nil;
  FDisabledLargeImages:=nil;
  FVisible:=true;
  FEnabled:=true;
end;

destructor TSpkBaseItem.Destroy;
begin
  { Pozosta³e operacje }
  inherited Destroy;
end;

procedure TSpkBaseItem.SetAppearance(const Value: TSpkToolbarAppearance);
begin
  FAppearance := Value;

  if assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseItem.SetDisabledImages(const Value: TImageList);
begin
  FDisabledImages := Value;
end;

procedure TSpkBaseItem.SetDisabledLargeImages(const Value: TImageList);
begin
FDisabledLargeImages:=Value;
end;

procedure TSpkBaseItem.SetEnabled(const Value: boolean);
begin
  if Value<>FEnabled then
     begin
     FEnabled:=Value;
     if FToolbarDispatch<>nil then
        FToolbarDispatch.NotifyVisualsChanged;
     end;
end;

procedure TSpkBaseItem.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

procedure TSpkBaseItem.SetLargeImages(const Value: TImageList);
begin
  FLargeImages := Value;
end;

procedure TSpkBaseItem.SetRect(const Value: T2DIntRect);
begin
  FRect := Value;
end;

procedure TSpkBaseItem.SetVisible(const Value: boolean);
begin
  if Value<>FVisible then
     begin
     FVisible:=Value;
     if FToolbarDispatch<>nil then
        FToolbarDispatch.NotifyMetricsChanged;
     end;
end;

end.
