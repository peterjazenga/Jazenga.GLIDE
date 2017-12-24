unit mvextradata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,graphics;

type

  { TDrawingExtraData }

  TDrawingExtraData = class
  private
    FColor: TColor;
    FId: integer;
    procedure SetColor(AValue: TColor);
  public
    constructor Create(aId : integer);virtual;
    property Color : TColor read FColor write SetColor;
    property Id : integer read FId;
  End;

implementation

{ TDrawingExtraData }


procedure TDrawingExtraData.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

constructor TDrawingExtraData.Create(aId: integer);
begin
  FId:=aId;
  FColor:=clRed;
end;

end.

