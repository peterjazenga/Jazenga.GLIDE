{***********************************************************************
 Package pl_Barcode
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit udbbarcodedatamatrix;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, ClipBrd, Controls, Graphics, Messages, SysUtils,
  DB, DBCtrls,
  ubarcodes;

type

  TdbBarcodeDataMatrix = class(TBarcodeDataMatrix)
  protected
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function  GetDataField: string;
    function  GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Text stored False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


implementation


{*** TdbBarcodeDataMatrix ***}

constructor TdbBarcodeDataMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
end;

procedure TdbBarcodeDataMatrix.DataChange(Sender: TObject);
begin
  if FDataLink.Field = nil then
    Text := '12345678922'
  else
    Text := FDataLink.Field.DisplayText;
end;

destructor TdbBarcodeDataMatrix.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

function TdbBarcodeDataMatrix.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TdbBarcodeDataMatrix.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TdbBarcodeDataMatrix.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TdbBarcodeDataMatrix.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;


end.
