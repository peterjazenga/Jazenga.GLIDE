{***********************************************************************
 Package pl_Barcode
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}
unit StDbPNBC;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, ClipBrd, Controls, Graphics, Messages, SysUtils,
  Db, DbCtrls,
  StCommon, StBarPN;

type
  TStDbPNBarCode = class(TStPNBarCode)
  protected 
    FDataLink : TFieldDataLink;
    procedure DataChange(Sender : TObject);
    function  GetDataField : string;
    function  GetDataSource : TDataSource;
    procedure SetDataField(const Value : string);
    procedure SetDataSource(Value : TDataSource);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;  
  published
    property PostalCode stored False;
    property DataField : string read GetDataField write SetDataField;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
  end;


implementation

{*** TStDbPNBarCode ***}

constructor TStDbPNBarCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
end;

procedure TStDbPNBarCode.DataChange(Sender : TObject);
begin
  if FDataLink.Field = nil then
    PostalCode := '12345'
  else
    PostalCode := FDataLink.Field.DisplayText;
end;

destructor TStDbPNBarCode.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

function TStDbPNBarCode.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end;

function TStDbPNBarCode.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource
end;

procedure TStDbPNBarCode.SetDataField(const Value : string);
begin
  FDataLink.FieldName := Value;
end;

procedure TStDbPNBarCode.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
end;


end.
