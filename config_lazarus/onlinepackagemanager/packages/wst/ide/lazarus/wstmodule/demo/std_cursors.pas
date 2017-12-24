{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007, 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{ $INCLUDE wst_global.inc}
unit std_cursors;

interface

uses
  Classes, SysUtils, Contnrs,
  cursor_intf;

{ $INCLUDE wst.inc}
{ $INCLUDE wst_delphi.inc}

type

  { TObjectListCursor }

  TObjectListCursor = class(TInterfacedObject,ICursor,IObjectCursor)
  private
    FList : TObjectList;
    FCurrentIndex : Integer;
  protected
    procedure Reset();
    function MoveNext() : Boolean;virtual;
    function Clone(): ICursor;
    function GetCount() : Integer;
    function GetCurrent() : TObject;
  public
    constructor Create(ADataList : TObjectList);
  end;

  { TObjectListFilterableCursor }

  TObjectListFilterableCursor = class(TObjectListCursor,IFilterableObjectCursor)
  private
    FFilter : IObjectFilter;
  protected
    function MoveNext() : Boolean;override;
    function GetFilter() : IObjectFilter;
    function SetFilter(const AFilter : IObjectFilter) : IObjectFilter;
  public
    destructor Destroy();override;
  end;

implementation

{ TObjectListCursor }

procedure TObjectListCursor.Reset();
begin
  FCurrentIndex := -1;
end;

function TObjectListCursor.MoveNext(): Boolean;
begin
  Inc(FCurrentIndex);
  Result := ( FCurrentIndex < FList.Count );
end;

function TObjectListCursor.Clone(): ICursor;
begin
  Result := TObjectListCursor.Create(FList) as ICursor;
end;

function TObjectListCursor.GetCount() : Integer;
begin
  Result := FList.Count;
end;

function TObjectListCursor.GetCurrent(): TObject;
begin
  if ( FCurrentIndex < 0 ) or ( FCurrentIndex >= FList.Count ) then
    raise ECursorException.Create('Invalid cursor state.');
  Result := FList[FCurrentIndex];
end;

constructor TObjectListCursor.Create(ADataList: TObjectList);
begin
  Assert(Assigned(ADataList));
  FList := ADataList;
  Reset();
end;

{ TObjectListFilterableCursor }

function TObjectListFilterableCursor.MoveNext(): Boolean;
begin
  if ( FFilter = nil ) then begin
    Result := inherited MoveNext();
  end else begin
    while ( inherited MoveNext() ) do begin
      if FFilter.Evaluate(GetCurrent()) then begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;
end;

function TObjectListFilterableCursor.GetFilter(): IObjectFilter;
begin
  Result := FFilter;
end;

function TObjectListFilterableCursor.SetFilter(
  const AFilter: IObjectFilter
): IObjectFilter;
begin
  FFilter := AFilter;
  Result := FFilter;
end;

destructor TObjectListFilterableCursor.Destroy();
begin
  FFilter := nil;
  inherited Destroy();
end;

end.

