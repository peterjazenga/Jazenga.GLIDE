{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit wst_cursors;

interface

uses
  Classes, SysUtils,
  wst_types, cursor_intf, base_service_intf;
  
type

  { TBaseObjectArrayRemotableCursor }

  TBaseObjectArrayRemotableCursor = class(TInterfacedObject,ICursor,IObjectCursor)
  private
    FList : TBaseObjectArrayRemotable;
    FCurrentIndex : PtrInt;
  protected
    procedure Reset();
    function MoveNext() : Boolean;virtual;
    function Clone():ICursor;
    function GetCount() : PtrInt;
    function GetCurrent() : TObject;
  public
    constructor Create(ADataList : TBaseObjectArrayRemotable);
  end;

  { TBaseObjectArrayRemotableFilterableCursor }

  TBaseObjectArrayRemotableFilterableCursor = class(TBaseObjectArrayRemotableCursor,IFilterableObjectCursor)
  private
    FFilter : IObjectFilter;
  protected
    function MoveNext() : Boolean;override;
    function GetFilter() : IObjectFilter;
    function SetFilter(const AFilter : IObjectFilter) : IObjectFilter;
  public
    destructor Destroy();override;
  end;
  
  { TObjectCollectionRemotableCursor }

  TObjectCollectionRemotableCursor = class(TInterfacedObject,ICursor,IObjectCursor)
  private
    FList : TObjectCollectionRemotable;
    FCurrentIndex : PtrInt;
  protected
    procedure Reset();
    function MoveNext() : Boolean;virtual;
    function Clone():ICursor;
    function GetCount() : PtrInt;
    function GetCurrent() : TObject;
  public
    constructor Create(ADataList : TObjectCollectionRemotable);
  end;
  
  function Find(
    const AList : TBaseObjectArrayRemotable;
    const AFilter : string
  ) : TBaseRemotable;overload;
  function Find(
    const AList : TObjectCollectionRemotable;
    const AFilter : string
  ) : TBaseRemotable;overload;

  function Filter(
    const AList : TBaseObjectArrayRemotable;
    const AFilter : string
  ) : IFilterableObjectCursor;overload;
  function Filter(
    const AList : TObjectCollectionRemotable;
    const AFilter : string
  ) : IFilterableObjectCursor;overload;

implementation
uses
  imp_utils, rtti_filters;
  
function Find(
  const AList : TBaseObjectArrayRemotable;
  const AFilter : string
) : TBaseRemotable ;
var
  locRes : TBaseRemotable;
  crs : IObjectCursor;
  fltr : IObjectFilter;
begin
  locRes := nil;
  if ( AList <> nil ) and ( AList.Length > 0 ) then begin
    if IsStrEmpty(AFilter) then begin
      locRes := AList[0];
    end else begin
      fltr := ParseFilter(AFilter,AList.GetItemClass());
      crs := CreateCursorOn(TBaseObjectArrayRemotableCursor.Create(AList),fltr);
      crs.Reset();
      if crs.MoveNext() then
        locRes := TBaseRemotable(crs.GetCurrent());
    end;
  end;
  Result := locRes;
end;

function Find(
  const AList : TObjectCollectionRemotable;
  const AFilter : string
) : TBaseRemotable ;
var
  locRes : TBaseRemotable;
  crs : IObjectCursor;
  fltr : IObjectFilter;
begin
  locRes := nil;
  if ( AList <> nil ) and ( AList.Length > 0 ) then begin
    if IsStrEmpty(AFilter) then begin
      locRes := AList[0];
    end else begin
      fltr := ParseFilter(AFilter,AList.GetItemClass());
      crs := CreateCursorOn(TObjectCollectionRemotableCursor.Create(AList),fltr);
      crs.Reset();
      if crs.MoveNext() then
        locRes := TBaseRemotable(crs.GetCurrent());
    end;
  end;
  Result := locRes;
end;

function Filter(
  const AList : TBaseObjectArrayRemotable;
  const AFilter : string
) : IFilterableObjectCursor ;
var
  crs : IFilterableObjectCursor;
  fltr : IObjectFilter;
begin
  crs := nil;
  if ( AList <> nil ) then begin
    if IsStrEmpty(AFilter) then begin
      crs := CreateCursorOn(TBaseObjectArrayRemotableCursor.Create(AList),nil);
    end else begin
      fltr := ParseFilter(AFilter,AList.GetItemClass());
      crs := CreateCursorOn(TBaseObjectArrayRemotableCursor.Create(AList),fltr);
      crs.Reset();
    end;
  end;
  Result := crs;
end;

function Filter(
  const AList : TObjectCollectionRemotable;
  const AFilter : string
) : IFilterableObjectCursor ;
var
  crs : IFilterableObjectCursor;
  fltr : IObjectFilter;
begin
  crs := nil;
  if ( AList <> nil ) then begin
    if IsStrEmpty(AFilter) then begin
      crs := CreateCursorOn(TObjectCollectionRemotableCursor.Create(AList),nil);
    end else begin
      fltr := ParseFilter(AFilter,AList.GetItemClass());
      crs := CreateCursorOn(TObjectCollectionRemotableCursor.Create(AList),fltr);
      crs.Reset();
    end;
  end;
  Result := crs;
end;

{ TBaseObjectArrayRemotableCursor }

procedure TBaseObjectArrayRemotableCursor.Reset();
begin
  FCurrentIndex := -1;
end;

function TBaseObjectArrayRemotableCursor.MoveNext() : Boolean;
begin
  Inc(FCurrentIndex);
  Result := ( FCurrentIndex < FList.Length );
end;

function TBaseObjectArrayRemotableCursor.Clone() : ICursor;
begin
  Result := TBaseObjectArrayRemotableCursor.Create(FList) as ICursor;
end;

function TBaseObjectArrayRemotableCursor.GetCount() : PtrInt;
begin
  Result := FList.Length;
end;

function TBaseObjectArrayRemotableCursor.GetCurrent() : TObject;
begin
  if ( FCurrentIndex < 0 ) or ( FCurrentIndex >= FList.Length ) then
    raise ECursorException.Create('Invalid cursor state.');
  Result := FList[FCurrentIndex];
end;

constructor TBaseObjectArrayRemotableCursor.Create(ADataList : TBaseObjectArrayRemotable);
begin
  Assert(Assigned(ADataList));
  FList := ADataList;
  Reset();
end;

{ TBaseObjectArrayRemotableFilterableCursor }

function TBaseObjectArrayRemotableFilterableCursor.MoveNext() : Boolean;
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

function TBaseObjectArrayRemotableFilterableCursor.GetFilter() : IObjectFilter;
begin
  Result := FFilter;
end;

function TBaseObjectArrayRemotableFilterableCursor.SetFilter(
  const AFilter : IObjectFilter
) : IObjectFilter;
begin
  FFilter := AFilter;
  Result := FFilter;
end;

destructor TBaseObjectArrayRemotableFilterableCursor.Destroy();
begin
  FFilter := nil;
  inherited Destroy();
end;

{ TObjectCollectionRemotableCursor }

procedure TObjectCollectionRemotableCursor.Reset();
begin
  FCurrentIndex := -1;
end;

function TObjectCollectionRemotableCursor.MoveNext() : Boolean;
begin
  Inc(FCurrentIndex);
  Result := ( FCurrentIndex < FList.Length );
end;

function TObjectCollectionRemotableCursor.Clone() : ICursor;
begin
  Result := TObjectCollectionRemotableCursor.Create(FList) as ICursor;
end;

function TObjectCollectionRemotableCursor.GetCount() : PtrInt;
begin
  Result := FList.Length;
end;

function TObjectCollectionRemotableCursor.GetCurrent() : TObject;
begin
  if ( FCurrentIndex < 0 ) or ( FCurrentIndex >= FList.Length ) then
    raise ECursorException.Create('Invalid cursor state.');
  Result := FList[FCurrentIndex];
end;

constructor TObjectCollectionRemotableCursor.Create(ADataList : TObjectCollectionRemotable);
begin
  Assert(Assigned(ADataList));
  FList := ADataList;
  Reset();
end;

end.

