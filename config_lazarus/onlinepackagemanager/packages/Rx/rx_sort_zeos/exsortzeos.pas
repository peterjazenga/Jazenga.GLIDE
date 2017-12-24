{ exsortzeos unit

  Copyright (C) 2005-2015 Lagunov Aleksey alexs@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit exsortzeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid, ZAbstractRODataset;

type

  { TZeosDataSetSortEngine }

  TZeosDataSetSortEngine = class(TRxDBGridSortEngine)
  protected
  public
    procedure Sort(FieldName: string; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField: string; ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions); override;
  end;

implementation
uses ZDbcIntfs, ZVariant;

function FixFieldName(S:string):string;inline;
begin
  if not IsValidIdent(S) then
    Result:='"'+S+'"'
  else
    Result:=S;
end;

procedure TZeosDataSetSortEngine.Sort(FieldName: string; ADataSet: TDataSet;
  Asc: boolean; SortOptions: TRxSortEngineOptions);
begin
  if not Assigned(ADataSet) then exit;

  if ADataSet is TZAbstractRODataset then
  begin
    if Asc then
      FieldName := FixFieldName(FieldName) + ' Asc'
    else
      FieldName := FixFieldName(FieldName) + ' Desc';
    TZAbstractRODataset(ADataSet).SortedFields:=FieldName;
{


    if Asc then
      TZAbstractRODataset(ADataSet).SortType:=stAscending
    else
      TZAbstractRODataset(ADataSet).SortType:=stDescending;}
  end;
end;

procedure TZeosDataSetSortEngine.SortList(ListField: string;
  ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions);
var
  S:string;
  i, C:integer;
begin
  if not Assigned(ADataSet) then exit;

  S:='';
  C:=Pos(';', ListField);
  i:=0;
  while C>0 do
  begin
    if S<>'' then S:=S+';';
    S:=S + FixFieldName(Copy(ListField, 1, C-1));
    Delete(ListField, 1, C);

    if (i<=High(Asc)) and (not Asc[i]) then
      S:=S + ' DESC';
    C:=Pos(';', ListField);
    inc(i);
  end;

  if ListField<>'' then
  begin
    if S<>'' then S:=S+';';
    S:=S + FixFieldName(ListField);
    if (i<=High(Asc)) and (not Asc[i]) then
      S:=S + ' DESC';
  end;

  (ADataSet as TZAbstractRODataset).SortedFields:=S;
end;


initialization
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZReadOnlyQuery');
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZQuery');
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZTable');
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZMacroQuery');
end.

