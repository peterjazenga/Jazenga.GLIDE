{ rxceEditLookupFields unit

  Copyright (C) 2005-2010 Lagunov Aleksey alexs@yandex.ru and Lazarus team
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

unit rxceEditLookupFields;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type

  { TLookupFieldProperty }

  TLookupFieldProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure FillValues(const Values: TStrings); virtual;
  end;

  { TLookupDisplayProperty }

  TLookupDisplayProperty = class(TLookupFieldProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  
procedure RegisterCEEditLookupFields;
implementation
uses
  //
  db, rxduallist, Forms, rxstrutils, TypInfo, rxdconst,
  //unit for edits
  rxlookup;

procedure RegisterCEEditLookupFields;
begin
  RegisterPropertyEditor(TypeInfo(string), TRxDBLookupCombo, 'LookupField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxDBLookupCombo, 'LookupDisplay', TLookupDisplayProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxLookupEdit, 'LookupField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxLookupEdit, 'LookupDisplay', TLookupDisplayProperty);
end;

{ TLookupFieldProperty }

function TLookupFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure TLookupFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    FillValues(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TLookupFieldProperty.FillValues(const Values: TStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), 'LookupSource') as TDataSource;
//  DataSource := TRxDBLookupCombo(GetComponent(0)).LookupSource;
  if (DataSource is TDataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;

{ TLookupDisplayProperty }

function TLookupDisplayProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes + [paDialog]
end;

procedure TLookupDisplayProperty.Edit;
var
  DualListDialog1: TDualListDialog;
  Cmp1:TRxDBLookupCombo;
  Cmp2:TRxLookupEdit;

procedure DoInitFill;
var
  i,j:integer;
  LookupDisplay:string;
begin
  if Assigned(Cmp1) then
    LookupDisplay:=Cmp1.LookupDisplay
  else
    LookupDisplay:=Cmp2.LookupDisplay;
  if LookupDisplay<>'' then
  begin
    StrToStrings(LookupDisplay, DualListDialog1.List2, ';');
    for i:=DualListDialog1.List1.Count-1 downto 0 do
    begin
      j:=DualListDialog1.List2.IndexOf(DualListDialog1.List1[i]);
      if j>=0 then
        DualListDialog1.List1.Delete(i);
    end;
  end;
end;

function DoFillDone:string;
var
  i:integer;
begin
  for i:=0 to DualListDialog1.List2.Count-1 do
    Result:=Result + DualListDialog1.List2[i]+';';
  if Result<>'' then
    Result:=Copy(Result, 1, Length(Result)-1);
end;

procedure DoSetCaptions;
begin
  DualListDialog1.Label1Caption:=sRxAllFields;
  DualListDialog1.Label2Caption:=sRxFieldsLookupDisplay;
  DualListDialog1.Title:=sRxFillFieldsLookupDisp;
end;

begin
  Cmp1:=nil;
  Cmp2:=nil;

  if GetComponent(0) is TRxDBLookupCombo then
    Cmp1:=TRxDBLookupCombo(GetComponent(0))
  else
    Cmp2:=TRxLookupEdit(GetComponent(0));
  
  DualListDialog1:=TDualListDialog.Create(Application);
  try
    DoSetCaptions;
    FillValues(DualListDialog1.List1);
    DoInitFill;
    if DualListDialog1.Execute then
    begin
      if Assigned(Cmp1) then
        Cmp1.LookupDisplay:=DoFillDone
      else
        Cmp2.LookupDisplay:=DoFillDone;
    end;
  finally
    FreeAndNil(DualListDialog1);
  end;
end;

end.

