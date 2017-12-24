{ RxDBGrid sort engine module for FBDataSet

  Copyright (C) 2011 BugMaker from freepascal.ru

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

unit exsortsql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid;

type

  { TSQLQuerySortEngine }

  TSQLQuerySortEngine = class(TRxDBGridSortEngine)
  public
    procedure Sort(FieldName: string; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField: string; ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions); override;
  end;

implementation
uses SQLDB, RegExpr, strUtils;


procedure TSQLQuerySortEngine.Sort(FieldName: string; ADataSet: TDataSet;
  Asc: boolean; SortOptions: TRxSortEngineOptions);
begin
  SortList(FieldName,ADataSet,Asc,SortOptions);
end;

procedure TSQLQuerySortEngine.SortList(ListField: string; ADataSet: TDataSet;
  Asc: array of boolean; SortOptions: TRxSortEngineOptions);
var
  S:string;
  i, C:integer;
  cmd:string;
  strLen:Integer;
  RegExpr: TRegExpr;
  Mask:String;
  OldParams:TParams;
begin
    if not (Assigned(ADataSet) and ADataset.Active) then exit;

    S:='';
    C:=Pos(';', ListField);
    i:=0;
    while C>0 do
    begin
      if S<>'' then S:=S+',';
      S:=S + Copy(ListField, 1, C-1);
      Delete(ListField, 1, C);

      if (i<=High(Asc)) and (not Asc[i]) then
        S:=S + ' DESC';
      C:=Pos(';', ListField);
      inc(i);
    end;

    if ListField<>'' then
    begin
      if S<>'' then S:=S+',';
      S:=S + ListField;
      if (i<=High(Asc)) and (not Asc[i]) then
        S:=S + ' DESC';
    end;

    OldParams:=TParams.Create;
    OldParams.Assign((ADataSet as TSQLQuery).Params);
    cmd:=(ADataSet as TSQLQuery).SQL.Text;
    strlen:=length(cmd);
    //Регулярное выражение позволяет найти уже имеющуюся конструкцию ORDER BY,
    //если она написана одной строкой, и между словами не понапихали комментариев :)
    //Работоспособные примеры:
    //ORDER BY FIELD1, FIELD2 DESC, FIELD100500
    //oRdeR bY    fielD1   ,   FiElD2,FieLD100500 DESC
    //Неработоспособный:
    //ORDER BY FIELD1,
    //FIELD2,
    //FIELD100500
    mask:='(?i)(^|\s)\s*order\s+by\s+\S+\.?\S*(\s+desc)?\s*(,\s*\S+\.?\S*(\s+desc)?(^|s*))*';
    with TRegExpr.Create do begin

      Expression := mask;
      if Exec(cmd) then begin
        s:=LeftStr(cmd,MatchPos[0]-1)
        +slinebreak+'order by '
        +s+slineBreak
        +RightStr(cmd, strlen-MatchPos[0]-MatchLen[0]+1);
      end
      else
        s:=cmd+slinebreak+'order by '+s+slineBreak;

      ADataSet.Active:=False;
      (ADataSet as TSQLQuery).SQL.Text:=s;
      (ADataSet as TSQLQuery).Params.Assign(OldParams);
      OldParams.Free;
      ADataSet.Active:=True;
      Free;
    end;

end;

initialization
  RegisterRxDBGridSortEngine(TSQLQuerySortEngine, 'TSQLQuery');
end.
