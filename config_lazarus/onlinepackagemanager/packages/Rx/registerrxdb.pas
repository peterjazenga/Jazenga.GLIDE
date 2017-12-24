{ RegisterRxDB unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
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

unit RegisterRxDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;
implementation
uses DB, DBPropEdits, rxdbgrid, RxDBSpinEdit, RxDBTimeEdit, RxDBCtrls, rxmemds,
  ComponentEditors, rxseldsfrm, PropEdits, RxDBColorBox, rxdbdateedit, rxdbcomb,
  rxlookup, rxdbcurredit, RxDBGridFooterTools
  {$IF (FPC_FULLVERSION >= 30101)}
  , RxDBGridExportPdf
  {$ENDIF}
  ;

type

{ TRxDBGridFieldProperty }
  TRxDBGridFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TRxDBGridFieldProperty }

procedure TRxDBGridFieldProperty.FillValues(const Values: TStringList);
var
  Column: TRxColumn;
  Grid: TRxDBGrid;
  DataSource: TDataSource;
begin
  Column:=TRxColumn(GetComponent(0));
  if not (Column is TRxColumn) then exit;
  Grid:=TRxDBGrid(Column.Grid);
  if not (Grid is TRxDBGrid) then exit;
  DataSource := Grid.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;

type
{ TRxDBGridFooterFieldProperty }
  TRxDBGridFooterFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TRxDBGridFieldProperty }

procedure TRxDBGridFooterFieldProperty.FillValues(const Values: TStringList);
var
  Grid: TRxDBGrid;
  DataSource: TDataSource;
begin
  if GetComponent(0) is TRxColumnFooterItem then
    Grid:=TRxDBGrid(TRxColumnFooterItem(GetComponent(0)).Owner.Grid)
  else
(*  if GetComponent(0) is TRxColumnFooter then
    Grid:=TRxDBGrid(TRxColumnFooter(GetComponent(0)).Owner.Grid)
  else *)
    exit;
  if not (Grid is TRxDBGrid) then exit;

  DataSource := Grid.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;


procedure RegisterRxDBSpinEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBSpinEdit]);
end;

procedure RegisterRxDBTimeEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBTimeEdit]);
end;

procedure RegisterRxDBCtrls;
begin
  RegisterComponents('RX DBAware',[TRxDBProgressBar, TRxDBTrackBar, TRxDBRadioGroup]);
end;

procedure RegisterRxDbGrid;
begin
  RegisterComponents('RX DBAware',[TRxDBGrid]);
end;

procedure RegisterRxDbGridFooterTools;
begin
  RegisterComponents('RX DBAware',[TRxDBGridFooterTools]);
end;

{$IF (FPC_FULLVERSION >= 30101)}
procedure RegisterRxDBGridExportPDF;
begin
  RegisterComponents('RX DBAware',[TRxDBGridExportPDF]);
end;
{$ENDIF}

procedure RegisterRxMemDS;
begin
  RegisterComponents('RX DBAware',[TRxMemoryData]);
end;

procedure RegisterRxDBColorBox;
begin
  RegisterComponents('RX DBAware',[TRxDBColorBox]);
end;

procedure RegisterUnitDBDateEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBDateEdit, TRxDBCalcEdit, TRxDBCurrEdit]);
end;

procedure RegisterRXLookup;
begin
  RegisterComponents('RX DBAware',[TRXLookupEdit, TRxDBLookupCombo]);
end;

procedure RegisterRxDBComb;
begin
  RegisterComponents('RX DBAware',[TRxDBComboBox]);
end;

procedure Register;
begin
  //RX DBAware
  RegisterUnit('rxdbdateedit', @RegisterUnitDBDateEdit);
  RegisterUnit('rxlookup', @RegisterRXLookup);
  RegisterUnit('rxdbcomb', @RegisterRxDBComb);

  RegisterUnit('RxDBTimeEdit', @RegisterRxDBTimeEdit);
  RegisterUnit('RxDBSpinEdit', @RegisterRxDBSpinEdit);
  RegisterUnit('RxDBCtrls', @RegisterRxDBCtrls);
  RegisterUnit('rxdbgrid', @RegisterRxDbGrid);
  RegisterUnit('rxmemds', @RegisterRxMemDS);
  RegisterUnit('RxDBColorBox', @RegisterRxDBColorBox);
  RegisterUnit('RxDBGridFooterTools', @RegisterRxDbGridFooterTools);
  {$IF (FPC_FULLVERSION >= 30101)}
  RegisterUnit('RxDBGridExportPdf', @RegisterRxDBGridExportPDF);
  {$ENDIF}

  //Component Editors
  RegisterComponentEditor(TRxMemoryData, TMemDataSetEditor);

  //
  RegisterPropertyEditor(TypeInfo(string), TRxColumn, 'FieldName', TRxDBGridFieldProperty);
(*  RegisterPropertyEditor(TypeInfo(string), TRxColumnFooter, 'FieldName', TRxDBGridFooterFieldProperty); *)
  RegisterPropertyEditor(TypeInfo(string), TRxColumnFooterItem, 'FieldName', TRxDBGridFooterFieldProperty);
end;

end.

