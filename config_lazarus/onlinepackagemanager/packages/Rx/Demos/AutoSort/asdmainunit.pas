{ Demo application from RXFPC

  Copyright (C) 2009 Lagunov Aleksey (alexs75@hotbox.ru)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit asdMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, rxmemds, rxdbgrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Datasource1: TDatasource;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1DATE_ENTER: TDateField;
    RxMemoryData1ID: TLongintField;
    RxMemoryData1NAME: TStringField;
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Delphi', EncodeDate(1995, 8, 1)]);
  RxMemoryData1.AppendRecord([2, 'Turbo Pascal', EncodeDate(1983, 8, 1)]);
  RxMemoryData1.AppendRecord([3, 'Free Pascal', EncodeDate(1993, 1, 1)]);
  RxMemoryData1.AppendRecord([4, 'Lazarus', Now]);

  CheckBox2.Checked:=RxDBGrid1.AutoSort;
  CheckBox1.Checked:=RxDBGrid1.TitleButtons;

end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  RxDBGrid1.AutoSort:=CheckBox2.Checked;
  RxDBGrid1.TitleButtons:=CheckBox1.Checked;
end;

end.

