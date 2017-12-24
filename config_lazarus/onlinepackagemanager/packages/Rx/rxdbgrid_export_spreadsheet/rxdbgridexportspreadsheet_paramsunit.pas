{ RxDBGridExportSpreadSheet_ParamsUnit unit

  Copyright (C) 2005-2013 Lagunov Aleksey alexs@yandex.ru
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

unit RxDBGridExportSpreadSheet_ParamsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel;

type

  { TRxDBGridExportSpreadSheet_ParamsForm }

  TRxDBGridExportSpreadSheet_ParamsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbExportSelectedRows: TCheckBox;
    cbExportFormula: TCheckBox;
    cbExportColumnFooter: TCheckBox;
    cbOpenAfterExport: TCheckBox;
    cbExportColumnHeader: TCheckBox;
    cbExportCellColors: TCheckBox;
    cbOverwriteExisting: TCheckBox;
    edtPageName: TEdit;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation
uses rxdconst;

{$R *.lfm}

{ TRxDBGridExportSpreadSheet_ParamsForm }

procedure TRxDBGridExportSpreadSheet_ParamsForm.FormCreate(Sender: TObject);
begin
  Caption:=sExportParams;
  Label1.Caption:=sExportFileName;
  Label3.Caption:=sPageName;
  cbOpenAfterExport.Caption:=sOpenAfterExport;
  cbExportColumnHeader.Caption:=sExportColumnHeader;
  cbExportColumnFooter.Caption:=sExportColumnFooter;
  cbExportCellColors.Caption:=sExportCellColors;
  cbOverwriteExisting.Caption:=sOverwriteExisting;
  cbExportFormula.Caption:=sExportFormula;
  cbExportSelectedRows.Caption:=sExportSelectedRows;
end;

end.

