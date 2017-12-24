{ TPdfExportOptions unit

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

unit RxDBGridExportPdfSetupUnit;

{$mode objfpc}{$H+}

interface

{$IF (FPC_FULLVERSION >= 30101)}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel, ExtCtrls, ComCtrls, ColorBox, fpPDF;

type

  { TRxDBGridExportPdfSetupForm }

  TRxDBGridExportPdfSetupForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbExportCellColors: TCheckBox;
    cbExportColumnFooter: TCheckBox;
    cbExportColumnHeader: TCheckBox;
    cbOpenAfterExport: TCheckBox;
    cbOverwriteExisting: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    ColorBox1: TColorBox;
    ComboBox1: TComboBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    RadioGroup1: TRadioGroup;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  RxDBGridExportPdfSetupForm: TRxDBGridExportPdfSetupForm;

const
  pdfPaperNames : array [TPDFPaperType] of string =
    ('Custom',       //ptCustom
     'A4',           //ptA4
     'A5',           //ptA5
     'Letter',       // ptLetter
     'Legal',        // ptLegal
     'Executive',    // ptExecutive
     'Comm10',       // ptComm10
     'Monarch',      // ptMonarch
     'DL',           // ptDL
     'C5',           // ptC5
     'B5'            // ptB5
   );

  {$ENDIF}
implementation
{$IF (FPC_FULLVERSION >= 30101)}
uses rxdconst;

{$R *.lfm}

{ TRxDBGridExportPdfSetupForm }

procedure TRxDBGridExportPdfSetupForm.FormCreate(Sender: TObject);
var
  c: TPDFPaperType;
begin
  PageControl1.ActivePageIndex:=0;

  Caption:=sExportParams;
  TabSheet1.Caption:=sGlobal;
  TabSheet2.Caption:=sPDFOptions;
  Label1.Caption:=sExportFileName;
  cbOpenAfterExport.Caption:=sOpenAfterExport;
  cbExportColumnHeader.Caption:=sExportColumnHeader;
  cbExportColumnFooter.Caption:=sExportColumnFooter;
  cbExportCellColors.Caption:=sExportCellColors;
  cbOverwriteExisting.Caption:=sOverwriteExisting;
  CheckBox6.Caption:=sExportImages;
  Label2.Caption:=sPaperType;
  Label5.Caption:=sTitleColor;
  CheckBox1.Caption:=sOutLine;
  CheckBox2.Caption:=sCompressText;
  CheckBox3.Caption:=sCompressFonts;
  CheckBox4.Caption:=sCompressImages;
  CheckBox5.Caption:=sUseRawJPEG;

  RadioGroup1.Caption:=sOrientation;
  RadioGroup1.Items[0]:=sPortrait;
  RadioGroup1.Items[1]:=sLandscape;

  ComboBox1.Items.Clear;
  for C:=ptA4 to High(TPDFPaperType) do
    ComboBox1.Items.Add(pdfPaperNames[C]);
end;

{$ENDIF}
end.

