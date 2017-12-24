{ RxDBGridPrintGrid unit

  Copyright (C) 2005-2014 Lagunov Aleksey alexs@yandex.ru and Lazarus team
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

unit RxDBGridPrintGrid_SetupUnit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, ExtCtrls;

type

  { TRxDBGridPrintGrid_SetupForm }

  TRxDBGridPrintGrid_SetupForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckGroup1: TCheckGroup;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RadioGroup1: TRadioGroup;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation
uses rxdconst;

{$R *.lfm}

{ TRxDBGridPrintGrid_SetupForm }

procedure TRxDBGridPrintGrid_SetupForm.FormCreate(Sender: TObject);
begin
  CheckBox1.Caption:=sShowColumnHeaderOnAllPage;
  CheckGroup1.Caption:=sPrintOptions;
  CheckGroup1.Items[0]:=sShowTitle;
  CheckGroup1.Items[1]:=sShowFooter;
  CheckGroup1.Items[2]:=sShowFooterColor;
  CheckGroup1.Items[3]:=sShowGridColor;
  CheckGroup1.Items[4]:=sShowReportTitle;
  CheckGroup1.Items[5]:=sHideZeroValues;


  GroupBox1.Caption:=sPageMargins;
  Label1.Caption:=sTopCaption;
  Label2.Caption:=sLeftCaption;
  Label3.Caption:=sRightCaption;
  Label4.Caption:=sBottomCaption;
  Label6.Caption:=sReportTitle;

  RadioGroup1.Caption:=sOrientation;
  RadioGroup1.Items[0]:=sPortrait;
  RadioGroup1.Items[1]:=sLandscape;

end;

end.

