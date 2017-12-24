{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Chart,
  Buttons, ExtCtrls, StdCtrls, RingChart, AnalogWatch, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    BarChart1: TBarChart;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RingChart1: TRingChart;
    AnalogWatch1: TAnalogWatch;
    SpinEditHH: TSpinEdit;
    SpinEditMM: TSpinEdit;
    SpinEditSS: TSpinEdit;
    procedure AnalogWatch1Appointment(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure SpinEditHHChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 
  i:integer=0;
implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var uno,due,tre,quattro:integer;
begin
  uno:=Random(100);
  due:=Random(100);
  tre:=Random(100);
  quattro:=Random(100);
  i:=i+1;
  BarChart1.Clear;
  BarChart1.LabelPosition:=TPosLabel(i mod 3);
  BarChart1.AddBar('Red',uno,clRed);
  BarChart1.AddBar('Fuchsia',due,clFuchsia);
  BarChart1.AddBar('Green',tre,clGreen);
  BarChart1.AddBar('Yellow',quattro,clYellow);
  BarChart1.AddBar('Aqua',tre,clAqua);
  BarChart1.AddBar('Cream',quattro,clCream);

  RingChart1.Clear;
  RingChart1.AddSector('Red',uno,clRed);
  RingChart1.AddSector('Fuchsia',due,clFuchsia);
  RingChart1.AddSector('Green',tre,clGreen);
  RingChart1.AddSector('Yellow',quattro,clYellow);
  RingChart1.AddSector('Aqua',tre,clAqua);
  RingChart1.AddSector('Cream',quattro,clCream);
end;

procedure TForm1.AnalogWatch1Appointment(Sender: TObject);
begin
  ShowMessage('CooCoo!!!!'+LineEnding+AnalogWatch1.MeetTime);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RingChart1.ChangeSector(1,'Light Gray', 12.5, clLtGray);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RingChart1.DeleteSector(2);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  RingChart1.Clear;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  RingChart1.Ring:=CheckBox1.Checked;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  RingChart1.Legend:=TLegendPosition(RadioGroup1.ItemIndex);
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  RingChart1.KindofLabel:=TKindLabel(RadioGroup2.ItemIndex);
end;

procedure TForm1.SpinEditHHChange(Sender: TObject);
begin
  AnalogWatch1.MeetTime:=FormatFloat('00',SpinEditHH.Value)+TimeSeparator+FormatFloat('00',SpinEditMM.Value)+TimeSeparator+FormatFloat('00',SpinEditSS.Value);
  Panel2.Caption:=AnalogWatch1.MeetTime;
end;

initialization
  {$I unit1.lrs}

end.

