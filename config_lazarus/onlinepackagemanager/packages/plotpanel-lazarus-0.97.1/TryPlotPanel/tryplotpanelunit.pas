unit TryPlotPanelUnit;
{
 *  Copyright (C) 2006  Marien van Westen  (m.c.van.westen@pl.hanze.nl)
 *
 *  You are free to use and modify this code under the terms of
 *  the GNU General Public Licence version 2 or later.
 *
}

{$mode objfpc}{$H+}

interface

uses
  LResources, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Spin, Math, Plotpanel, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    btXLabel: TButton;
    btYLabel: TButton;
    ClrRed: TButton;
    ClrGreen: TButton;
    Colorbox: TCheckBox;
    Days: TCheckBox;
    Percent: TCheckBox;
    Cosine: TButton;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    ScrollBarXMin: TTrackBar;
    MarginSpin: TSpinEdit;
    Start: TButton;
    Stop: TButton;
    StopChart: TButton;
    StartChart: TButton;
    GroupBox1: TGroupBox;
    Lissajous1: TButton;
    PageControl1: TPageControl;
    PlotModeAll: TRadioGroup;
    SimplePlot: TTabSheet;
    Advanced: TTabSheet;
    ChartTimer: TTimer;
    Marks: TTabSheet;
    TimerPlot: TTabSheet;
    Parabool: TButton;
    Sinus: TButton;
    Sinus2: TButton;
    Parabool2: TButton;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    btClear: TButton;
    btShowMarks: TButton;
    btAxisColor: TButton;
    btBackColor: TButton;
    btAutoScale: TButton;
    Title: TButton;
    ColorDialog1: TColorDialog;
    HideRed: TButton;
    HideGreen: TButton;
    ScrollBarXmax: TTrackBar;
    XLabel: TLabel;
    YLabel: TLabel;
    TestLabel: TLabel;
    PenWidth: TSpinEdit;
    SaveDialog1: TSaveDialog;
    SaveBitmap: TButton;
    TabSheet1: TTabSheet;
    DrawGraph: TButton;
    CheckBox1: TCheckBox;
    AxisSpin: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    XMinEdit: TEdit;
    XMaxEdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Ylog: TCheckBox;
    AutoScaleRed: TButton;
    AutoScaleGreen: TButton;
    procedure btXLabelClick(Sender: TObject);
    procedure btYLabelClick(Sender: TObject);
    procedure ChartTimerTimer(Sender: TObject);
    procedure ClrGreenClick(Sender: TObject);
    procedure ClrRedClick(Sender: TObject);
    procedure CosineClick(Sender: TObject);
    procedure CreatePanelClick(Sender: TObject);
    procedure MarginSpinChange(Sender: TObject);
    procedure Parabool2Click(Sender: TObject);
    procedure PlotPanel1MouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SinusClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure ParaboolClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Sinus2Click(Sender: TObject);
    procedure PlotModeClick(Sender: TObject);
    procedure PenWidthClick(Sender: TObject);
    procedure btAutoScaleClick(Sender: TObject);
    procedure btShowMarksClick(Sender: TObject);
    procedure btAxisColorClick(Sender: TObject);
    procedure btBackColorClick(Sender: TObject);
    procedure LissajousClick(Sender: TObject);
    procedure StartChartClick(Sender: TObject);
    procedure StopChartClick(Sender: TObject);
    procedure TitleClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure HideRedClick(Sender: TObject);
    procedure HideGreenClick(Sender: TObject);
    procedure PlotPanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PenWidthChanged(Sender: TObject; NewValue: Integer);
    procedure SaveBitmapClick(Sender: TObject);
    procedure DrawGraphClick(Sender: TObject);
    procedure AxisSpinChange(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure AutoScaleRedClick(Sender: TObject);
    procedure AutoScaleGreenClick(Sender: TObject);
    procedure PlotPanel1YMarksWrite(Sender: TCustomPanel; Value: extended;
  var AxisText: String; var AFont: TFont; var EventHandled: boolean);
    procedure PlotPanel1XMarksWrite(Sender: TCustomPanel; Value: extended;
  var AxisText: String; var AFont: TFont; var EventHandled: boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1 : TForm1;
  PlotPanel1: TPlotpanel;
  PanelWidth, PanelHeight : Integer;
  But : Integer;
  max, tel : Integer;
  rij : Array [0 .. 150] of real;

implementation

//////////////////////////////////////////////////////////////////////
//
//      General Procedures
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.CreatePanelClick(Sender: TObject);
begin
 PlotPanel1:=Tplotpanel.Create(Self);
 PlotPanel1.Top:=8;
 PlotPanel1.Left:=8;
 Plotpanel1.Width:=462;
 PlotPanel1.Height:=462;
 PlotPanel1.Margin:=MarginSpin.Value;
 PlotPanel1.Anchors:=[akTop,akLeft,akBottom,akRight];
 PlotPanel1.OnMouseMove:=@PlotPanel1MouseMove;
 PlotPanel1.OnXMarksWrite:=@PlotPanel1XMarksWrite;
 PlotPanel1.OnYMarksWrite:=@PlotPanel1YMarksWrite;
 PanelWidth:=PlotPanel1.Width;
 PanelHeight:=PlotPanel1.Height;
 PlotPanel1.PlotPen.Width:=2;
 PenWidth.Value:= PlotPanel1.PlotPen.Width;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  CreatePanelClick(nil);
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  Timer1.Enabled:=False;
  With PlotPanel1 do
  begin
  Freeze(True);
  XMax:=10;
  XMin:=0;
  YMax:=10;
  YMin:=00;
  Color:=clBtnFace;
  BackColor:=clWhite;
  GridColor:=clBlack;
  XMarksFont.Color:=clBlack;
  XInterval:=1;
  YInterval:=1;
  YMarks:=False;
  XMarks:=False;
  btShowMarks.Caption:='Show Marks';
  Title:='';
  LayerOption:=False;
  PlotPen.Width:=2;
  PlotMode:=pmLine;
  ClearData;
  PenWidth.Value:=2;
  Freeze(False);
  end;
end;

//////////////////////////////////////////////////////////////////////
//
//      Bottom Panel
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.btClearClick(Sender: TObject);
begin
   PlotPanel1.Freeze(True);
   PlotPanel1.XMarksFont.Color:=clBlack;
   PlotPanel1.YMarksFont.Color:=clBlack;
   PlotPanel1.Color:=clBtnFace;
   PlotPanel1.BackColor:=clWhite;
   PlotPanel1.GridColor:=clBlack;
   PlotPanel1.XMin:=0;
   PlotPanel1.XMax:=10;
   PlotPanel1.YMin:=0;
   PlotPanel1.YMax:=10;
   PlotPanel1.XInterval:=1;
   PlotPanel1.YInterval:=1;
   PlotPanel1.ClearData;
   PlotPanel1.Freeze(False);
end;

procedure TForm1.TitleClick(Sender: TObject);
begin
 PlotPanel1.Title:=Inputbox('PlotPanel Test','What is the Title', 'PlotPanel');
 if Length(PlotPanel1.Title)<2 then PlotPanel1.Title:='';
end;

procedure TForm1.btXLabelClick(Sender: TObject);
begin
  PlotPanel1.XLabel:=Inputbox('PlotPanel Test','What is the X-Axis Label', 'X-axis');
  if Length(PlotPanel1.XLabel)<2 then PlotPanel1.XLabel:='';
end;

procedure TForm1.btYLabelClick(Sender: TObject);
begin
  PlotPanel1.YLabel:=Inputbox('PlotPanel Test','What is the Y-Axis Label', 'Y-axis');
  if Length(PlotPanel1.YLabel)<2 then PlotPanel1.YLabel:='';
end;

procedure TForm1.btAutoScaleClick(Sender: TObject);
begin
 PlotPanel1.AutoScale(0);
end;

procedure TForm1.MarginSpinChange(Sender: TObject);
begin
  Plotpanel1.Margin:=MarginSpin.Value;
end;

procedure TForm1.PenWidthClick(Sender: TObject);
begin
  PlotPanel1.PlotPen.Width:=PenWidth.Value;
end;

procedure TForm1.btShowMarksClick(Sender: TObject);
begin
   if PlotPanel1.XMarks=True then
     begin
     PlotPanel1.XMarks:=False;
     PlotPanel1.YMarks:=False;
     btShowMarks.Caption:='Show Marks';
     end
   else
     begin
     PlotPanel1.XMarks:=True;
     PlotPanel1.YMarks:=True;
     btShowMarks.Caption:='Hide Marks';
   end;
end;

procedure TForm1.btAxisColorClick(Sender: TObject);
begin
 ColorDialog1.Color:=PlotPanel1.GridColor;
 if ColorDialog1.Execute then PlotPanel1.GridColor := ColorDialog1.Color;
end;

procedure TForm1.btBackColorClick(Sender: TObject);
begin
 ColorDialog1.Color:=PlotPanel1.BackColor;
 if ColorDialog1.Execute then PlotPanel1.BackColor := ColorDialog1.Color;
end;

procedure TForm1.AxisSpinChange(Sender: TObject);
begin
   if AxisSpin.Value  < 8 then
     begin
     PlotPanel1.Font.Name:='Small Fonts';
     PlotPanel1.XMarksFont.Name:='Small Fonts';
     PlotPanel1.YMarksFont.Name:='Small Fonts';
     end
     else
     begin
     PlotPanel1.Font.Name:='MS Sans Serif';
     PlotPanel1.XMarksFont.Name:='MS Sans Serif';
     PlotPanel1.YMarksFont.Name:='MS Sans Serif';
     end;
   PlotPanel1.Font.Size:=AxisSpin.Value;
   PlotPanel1.XMarksFont.Size:=AxisSpin.Value;
   PlotPanel1.YMarksFont.Size:=AxisSpin.Value;
end;


 procedure TForm1.PenWidthChanged(Sender: TObject; NewValue: Integer);
begin
 PlotPanel1.PlotPen.Width:=PenWidth.Value;
end;


procedure TForm1.SaveBitmapClick(Sender: TObject);
var plot : TBitmap;
begin
  plot := PlotPanel1.PlotBMP;
  if SaveDialog1.Execute then
    plot.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.PlotModeClick(Sender: TObject);
begin
   if PlotModeall.ItemIndex=0 then PlotPanel1.PlotMode:=pmDot;
   if PlotModeAll.ItemIndex=1 then PlotPanel1.PlotMode:=pmLine;
   if PlotModeAll.ItemIndex=2 then PlotPanel1.PlotMode:=pmBar;
end;

//////////////////////////////////////////////////////////////////////
//
//      Simple Plot Tab
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.ParaboolClick(Sender: TObject);
var
  i : Integer;
begin
   PlotPanel1.Freeze(True);
   PlotPanel1.XMin:=-10;
   PlotPanel1.XMax:=10;
   PlotPanel1.YMin:=0;
   PlotPanel1.YMax:=100;
   PlotPanel1.XInterval:=2;
   PlotPanel1.YInterval:=10;
   PlotPanel1.ClearData;
   PlotPanel1.PlotPen.Color:=clRed;
   for i:= -10 to +10 do
     PlotPanel1.AddXY(i, i*i);
   PlotPanel1.Freeze(False);
end;

procedure TForm1.SinusClick(Sender: TObject);
var i:Integer;
    y:Real;
begin
   PlotPanel1.Freeze(True);
   PlotPanel1.XMin:=0;
   PlotPanel1.XMax:=50;
   PlotPanel1.YMin:=-40;
   PlotPanel1.YMax:=40;
   PlotPanel1.XInterval:=5;
   PlotPanel1.YInterval:=5;
   PlotPanel1.ClearData;
   for i:=0 to 50 do
     begin
       y:=30 * sin(2*PI*i/50);
       PlotPanel1.AddXY(i,y,clRed,0);
     end;
   PlotPanel1.Freeze(False);
end;

procedure TForm1.LissajousClick(Sender: TObject);
var t:Integer;
     rd,kx1,ky1,py,fx1,fy1:Real;
begin
   rd:= Pi/360;
   fx1:=2;
   fy1:=3;
   py:=10;
   kx1:=7.0;
   ky1:=6.2;
   PlotPanel1.ClearData;
   PlotPanel1.XScaleLog:=False;
   PlotPanel1.YScaleLog:=False;
   PlotPanel1.Freeze(true);
   PlotPanel1.LayerOption:=False;
   PlotPanel1.PlotPen.Color:=clBlue;
      for t:=0 to 720 do
      begin
         PlotPanel1.AddXY(kx1*sin(fx1*t*rd),ky1*sin(fy1*t*rd+py*rd*2));
      end;
   PlotPanel1.Freeze(False);
end;


//////////////////////////////////////////////////////////////////////
//
//      Advanced  Tab
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.Parabool2Click(Sender: TObject);
var
  i : Integer;
begin
   PlotPanel1.Freeze(True);
   PlotPanel1.XMin:=-8;
   PlotPanel1.XMax:=12;
   PlotPanel1.YMin:=-20;
   PlotPanel1.YMax:=80;
   PlotPanel1.XInterval:=2;
   PlotPanel1.YInterval:=10;
   PlotPanel1.ClearData;
   PlotPanel1.LayerOptions(0,pmLine,2);
   PlotPanel1.LayerOptions(1,pmLine,2);
   for i:= -10 to +10 do
     begin
     PlotPanel1.AddXY(i, i*i,clRed,0);
     PlotPanel1.AddXY(i+4, i*i+i*3,clLime,1);
     end;
  PlotPanel1.Freeze(False);
end;


procedure TForm1.PlotPanel1MouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;


procedure TForm1.Sinus2Click(Sender: TObject);
var
   i : Integer;
   y: Real;
begin
   PlotPanel1.Freeze(True);
   But:=5 ;
   PlotPanel1.ClearData;
   PlotPanel1.XMin:=10;
   PlotPanel1.XMax:=55;
   PlotPanel1.YMin:=-30;
   PlotPanel1.YMax:=40;
   PlotPanel1.XInterval:=5;
   PlotPanel1.YInterval:=5;
   PlotPanel1.ClearData;
   PlotPanel1.LayerOptions(0,pmDot,4);
   PlotPanel1.LayerOptions(1,pmLine,2);
   for i:=0 to 50 do
     begin
       y:=30 * sin(2*PI*i/50);
       PlotPanel1.AddXY(i,y/2,clRed,0);
       PlotPanel1.AddXY(i,-y,clLime,1);
     end;
   PlotPanel1.Freeze(False);
end;


procedure TForm1.PlotPanel1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  WX, WY : Extended;
  Test : Boolean;
begin
  Test:=PlotPanel1.ConvertS2W(X,Y,WX,WY);
  if Test=True then TestLabel.Caption:='Inside is True'
               else TestLabel.Caption:='Inside is False' ;
  XLabel.Caption:='X = '+FloatToStr(WX);
  YLabel.Caption:='Y = '+FloatToStr(WY);
end;

procedure TForm1.ClrRedClick(Sender: TObject);
begin
  PlotPanel1.ClearData(0);
end;

procedure TForm1.ClrGreenClick(Sender: TObject);
begin
  PlotPanel1.ClearData(1);
end;

procedure TForm1.HideRedClick(Sender: TObject);
begin
  HideRed.Tag:=1-HideRed.Tag;
  if HideRed.Tag=1 then PlotPanel1.HideLayer(0)
                   else PlotPanel1.UnHideLayer(0);
end;

procedure TForm1.HideGreenClick(Sender: TObject);
begin
  Hidegreen.Tag:=1-HideGreen.Tag;
  if HideGreen.Tag=1 then PlotPanel1.HideLayer(1)
                     else PlotPanel1.UnHideLayer(1);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TestLabel.Caption:='Inside is ----' ;
  XLabel.Caption:='X = ----';
  YLabel.Caption:='Y = ----';
end;

procedure TForm1.AutoScaleRedClick(Sender: TObject);
begin
 PlotPanel1.AutoScale(0);
end;

procedure TForm1.AutoScaleGreenClick(Sender: TObject);
begin
  PlotPanel1.AutoScale(1);
end;

//////////////////////////////////////////////////////////////////////
//
//      TimerPlot Tab    (Standing wave)
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
var
   i :Integer;
   y,u : Double;
begin
  u:=30*sin(2*PI*Start.Tag/60);
  Start.Tag:=Start.Tag+1;
  if Start.Tag >=60 then Start.Tag:=0;
  PlotPanel1.Freeze(true);
  PlotPanel1.ClearData;
  PlotPanel1.AddXY(0,0,clRed,1);
    for i:=1 to 60 do
    begin
      y:=u*sin(2*Pi*i/21.6);
      PlotPanel1.AddXY(i,y,clRed,1);
    end;
  PlotPanel1.AddXY(0,0,clBlue,2);
  for i:=1 to 60 do
    begin
      y:=u*sin(2*Pi*i/21.6);
      PlotPanel1.AddXY(i,y,clBlue,2);
    end;
  PlotPanel1.Freeze(false);
end;

procedure TForm1.StartClick(Sender: TObject);
begin
  ChartTimer.Enabled:=False;
  With PlotPanel1 do
  begin
  Freeze(True);
  XMax:=60;
  XMin:=0;
  YMax:=30;
  YMin:=-30;
  Color:=clBlack;
  BackColor:=clBlack;
  GridColor:=clOlive;
  XMarksFont.Color:=clWhite;
  XInterval:=6;
  YInterval:=6;
  YMarks:=False;
  XMarks:=True;
  Title:='';
  LayerOptions(2,pmDot,10);
  LayerOptions(1,pmLine,1);
  Freeze(False);
  end;
  Timer1.Enabled:=True;
end;

procedure TForm1.StopClick(Sender: TObject);
begin
 Timer1.Enabled:=False;
end;

//////////////////////////////////////////////////////////////////////
//
//      TimerPlot Tab   (Moving Chart)
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.StartChartClick(Sender: TObject);
var
 i: Integer;
begin
  Timer1.Enabled:=False;
  max:=100;
  for i:=0 to max do
     Rij[i]:=0;
  btClearClick(nil);
  With PlotPanel1 do
    begin
    XMin:=0;
    Xmax:=max;
    Xinterval:=max/5;
    YMax:=2;
    YMin:=-2;
    Yinterval :=5;
    BackColor:=clBlack;
    Color:=clActiveBorder;
    GridColor:=clGreen;
    PlotPen.Color:=clRed;
    PlotPen.Width:=2;
    PlotMode:=pmLine;
    end;
  tel:=0;
  ChartTimer.Enabled:=True;
end;

procedure TForm1.StopChartClick(Sender: TObject);
begin
  ChartTimer.Enabled:=False;
end;

procedure TForm1.ChartTimerTimer(Sender: TObject);
var
 i,j : Integer;
begin
   Rij[tel mod max]:= sin(2*PI*tel/20)+(Random(10)-5)/10;
   inc(tel);
   With PlotPanel1 do
   begin
     Freeze(True);
     ClearData;
     if tel>=max then begin Xmax:=tel; Xmin:=tel-max; end;
     j:= tel mod max;
     for i:= 0 to max-1 do
        begin
        AddXY(Xmin+i,rij[(j+i) mod max]);
        end;
     Freeze(False);
   end;
end;


//////////////////////////////////////////////////////////////////////
//
//      LogScale Tab
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.DrawGraphClick(Sender: TObject);
var
  i : Integer;
begin
   PlotPanel1.Freeze(True);
   Plotpanel1.ClearData;
   PlotPanel1.PlotMode:=pmLine;
   PlotPanel1.XMin:=Power(10,ScrollbarXMin.Position/10);
   PlotPanel1.XMax:=Power(10,ScrollbarXMax.Position/10);
   PlotPanel1.YScaleLog:=Ylog.Checked;
   PlotPanel1.YMin:=1;
   PlotPanel1.YMax:=1000;
   PlotPanel1.XInterval:=100;
   PlotPanel1.YInterval:=100;
   if Checkbox1.Checked=True Then Plotpanel1.XScaleLog:=True else Plotpanel1.XScaleLog:=False;
   if Plotpanel1.XScaleLog=False then
      PlotPanel1.XMin:=0;
   PlotPanel1.PlotPen.Color:=clRed;
   for i:= 1 to 1000 do
     PlotPanel1.AddXY(i*2, i);
   PlotPanel1.Freeze(False);
end;

procedure TForm1.ScrollBarChange(Sender: TObject);
begin
 XMinEdit.Text:=FloatToStr(ScrollbarXMin.Position/10);
 XMaxEdit.Text:=FloatToStr(ScrollbarXMin.Position/10+ScrollbarXMax.Position/10);
 PlotPanel1.XMin:=Power(10,ScrollbarXMin.Position/10);
 PlotPanel1.XMax:=PlotPanel1.XMin+Power(10,ScrollbarXMax.Position/10);
end;

//////////////////////////////////////////////////////////////////////
//
//      OnXMarksWrite, OnYMarksWrite
//      Thanks to Jorge Solla
//
/////////////////////////////////////////////////////////////////////

procedure TForm1.CosineClick(Sender: TObject);
var i:Integer;
    y:Real;
begin
   PlotPanel1.Freeze(True);
   PlotPanel1.XMin:=0;
   PlotPanel1.XMax:=10;
   PlotPanel1.YMin:=-60;
   PlotPanel1.YMax:=60;
   PlotPanel1.XInterval:=1;
   PlotPanel1.YInterval:=10;
   PlotPanel1.XMarks:=True;
   PlotPanel1.YMarks:=True;
   PlotPanel1.YMarksFont.Color:=clBlack;
   PlotPanel1.ClearData;
   for i:=0 to 50 do
     begin
       y:=50 * cos(2*PI*i/50);
       PlotPanel1.AddXY(i/5,y,clRed,0);
     end;
   PlotPanel1.Freeze(False);
end;

procedure TForm1.PlotPanel1YMarksWrite(Sender: TCustomPanel; Value: extended;
  var AxisText: String; var AFont: TFont; var EventHandled: boolean);
var
  MyFont : TFont;
begin

   if (Percent.Checked=False) and (Colorbox.Checked=False) then exit;
   AxisText:=FloattoStr(Value);
   MyFont:=TFont.Create;
   MyFont.Assign(AFont);
   if Colorbox.Checked then
      begin
      if (value=0)
         then MyFont.Color:=clBlack
         else
            if (Value<0)
               then MyFont.Color:=clRed
               else MyFont.Color:=clBlue;
      end;
    if Percent.Checked then AxisText:=FloatToStr(Value*2)+' %';
    AFont.Assign(MyFont);
    EventHandled:=True;
    MyFont.Free;
end;

procedure TForm1.PlotPanel1XMarksWrite(Sender: TCustomPanel; Value: extended;
  var AxisText: String; var AFont: TFont; var EventHandled: boolean);
var
  D : Integer;
begin
  if Days.Checked=False then exit;
  D := Round(Value);
  D :=D mod 7;
  Case D of
  0 : AxisText:='Sun';
  1 : AxisText:='Mon';
  2 : AxisText:='Tue';
  3 : AxisText:='Wed';
  4 : AxisText:='Thu';
  5 : AxisText:='Fri';
  6 : AxisText:='Sat';
  end;
  EventHandled:=True;
end;

initialization
  {$I tryplotpanelunit.lrs}
end.

