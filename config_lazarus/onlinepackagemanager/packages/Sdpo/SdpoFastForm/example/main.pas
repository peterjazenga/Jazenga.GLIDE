unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SdpoFastForm, ExtCtrls, LCLIntf;

const
  ImgW = 255;
  ImgH = 255;

type
  TRGB32=record
    blue,green,red,alpha: byte;
  end;
  pTRGB32=^TRGB32;

  TProcImg = array[0..ImgH-1,0..ImgW-1] of TRGB32;
  pTProcImg = ^TProcImg;

type

  { TFMain }

  TFMain = class(TForm)
    CBCanvas: TCheckBox;
    EditMouseMove: TEdit;
    EditMouseDown: TEdit;
    EditTime: TEdit;
    EditText: TEdit;
    SdpoFastForm: TSdpoFastForm;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure SdpoFastFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SdpoFastFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SdpoFastFormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CurColor: byte;
    TextPos: byte;
    FirstTimeValSec: LongInt;
    PrevTime: LongWord;
  end; 

var
  FMain: TFMain;

implementation

{ TFMain }

procedure TFMain.FormShow(Sender: TObject);
begin
  SdpoFastForm.Show;
  Timer.Enabled:=true;
  CurColor:=0;
  TextPos:=0;

  SdpoFastForm.Canvas.Pen.Width:=2;
  
  SdpoFastForm.Canvas.Pen.Color:=clBlue;
  SdpoFastForm.Canvas.Font.Color:=clBlue;
  SdpoFastForm.Canvas.Brush.Style:=bsClear;
end;

procedure TFMain.SdpoFastFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caNone;
end;

procedure TFMain.SdpoFastFormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var txt: string;
begin
  txt := Format('%.3d,%.3d ',[X,Y]);

  if (ssLeft in Shift) then
    txt := txt + 'L'
  else if (ssRight in Shift) then
    txt := txt + 'R';

  EditMouseDown.Text := txt;
end;

procedure TFMain.SdpoFastFormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  EditMouseMove.Text := Format('%.3d,%.3d ',[X,Y]);
end;

procedure TFMain.TimerTimer(Sender: TObject);
var
  //pImage: PLongWord;
  pImage: pTProcImg;
  ImgPos: integer;
  PaintColor: byte;
  txt: string;
  time: LongWord;
  y,x: integer;
begin
  time := GetTickCount;

  txt := inttostr(time-PrevTime)+' ';
  PrevTime:=time;
  PaintColor:=CurColor;

  //pImage:=PLongWord(SdpoFastForm.Data);
  //for y:=0 to SdpoFastForm.Height-1 do begin
  //  for x:=0 to SdpoFastForm.Width-1 do begin
  //    FillChar(pImage^, 4, PaintColor);
  //    inc(pImage);
  //  end;
  //  inc(PaintColor);
  //end;

  pImage := pTProcImg(SdpoFastForm.Data);
  for y := 0 to ImgH - 1 do begin
    for x := 0 to ImgW - 1 do begin
      pImage^[y,x].red := PaintColor;
      pImage^[y,x].green := PaintColor;
      pImage^[y,x].blue := PaintColor;
    end;
    inc(PaintColor);
  end;
  
  inc(CurColor);
  
  if CBCanvas.Checked then begin
    SdpoFastForm.DrawToPixmap;
    
    SdpoFastForm.Canvas.TextOut(TextPos,SdpoFastForm.Height div 2,EditText.Text);
    
    inc(TextPos);
    SdpoFastForm.Canvas.Rectangle(Bounds(5,5,SdpoFastForm.Width-2*5,SdpoFastForm.Height-2*5));

    SdpoFastForm.PaintFromPixmap;
  end else
    SdpoFastForm.Paint;

  SdpoFastForm.Sync;
  
  EditTime.Text:=txt+'paint:'+inttostr(GetTickCount-time);
end;

initialization
  {$I main.lrs}

end.

