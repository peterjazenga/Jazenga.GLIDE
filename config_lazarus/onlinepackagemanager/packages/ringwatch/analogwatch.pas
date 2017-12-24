{
 /***************************************************************************
                               AnalogWatch.pas
                               --------
                 Component Library Extended Controls

 ***************************************************************************/

 *****************************************************************************
 * These files are distributed under the Library GNU General Public License  *
 * (see the file COPYING.LPGL) with the following modification:              *
 *                                                                           *
 * As a special exception, the copyright holders of this library give you    *
 * permission to link this library with independent modules to produce an    *
 * executable, regardless of the license terms of these independent modules, *
 * and to copy and distribute the resulting executable under terms of your   *
 * choice, provided that you also meet, for each linked independent module,  *
 * the terms and conditions of the license of that module. An independent    *
 * module is a module which is not derived from or based on this library. If *
 * you modify this library, you may extend this exception to your version of *
 * the library, but you are not obligated to do so. If you do not wish to do *
 * so, delete this exception statement from your version.                    *
 *                                                                           *
 * If you didn't receive a copy of the file COPYING.LGPL, contact:           *
 *     Free Software Foundation, Inc.,                                       *
 *     675 Mass Ave                                                          *
 *     Cambridge, MA  02139                                                  *
 *     USA                                                                   *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Salvatore Coppola
}

unit AnalogWatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, LCLType, Controls, ExtCtrls, Graphics,
  Dialogs, LResources, GraphMath, TimeEditor, PropEdits;

const
  HoursHand:   array[1..7,1..2]of integer =((-4,0),(-4,1),(-4,1),(0,1),(4,1),(4,1),(4,0));
  MinutesHand: array[1..7,1..2]of integer =((-4,0),(-4,1),(-4,1),(0,1),(4,1),(4,1),(4,0));
  SecondsHand: array[1..7,1..2]of integer =((-2,0),(-2,1),(-2,1),(0,1),(2,1),(2,1),(2,0));

type

  WatchMode = (wmClock, wmStopwatch, wmDefined);

  { TCustomAnalogWatch }

  TCustomAnalogWatch = class(TGraphicControl)
  private
    bx,by:integer;
    a,b:integer;//ellipse radius x^2/a^2+y^2/a^2=1
    HHOLD,MMOLD,SSOLD: Word;
    FTimer: TTimer;
    FDepth: byte;
    FWholeRepaint:boolean;
    FColorFace: TColor;
    FColorHoursHand: TColor;
    FColorMinutesHand: TColor;
    FColorSecondsHand: TColor;
    FColorThicks: TColor;
    FDisplayDigits : boolean;
    FDisplayTicks : boolean;
    FMeetTime: string;
    FAppointment: TNotifyEvent;
    FWatchMode : WatchMode;
    FStopwatchRunning : boolean;
    FStopwatchStartTime : TDateTime;
    FDefinedTime : string;
    FDisplayHours : boolean;
    FDisplayMinutes : boolean;
    FDisplaySeconds : boolean;
    procedure SetDepth(Value:byte);
    procedure SetColorFace(Value:TColor);
    procedure SetColorThicks(Value:TColor);
    procedure SetMeetTime(Value:string);
    procedure SetWatchMode(Value:WatchMode);
    procedure SetStopwatchRunning(Value:boolean);
    procedure SetDefinedTime(Value:string);
    procedure SetDisplayDigits(Value:boolean);
    procedure SetDisplayTicks(Value:boolean);
    function  GetInterval : Cardinal;
    procedure SetInterval (Value:Cardinal);
    procedure SetDisplayHours(Value:boolean);
    procedure SetDisplayMinutes(Value:boolean);
    procedure SetDisplaySeconds(Value:boolean);
  protected
    procedure PaintWatch;
    procedure ResizeWatch;
    procedure Paint; override;
    procedure WatchOnTimer(Sender: TObject);
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StopwatchRunning : boolean read FStopwatchRunning write SetStopwatchRunning;
  published
    property Depth: byte read FDepth write SetDepth default 6;
    property WholeRepaint: boolean read FWholeRepaint write FWholeRepaint default false;
    property ColorFace:TColor read FColorFace write SetColorFace default clCream;
    property ColorHoursHand:TColor read FColorHoursHand write FColorHoursHand default clGreen;
    property ColorMinutesHand:TColor read FColorMinutesHand write FColorMinutesHand default clRed;
    property ColorSecondsHand:TColor read FColorSecondsHand write FColorSecondsHand default clBlack;
    property ColorThicks:TColor read FColorThicks write SetColorThicks default clBlack;
    property DisplayDigits:boolean read FDisplayDigits write SetDisplayDigits default true;
    property DisplayTicks:boolean read FDisplayTicks write SetDisplayTicks default true;
    property DisplayHours:boolean read FDisplayHours write SetDisplayHours default true;
    property DisplayMinutes:boolean read FDisplayMinutes write SetDisplayMinutes default true;
    property DisplaySeconds:boolean read FDisplaySeconds write SetDisplaySeconds default true;
    property Interval:Cardinal read GetInterval write SetInterval;
    property Mode:WatchMode read FWatchMode write SetWatchMode default wmClock;
    property DefinedTime : string read FDefinedTime write SetDefinedTime;
    property MeetTime: string read FMeetTime write SetMeetTime;
    property OnAppointment:TNotifyEvent read FAppointment write FAppointment;
  end;

  { TAnalogWatch }

  TAnalogWatch = class(TCustomAnalogWatch)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DragMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
  
procedure Register;

implementation

uses Ellipse;

procedure Register;
begin
  RegisterComponents('Misc',[TAnalogWatch]);
  RegisterPropertyEditor(TypeInfo(string), TCustomAnalogWatch, 'MeetTime',
                         TMeetTimeProperty);
  RegisterPropertyEditor(TypeInfo(string), TCustomAnalogWatch, 'DefinedTime',
                         TMeetTimeProperty);
end;

procedure TimeToAlpha(hh,mm,ss:Word; var alfaH,alfaM,alfaS:double);
var dh:double;
begin
  alfaS:=(90-6*ss+360)*pi/180;
  alfaM:=(90-6*mm+360)*pi/180;
  dh:=hh+mm/60;
  if dh>12 then
    dh:=dh-12;
  alfaH:=(90-30*dh+360)*pi/180;
end;

procedure TCustomAnalogWatch.WatchOnTimer(Sender: TObject);
var HH,MM,SS,MS: Word;
    alphaH,alphaM,alphaS:double;
    rotateSecondsHand,rotateMinutesHand,rotateHoursHand:array[1..7]of TPoint;
    i:integer;
    amin:integer;
    p0:TPoint;
    oldPenWidth:integer;
    StartPoint,EndPoint:TFloatPoint;
    strhour:string;
    dx,dy:double;
begin
  if not Visible then exit;

  p0.x:=Width div 2;
  p0.y:=(Height-FDepth) div 2;

  amin:=a;
  if b<a then
    amin:=b;
  if HHOLD<>25 then begin
    if FWholeRepaint then
      PaintWatch
    else begin
      TimeToAlpha(HHOLD,MMOLD,SSOLD,alphaH,alphaM,alphaS);
      Canvas.Pen.Color:=ColorFace;
      oldPenWidth:=Canvas.Pen.Width;
      Canvas.Pen.Width:=2;

      for i:=1 to 7 do begin
        case i of
          2,3,5,6:begin
            rotateHoursHand[i].x:=round(p0.x-HoursHand[i,1]*sin(alphaH)+HoursHand[i,2]*0.2*amin*cos(alphaH));
            rotateHoursHand[i].y:=round(p0.y-HoursHand[i,1]*cos(alphaH)-HoursHand[i,2]*0.2*amin*sin(alphaH));
          end;
          4:begin
            EndPoint:=EllipsePoint(p0,a-a div 2,b-b div 2,alphaH);
            rotateHoursHand[i].x:=round(EndPoint.X);
            rotateHoursHand[i].y:=round(EndPoint.Y);
          end;
          1,7:begin
            rotateHoursHand[i].x:=round(p0.x-HoursHand[i,1]*sin(alphaH)+HoursHand[i,2]*cos(alphaH));
            rotateHoursHand[i].y:=round(p0.y-HoursHand[i,1]*cos(alphaH)-HoursHand[i,2]*sin(alphaH));
          end;
        end;
      end;
      if FDisplayHours then Canvas.Polygon(rotateHoursHand);

      for i:=1 to 7 do begin
        case i of
          2,3,5,6:begin
            rotateMinutesHand[i].x:=round(p0.x-MinutesHand[i,1]*sin(alphaM)+MinutesHand[i,2]*0.5*amin*cos(alphaM));
            rotateMinutesHand[i].y:=round(p0.y-MinutesHand[i,1]*cos(alphaM)-MinutesHand[i,2]*0.5*amin*sin(alphaM));
          end;
          4:begin
            EndPoint:=EllipsePoint(p0,a-a div 5,b-b div 5,alphaM);
            rotateMinutesHand[i].x:=round(EndPoint.X);
            rotateMinutesHand[i].y:=round(EndPoint.Y);
          end;
          1,7:begin
            rotateMinutesHand[i].x:=round(p0.x-MinutesHand[i,1]*sin(alphaM)+MinutesHand[i,2]*cos(alphaM));
            rotateMinutesHand[i].y:=round(p0.y-MinutesHand[i,1]*cos(alphaM)-MinutesHand[i,2]*sin(alphaM));
          end;
        end;
      end;
      if FDisplayMinutes then Canvas.Polygon(rotateMinutesHand);

      for i:=1 to 7 do begin
        case i of
          2,3,5,6:begin
            rotateSecondsHand[i].x:=round(p0.x-SecondsHand[i,1]*sin(alphaS)+SecondsHand[i,2]*0.8*amin*cos(alphaS));
            rotateSecondsHand[i].y:=round(p0.y-SecondsHand[i,1]*cos(alphaS)-SecondsHand[i,2]*0.8*amin*sin(alphaS));
          end;
          4:begin
            EndPoint:=EllipsePoint(p0,a-a div 10,b-b div 10,alphaS);
            rotateSecondsHand[i].x:=round(EndPoint.X);
            rotateSecondsHand[i].y:=round(EndPoint.Y);
          end;
          1,7:begin
            rotateSecondsHand[i].x:=round(p0.x-SecondsHand[i,1]*sin(alphaS)+SecondsHand[i,2]*cos(alphaS));
            rotateSecondsHand[i].y:=round(p0.y-SecondsHand[i,1]*cos(alphaS)-SecondsHand[i,2]*sin(alphaS));
          end;
        end;
      end;
      if FDisplaySeconds then Canvas.Polygon(rotateSecondsHand);

      Canvas.Pen.Width:=oldPenWidth;

      Canvas.Pen.Color:=ColorThicks;//Black;
      i:=round(alphaS/(6*pi/180));

      StartPoint:=EllipsePoint(p0,a-a div 20,b-b div 20,alphaS);
      EndPoint:=EllipsePoint(p0,a-a div 10,b-b div 10,alphaS);
      if i mod 5 = 0 then begin
        oldPenWidth:=Canvas.Pen.Width;
        Canvas.Pen.Width:=3;
      end;
      if FDisplayTicks then begin
        Canvas.Line(trunc(StartPoint.X),trunc(StartPoint.Y),trunc(EndPoint.X),trunc(EndPoint.Y));
      end;
      if i mod 5 = 0 then begin
        EndPoint:=EllipsePoint(p0,a-a div 5,b-b div 5,alphaS);
        i:=3-(i div 5);
        if i<=0 then
          i:=i+12;
        if i=0 then
          i:=12;
        strhour:=IntToStr(i);
        dx:=-Canvas.TextWidth(strhour)/2;
        dy:=-Canvas.TextHeight(strhour)/2;
        if FDisplayDigits then begin
          Canvas.TextOut(trunc(EndPoint.X+dx),trunc(EndPoint.Y+dy),strhour);
        end;
        Canvas.Pen.Width:=oldPenWidth;
      end;

      i:=round(alphaM/(6*pi/180));

      StartPoint:=EllipsePoint(p0,a-a div 20,b-b div 20,alphaM);
      EndPoint:=EllipsePoint(p0,a-a div 10,b-b div 10,alphaM);
      if i mod 5 = 0 then begin
        oldPenWidth:=Canvas.Pen.Width;
        Canvas.Pen.Width:=3;
      end;
      if FDisplayTicks then begin
        Canvas.Line(trunc(StartPoint.X),trunc(StartPoint.Y),trunc(EndPoint.X),trunc(EndPoint.Y));
      end;
      if i mod 5 = 0 then begin
        EndPoint:=EllipsePoint(p0,a-a div 5,b-b div 5,alphaM);
        i:=3-(i div 5);
        if i<=0 then
          i:=i+12;
        if i=0 then
          i:=12;
        strhour:=IntToStr(i);
        dx:=-Canvas.TextWidth(strhour)/2;
        dy:=-Canvas.TextHeight(strhour)/2;
        if FDisplayDigits then begin
          Canvas.TextOut(trunc(EndPoint.X+dx),trunc(EndPoint.Y+dy),strhour);
        end;
        Canvas.Pen.Width:=oldPenWidth;
      end;
    end;//not wholerepaint
  end;
  
  if (FWatchMode = wmClock) then begin
    DecodeTime(Now,HH,MM,SS,MS);
  end else if (FWatchMode = wmStopwatch) then begin
    if FStopwatchRunning then begin
      DecodeTime((Now - FStopwatchStartTime),HH,MM,SS,MS);
    end else begin
      DecodeTime(trunc(Now),HH,MM,SS,MS);
    end;
  end else if (FWatchMode = wmDefined) then begin
    HH:=StrToInt(Copy(FDefinedTime,1,2));
    MM:=StrToInt(Copy(FDefinedTime,4,2));
    try
      SS:=StrToInt(Copy(FDefinedTime,7,2))
    except
      SS:=0;
    end;
  end;
  
  if (Assigned(FAppointment))and(FormatFloat('00',HH)+TimeSeparator+FormatFloat('00',MM)+TimeSeparator+FormatFloat('00',SS)=FMeetTime) then begin
    FAppointment(Self);         // notify user that is Meeting time.
    Invalidate
  end;
  
  TimeToAlpha(HH,MM,SS,alphaH,alphaM,alphaS);

  oldPenWidth:=Canvas.Pen.Width;
  Canvas.Pen.Width:=2;
  Canvas.Pen.Color:=FColorHoursHand;
  for i:=1 to 7 do begin
    case i of
      2,3,5,6:begin
        rotateHoursHand[i].x:=round(p0.x-HoursHand[i,1]*sin(alphaH)+HoursHand[i,2]*0.2*amin*cos(alphaH));
        rotateHoursHand[i].y:=round(p0.y-HoursHand[i,1]*cos(alphaH)-HoursHand[i,2]*0.2*amin*sin(alphaH));
      end;
      4:begin
        EndPoint:=EllipsePoint(p0,a-a div 2,b-b div 2,alphaH);
        rotateHoursHand[i].x:=round(EndPoint.X);
        rotateHoursHand[i].y:=round(EndPoint.Y);
      end;
      1,7:begin
        rotateHoursHand[i].x:=round(p0.x-HoursHand[i,1]*sin(alphaH)+HoursHand[i,2]*cos(alphaH));
        rotateHoursHand[i].y:=round(p0.y-HoursHand[i,1]*cos(alphaH)-HoursHand[i,2]*sin(alphaH));
      end;
    end;
  end;
  if FDisplayHours then Canvas.Polygon(rotateHoursHand);

  Canvas.Pen.Color:=FColorMinutesHand;
  for i:=1 to 7 do begin
    case i of
      2,3,5,6:begin
        rotateMinutesHand[i].x:=round(p0.x-MinutesHand[i,1]*sin(alphaM)+MinutesHand[i,2]*0.5*amin*cos(alphaM));
        rotateMinutesHand[i].y:=round(p0.y-MinutesHand[i,1]*cos(alphaM)-MinutesHand[i,2]*0.5*amin*sin(alphaM));
      end;
      4:begin
        EndPoint:=EllipsePoint(p0,a-a div 5,b-b div 5,alphaM);
        rotateMinutesHand[i].x:=round(EndPoint.X);
        rotateMinutesHand[i].y:=round(EndPoint.Y);
      end;
      1,7:begin
        rotateMinutesHand[i].x:=round(p0.x-MinutesHand[i,1]*sin(alphaM)+MinutesHand[i,2]*cos(alphaM));
        rotateMinutesHand[i].y:=round(p0.y-MinutesHand[i,1]*cos(alphaM)-MinutesHand[i,2]*sin(alphaM));
      end;
    end;
  end;
  if FDisplayMinutes then Canvas.Polygon(rotateMinutesHand);

  Canvas.Pen.Color:=FColorSecondsHand;
  for i:=1 to 7 do begin
    case i of
      2,3,5,6:begin
        rotateSecondsHand[i].x:=round(p0.x-SecondsHand[i,1]*sin(alphaS)+SecondsHand[i,2]*0.8*amin*cos(alphaS));
        rotateSecondsHand[i].y:=round(p0.y-SecondsHand[i,1]*cos(alphaS)-SecondsHand[i,2]*0.8*amin*sin(alphaS));
      end;
      4:begin
        EndPoint:=EllipsePoint(p0,a-a div 10,b-b div 10,alphaS);
        rotateSecondsHand[i].x:=round(EndPoint.X);
        rotateSecondsHand[i].y:=round(EndPoint.Y);
      end;
      1,7:begin
        rotateSecondsHand[i].x:=round(p0.x-SecondsHand[i,1]*sin(alphaS)+SecondsHand[i,2]*cos(alphaS));
        rotateSecondsHand[i].y:=round(p0.y-SecondsHand[i,1]*cos(alphaS)-SecondsHand[i,2]*sin(alphaS));
      end;
    end;
  end;
  if FDisplaySeconds then Canvas.Polygon(rotateSecondsHand);
  
  Canvas.Pen.Color:=ColorThicks;
  Canvas.EllipseC(a+bx,b+by,4,4);//borchia

  Canvas.Pen.Width:=oldPenWidth;

  HHOLD:=HH;
  MMOLD:=MM;
  SSOLD:=SS;
end;

procedure TCustomAnalogWatch.SetDepth(Value:byte);
begin
  if FDepth=Value then exit;
  FDepth:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetColorFace(Value:TColor);
begin
  if FColorFace=Value then exit;
  FColorFace:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetColorThicks(Value:TColor);
begin
  if FColorThicks=Value then exit;
  FColorThicks:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetMeetTime(Value:string);
begin
  if FMeetTime=Value then exit;
  if pos(TimeSeparator,Value)=0 then exit;
  FMeetTime:=Value;
end;

procedure TCustomAnalogWatch.SetWatchMode(Value:WatchMode);
begin
  if FWatchMode=Value then exit;
  FWatchMode:=Value;
  if (FWatchMode = wmStopWatch) then begin
    if FStopwatchRunning then begin
      FStopwatchStartTime := now;
    end;
  end else begin
    FStopwatchRunning := false;
  end;
end;

procedure TCustomAnalogWatch.SetStopwatchRunning(Value:boolean);
begin
  FStopwatchRunning := Value;
  if FStopwatchRunning and
     (FWatchMode = wmStopWatch) then begin
    FStopwatchStartTime := now;
  end;
end;

procedure TCustomAnalogWatch.SetDefinedTime(Value:string);
begin
  if FDefinedTime=Value then exit;
  FDefinedTime:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetDisplayDigits(Value:boolean);
begin
  if FDisplayDigits=Value then exit;
  FDisplayDigits:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetDisplayTicks(Value:boolean);
begin
  if FDisplayTicks=Value then exit;
  FDisplayTicks:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetDisplayHours(Value:boolean);
begin
  if FDisplayHours=Value then exit;
  FDisplayHours:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetDisplayMinutes(Value:boolean);
begin
  if FDisplayMinutes=Value then exit;
  FDisplayMinutes:=Value;
  Invalidate;
end;

procedure TCustomAnalogWatch.SetDisplaySeconds(Value:boolean);
begin
  if FDisplaySeconds=Value then exit;
  FDisplaySeconds:=Value;
  Invalidate;
end;

function TCustomAnalogWatch.GetInterval : Cardinal;
begin
  result := FTimer.Interval;
end;

procedure TCustomAnalogWatch.SetInterval (Value:Cardinal);
begin
  if FTimer.Interval=Value then exit;
  FTimer.Interval := Value;
end;


constructor TCustomAnalogWatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
  bx:=GetSystemMetrics(SM_CXEDGE);
  by:=GetSystemMetrics(SM_CYEDGE);
  {$ELSE WIN32}
  bx:=2;//GetSystemMetrics(SM_CXEDGE);
  by:=2;//GetSystemMetrics(SM_CYEDGE);
  {$ENDIF WIN32}
  HHOLD:=25;
  FDepth:=6;
  FWholeRepaint:=false;
  FColorFace:=clCream;
  FColorHoursHand:=clGreen;
  FColorMinutesHand:=clRed;
  FColorSecondsHand:=clBlack;
  FColorThicks:=clBlack;
  FDisplayDigits := true;
  FDisplayTicks := true;
  FDisplayHours := true;
  FDisplayMinutes := true;
  FDisplaySeconds := true;
  FTimer:=TTimer.Create(Self);
  FWatchMode := wmClock;
  FStopwatchRunning := false;
  FDefinedTime := TimeToStr(now)+TimeSeparator+'00';
  FTimer.Enabled:=true;
  FTimer.Interval:=1000;
  FMeetTime:='12'+TimeSeparator+'00'+TimeSeparator+'00';
  FTimer.OnTimer:=@WatchOnTimer;
  SetInitialBounds(0,0,300,200);
end;

destructor TCustomAnalogWatch.Destroy;
begin
  FreeThenNil(FTimer);
  inherited Destroy;
end;

procedure TCustomAnalogWatch.PaintWatch;
var i: integer;
    AngoloF:double;
    p0:TPoint;
    CenterEllipsePoint,StartPoint,EndPoint:TFloatPoint;
    oldPenWidth:integer;
    ihour:integer;
    dx,dy:double;
    strhour:string;
begin
  p0.x:=bx;
  p0.y:=by;

  //Watch Thickness
  oldPenWidth:=Canvas.Pen.Width;
  Canvas.Pen.Width:=2;
  Canvas.Brush.Color:=clGray;
  Canvas.Pen.Color:=ColorThicks;
  Canvas.Ellipse(p0.x,p0.y+FDepth,p0.x+2*a,p0.y+2*b+FDepth);

  //Watch Face
  Canvas.Brush.Color:=FColorFace;
  Canvas.Ellipse(p0.x,p0.y,p0.x+2*a,p0.y+2*b);
  Canvas.Pen.Width:=oldPenWidth;

  //Watch Minutes Tick
    CenterEllipsePoint.X:=p0.x+a;
    CenterEllipsePoint.Y:=p0.y+b;
    for i:=0 to 59 do begin
      AngoloF:=i*6*pi/180;
      StartPoint:=EllipsePoint(CenterEllipsePoint,a-a div 20,b-b div 20,AngoloF);
      EndPoint:=EllipsePoint(CenterEllipsePoint,a-a div 10,b-b div 10,AngoloF);
      if i mod 5 = 0 then begin
        oldPenWidth:=Canvas.Pen.Width;
        Canvas.Pen.Width:=3;
      end;
      if FDisplayTicks then begin
        Canvas.Line(trunc(StartPoint.X),trunc(StartPoint.Y),trunc(EndPoint.X),trunc(EndPoint.Y));
      end;
      if i mod 5 = 0 then begin
        EndPoint:=EllipsePoint(CenterEllipsePoint,a-a div 5,b-b div 5,AngoloF);
        ihour:=3-(i div 5);
        if ihour<=0 then
          ihour:=ihour+12;
        strhour:=IntToStr(ihour);
        dx:=-Canvas.TextWidth(strhour)/2;
        dy:=-Canvas.TextHeight(strhour)/2;
        if FDisplayDigits then begin
          Canvas.TextOut(trunc(EndPoint.X+dx),trunc(EndPoint.Y+dy),strhour);
        end;
        Canvas.Pen.Width:=oldPenWidth;
      end;
    end;

  // Watch Captions
  Canvas.TextOut(bx,by,Caption);
end;

procedure TCustomAnalogWatch.ResizeWatch;
begin
  b:=((Height-FDepth)div(2))-by;
  a:=(Width div 2)-bx;
  if a<=0 then
    a:=1;
  if b<=0 then
    b:=1;
end;

procedure TCustomAnalogWatch.Paint;
begin
  inherited Paint;
  ResizeWatch;
  PaintWatch
end;

procedure TCustomAnalogWatch.Resize;
begin
  inherited Resize;
  ResizeWatch;
end;

initialization

{$i analogwatch.lrs}

end.
