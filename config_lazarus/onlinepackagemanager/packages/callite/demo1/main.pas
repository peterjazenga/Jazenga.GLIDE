unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, CalendarLite;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    CalendarLite1: TCalendarLite;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  CalendarLite1 := TCalendarLite.Create(self);
  with CalendarLite1 do begin
    Parent := self;
    Left := 10;
    Top := 10;
    Width := self.Width - 2*Left;
    Height := self.Height - 2*Top;
    Date := Now();
    DisplayTexts := '"Today is",dd/mm/yyyy,"Holidays during","There are no holidays set for"';
    WeekendDays := [dowSaturday];
    Anchors := [akLeft, akTop, akRight, akBottom];
  end;
end;

end.

