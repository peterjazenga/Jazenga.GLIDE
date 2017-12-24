unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  chemtext;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  StringGrid1.DefaultRowHeight := ChemTextHeight(StringGrid1.Canvas, 'H2') + 2*constCellpadding;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  X, Y: Integer;
  txt: String;
begin
  X := ARect.Left + constCellpadding;
  Y := ARect.Top + constCellpadding;
  txt := StringGrid1.Cells[ACol, ARow];

  StringGrid1.Canvas.FillRect(ARect);

  case ACol of
    0 : if ARow > 0 then StringGrid1.Canvas.TextOut(X, Y, IntToStr(ARow));
    1 : ChemTextOut(StringGrid1.Canvas, X, Y, txt)
   else StringGrid1.Canvas.TextOut(X, Y, txt);
  end;
end;

end.

