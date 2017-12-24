unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
   SMNetGradient;

type

  { TForm1 }

  TForm1 = class(TForm)
    NetGradient1: TNetGradient;
    NetGradient2: TNetGradient;
    NetGradient3: TNetGradient;
    NetGradient4: TNetGradient;
    NetGradient5: TNetGradient;
    NetGradient6: TNetGradient;
    NetGradient8: TNetGradient;
    NetGradient9: TNetGradient;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I unit1.lrs}

end.

