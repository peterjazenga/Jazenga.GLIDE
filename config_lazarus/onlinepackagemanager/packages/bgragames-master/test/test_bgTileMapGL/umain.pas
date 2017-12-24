unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, bgTileMapGL, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControl1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    TileMap: TBGTileMapGL;
    dataLoaded: boolean;
    procedure Load;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  Load;

  TileMap.DrawMap(OpenGLControl1.Width, OpenGLControl1.Height);

  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TileMap.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DataLoaded := False;
end;

procedure TForm1.Load;
begin
  if DataLoaded then
    exit;

  DataLoaded := True;

  TileMap := TBGTileMapGL.Create('map.ini');
end;

end.

