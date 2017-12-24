{
APE (Actionscript Physics Engine) is an AS3 open source 2D physics engine
Copyright 2006, Alec Cove

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

Contact: ape@cove.org

Converted to ObjectPascal by Vincent Gsell vincent.gsell@gmail.com
}
unit uGDIRender;

interface

Uses uRender, Graphics, Classes, Math, uvector;

Type

GDIRenderer = Class(AbstractRenderer)
Private
  FCanvas : TCanvas;
Public
  Constructor Create(aCanvas : TCanvas); Reintroduce;

  Procedure Circle(xcenter,ycenter,Radius, Rotate : Double); Override;
  Procedure Line(x,y,xx,yy : Double); Override;
  Procedure Text(x,y : Double; Text : String); Override;
end;

implementation

{ GDIRenderer }

procedure GDIRenderer.Circle(xcenter, ycenter, Radius, Rotate: Double);
begin
  FCanvas.Ellipse(Round(xcenter-Radius),Round(ycenter-radius),Round(xcenter+radius),Round(ycenter+radius));
end;

constructor GDIRenderer.Create(aCanvas: TCanvas);
begin
  Assert(Assigned(aCanvas));
  FCanvas:=aCanvas;
end;

procedure GDIRenderer.Line(x, y, xx, yy: Double);
begin
  FCanvas.MoveTo(Round(x),Round(y));
  FCanvas.LineTo(Round(xx),Round(yy));
end;

//procedure GDIRenderer.Rectangle(xcenter, ycenter, Width, height,
//  Rotate: Double);
//begin
//  Fcanvas.Rectangle(Round(xcenter-Width/2),Round(ycenter-Height/2),Round(xcenter+Width/2),Round(ycenter+Height/2));
//end;

procedure GDIRenderer.Text(x, y : Double; Text: String);
begin
  FCanvas.TextOut(Round(x),Round(y),Text);
end;

end.
