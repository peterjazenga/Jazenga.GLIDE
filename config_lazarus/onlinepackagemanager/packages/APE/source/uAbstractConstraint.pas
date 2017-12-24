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
unit uAbstractConstraint;

interface

Uses uAbstractITem, uRender;

type

AbstractConstraint = Class(AbstractITem)
Private
  _stiffness : double;
Public
  aRenderer : AbstractRenderer;

  constructor Create(Stiffness : Double); Reintroduce;

  Function Stiffness : Double; Overload;
  Procedure Stiffness(Value : Double); Overload;

  Procedure SetRenderer(TheRenderer : AbstractRenderer);

  Procedure Resolve; Virtual; Abstract;
end;

implementation

{ AbstractConstraint }

constructor AbstractConstraint.Create(Stiffness: Double);
begin
  Inherited Create;
  Self.Stiffness(Stiffness);
end;

function AbstractConstraint.Stiffness: Double;
begin
  Result := _stiffness;
end;

procedure AbstractConstraint.SetRenderer(TheRenderer: AbstractRenderer);
begin
  aRenderer := TheRenderer;
end;

procedure AbstractConstraint.Stiffness(Value: Double);
begin
  _stiffness := Value;
end;

end.


