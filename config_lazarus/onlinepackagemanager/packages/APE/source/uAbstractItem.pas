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
unit uAbstractItem;

interface

Type

AbstractItem = class
  private
    _Visible : Boolean;
    _AlwaysRepaint : Boolean;
    function GetAR: Boolean;
    function GetVisible: Boolean;
    procedure SetAR(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
Public
  Constructor Create; Virtual;
  Procedure Init; Virtual; Abstract;
  Procedure Paint; Virtual; Abstract;
  Procedure CleanUp; Virtual; Abstract;
  Property Visible : Boolean read GetVisible Write SetVisible;
  Property AlwaysRepaint : Boolean read GetAR Write SetAR;
end;

implementation

{ AbstractItem }

constructor AbstractItem.Create;
begin
  Visible := true;
  AlwaysRepaint := False;
end;

function AbstractItem.GetAR: Boolean;
begin
  Result := _AlwaysRepaint;
end;

function AbstractItem.GetVisible: Boolean;
begin
  Result := _Visible;
end;

procedure AbstractItem.SetAR(const Value: Boolean);
begin
  _AlwaysRepaint := Value;
end;

procedure AbstractItem.SetVisible(const Value: Boolean);
begin
   _Visible := Value;
end;

end.
