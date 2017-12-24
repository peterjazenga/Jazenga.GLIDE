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
unit uInterval;

interface

type

Interval = Class
Public
  min,max : Double;

  Constructor Create(amin,amax : Double);
  Function ToString : String;
end;

implementation

uses SysUtils;

{ Interval }

constructor Interval.Create(amin,amax: Double);
begin
  Self.min:=amin;
  Self.max:=amax;
end;

function Interval.ToString: String;
begin
  result:=FloatToStr(min)+' : '+FloatToStr(max);
end;

end.
