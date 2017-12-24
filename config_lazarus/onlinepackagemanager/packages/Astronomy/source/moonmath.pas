{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


unit MoonMath;

interface   

function tan(x:extended):extended;
function arctan2(a,b:extended):extended;
function arcsin(x:extended):extended;
function arccos(x:extended):extended;

function deg2rad(x:extended):extended;
function rad2deg(x:extended):extended;

function sin_d(x:extended):extended;
function cos_d(x:extended):extended;
function tan_d(x:extended):extended;
function arctan2_d(a,b:extended):extended;
function arcsin_d(x:extended):extended;
function arccos_d(x:extended):extended;
function arctan_d(x:extended):extended;


function put_in_360(x:extended):extended;

function adjusted_mod(a,b:integer):integer;

implementation

uses  math;

function deg2rad(x:extended):extended;
begin
  result:=x/180*pi;
  end;

function rad2deg(x:extended):extended;
begin
  result:=x*180/pi;
end;

function tan(x:extended):extended;
begin
  result:=math.tan(x);
end;

function arctan2(a,b:extended):extended;
begin
  result:=math.arctan2(a,b);
end;

function arcsin(x:extended):extended;
begin
  result:=math.arcsin(x);
end;

function arccos(x:extended):extended;
begin
  result:=math.arccos(x);
end;

function sin_d(x:extended):extended;
begin
  sin_d:=sin(deg2rad(put_in_360(x)));
  end;

function cos_d(x:extended):extended;
begin
  cos_d:=cos(deg2rad(put_in_360(x)));
end;

function tan_d(x:extended):extended;
begin
  tan_d:=tan(deg2rad(put_in_360(x)));
end;

function arctan2_d(a,b:extended):extended;
begin
  result:=rad2deg(arctan2(a,b));
end;

function arcsin_d(x:extended):extended;
begin
  result:=rad2deg(arcsin(x));
end;

function arccos_d(x:extended):extended;
begin
  result:=rad2deg(arccos(x));
end;

function arctan_d(x:extended):extended;
begin
  result:=rad2deg(arctan(x));
end;

function put_in_360(x:extended):extended;
begin
  result:=x-round(x/360)*360;
  while result<0 do result:=result+360;
end;

function adjusted_mod(a,b:integer):integer;
begin
  result:=a mod b;
  while result<1 do
    result:=result+b;
  end;

end.

