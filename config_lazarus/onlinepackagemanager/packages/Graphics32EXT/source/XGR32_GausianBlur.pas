
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_GausianBlur;


interface

uses

  SysUtils, classes, Math, GR32;

type
  TGausianBlur = class
  public
    fradius    : integer;
    fkernelSize: integer;
    fkernel    : array of integer;
    fmult      : array of array of integer;
    constructor Create(radius: integer);
    procedure SetRadius(radius: integer);
    procedure Execute(bitmap: TBitmap32; x, y, w, h: integer);
  end;

implementation

constructor TGausianBlur.Create(radius: integer);
begin
  setRadius(radius);
end;

procedure TGausianBlur.SetRadius(radius: integer);
var
  i,j,radiusi: integer;
begin
  radius := min(max(1,radius),248);
  if (self.fradius = radius) then exit;
  self.fradius := radius;
  fkernelSize := 1 +radius*2;
  setlength(fkernel, fkernelSize);
  setlength(fmult, fkernelSize);
  for i := 0 to high(fmult) do setlength(fmult[i], 256);
  for i := 1 to radius -1 do
  begin
    radiusi := radius-i;
    fkernel[radiusi] := radiusi*radiusi;
    fkernel[radius+i] := fkernel[radiusi];
    for j := 0 to 255 do
    begin
      fmult[radiusi][j] := fkernel[radiusi]*j;
      fmult[radius+i][j] := fmult[radiusi][j];
    end;
  end;
  fkernel[radius] := radius*radius;
  for j :=0 to 255 do
    fmult[radius][j] := fkernel[radius]*j;
end;

procedure TGausianBlur.Execute(bitmap: TBitmap32; x, y, w, h: integer);
var
  sum,ca,cr,cg,cb,k: integer;
  pixel,i,xl,yl,yi,ym,riw: integer;
  iw,wh,p,q: integer;
  pix: PColor32Array;
  ri: TColor32Entry;
  a,r,g,b, a2,r2,g2,b2: array of integer;
begin
  pix := bitmap.bits;
  iw := bitmap.width;
  wh := iw * bitmap.height;
  setlength(a, wh);
  setlength(r, wh);
  setlength(g, wh);
  setlength(b, wh);

  for i := 0 to wh -1 do
  begin
    {$R-}
    ri := TColor32Entry(pix[i]);
    {$R+}
    a[i] := ri.A;
    r[i] := ri.R;
    g[i] := ri.G;
    b[i] := ri.B;
  end;

  setlength(a2, wh);
  setlength(r2, wh);
  setlength(g2, wh);
  setlength(b2, wh);

  x := max(0,x);
  y := max(0,y);
  w := x+w -max(0,(x+w) -iw);
  h := y+h -max(0,(y+h) -bitmap.height);
  yi := y*iw;

  for yl := y to h-1 do
  begin
    for xl := x to w-1 do
    begin
      cb := 0; cg := 0; cr := 0; ca := 0; sum:= 0;
      q := xl - fradius;
      for i :=0 to fkernelSize-1 do
      begin
        p := q+i;
        if (p >= x) and (p < w) then
        begin
          inc(p, yi);
          inc(ca, fmult[i][a[p]]);
          inc(cr, fmult[i][r[p]]);
          inc(cg, fmult[i][g[p]]);
          inc(cb, fmult[i][b[p]]);
          inc(sum, fkernel[i]);
        end;
      end;
      q := yi+xl;
      a2[q] := ca div sum;
      r2[q] := cr div sum;
      g2[q] := cg div sum;
      b2[q] := cb div sum;
    end;
    inc(yi, iw);
  end;

  yi := y*iw;
  for yl :=y to h -1 do
  begin
    ym := yl -fradius;
    riw := ym*iw;
    for xl := x to w -1 do
    begin
      cb := 0; cg := 0; cr := 0; ca := 0; sum := 0;
      q := ym;
      p := xl +riw;
      for i := 0 to fkernelSize -1 do
      begin
        if (q < h) and (q >= y) then
        begin
          inc(ca, fmult[i][a2[p]]);
          inc(cr, fmult[i][r2[p]]);
          inc(cg, fmult[i][g2[p]]);
          inc(cb, fmult[i][b2[p]]);
          inc(sum, fkernel[i]);
        end;
        inc(q);
        inc(p, iw);
      end;
    {$R-}
      TColor32Entry(pix[xl+yi]).ARGB := cardinal((ca div sum) shl 24 or
        (cr div sum) shl 16 or (cg div sum) shl 8 or (cb div sum));
    {$R+}
    end;
    inc(yi, iw);
  end;
end;


end.
