
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_clip_liang_barsky;

interface

{$I agg_mode.inc }

uses
  agg_basics;

function clipping_flags_int(x, y: int; clip_box: rect_ptr): unsigned;
function clipping_flags_dbl(x, y: double; clip_box: rect_d_ptr): unsigned;

function clipping_flags_x_int(x: int; clip_box: rect_ptr): unsigned;
function clipping_flags_x_dbl(x: double; clip_box: rect_d_ptr): unsigned;

function clipping_flags_y_int(y: int; clip_box: rect_ptr): unsigned;
function clipping_flags_y_dbl(y: double; clip_box: rect_d_ptr): unsigned;

function clip_liang_barsky_int(x1, y1, x2, y2: int; clip_box: rect_ptr; x, y: int_ptr): unsigned;
function clip_liang_barsky_d(x1, y1, x2, y2: double; clip_box: rect_d_ptr; x, y: double_ptr): unsigned;


implementation


// Determine the clipping code of the vertex according to the
// Cyrus-Beck line clipping algorithm

//        |        |
//  0110  |  0010  | 0011
//        |        |
// -------+--------+-------- clip_box^.y2
//        |        |
//  0100  |  0000  | 0001
//        |        |
// -------+--------+-------- clip_box^.y1
//        |        |
//  1100  |  1000  | 1001
//        |        |
//  clip_box^.x1  clip_box^.x2


function clipping_flags_int(x, y: int; clip_box: rect_ptr): unsigned;
begin
  Result :=
    unsigned(x > clip_box^.x2) or (unsigned(y > clip_box^.y2) shl 1) or (unsigned(x < clip_box^.x1) shl 2) or (unsigned(y < clip_box^.y1) shl 3);

end;

{ CLIPPING_FLAGS_DBL }
function clipping_flags_dbl(x, y: double; clip_box: rect_d_ptr): unsigned;
begin
  Result :=
    unsigned(x > clip_box^.x2) or (unsigned(y > clip_box^.y2) shl 1) or (unsigned(x < clip_box^.x1) shl 2) or (unsigned(y < clip_box^.y1) shl 3);

end;

{ CLIPPING_FLAGS_X_INT }
function clipping_flags_x_int(x: int; clip_box: rect_ptr): unsigned;
begin
  Result :=
    unsigned(x > clip_box^.x2) or (unsigned(x < clip_box^.x1) shl 2);

end;

{ CLIPPING_FLAGS_X_DBL }
function clipping_flags_x_dbl(x: double; clip_box: rect_d_ptr): unsigned;
begin
  Result :=
    unsigned(x > clip_box^.x2) or (unsigned(x < clip_box^.x1) shl 2);

end;

{ CLIPPING_FLAGS_Y_INT }
function clipping_flags_y_int(y: int; clip_box: rect_ptr): unsigned;
begin
  Result :=
    (unsigned(y > clip_box^.y2) shl 1) or (unsigned(y < clip_box^.y1) shl 3);

end;

{ CLIPPING_FLAGS_Y_DBL }
function clipping_flags_y_dbl(y: double; clip_box: rect_d_ptr): unsigned;
begin
  Result :=
    (unsigned(y > clip_box^.y2) shl 1) or (unsigned(y < clip_box^.y1) shl 3);

end;

{ CLIP_LIANG_BARSKY_INT }
function clip_liang_barsky_int(x1, y1, x2, y2: int; clip_box: rect_ptr; x, y: int_ptr): unsigned;
const
  nearzero = 1e-30;

var
  deltax, deltay, xin, xout, yin, yout, tinx, tiny, toutx, touty, tin1, tin2, tout1: double;

  np: unsigned;

begin
  deltax := x2 - x1;
  deltay := y2 - y1;

  np := 0;

  // bump off of the vertical
  if deltax = 0.0 then
    if x1 > clip_box^.x1 then
      deltax := -nearzero
    else
      deltax := nearzero;

  // bump off of the horizontal
  if deltay = 0.0 then
    if y1 > clip_box^.y1 then
      deltay := -nearzero
    else
      deltay := nearzero;

  if deltax > 0.0 then
  begin
    // points to right
    xin := clip_box^.x1;
    xout := clip_box^.x2;

  end
  else
  begin
    xin := clip_box^.x2;
    xout := clip_box^.x1;

  end;

  if deltay > 0.0 then
  begin
    // points up
    yin := clip_box^.y1;
    yout := clip_box^.y2;

  end
  else
  begin
    yin := clip_box^.y2;
    yout := clip_box^.y1;

  end;

  tinx := (xin - x1) / deltax;
  tiny := (yin - y1) / deltay;

  if tinx < tiny then
  begin
    // hits x first
    tin1 := tinx;
    tin2 := tiny;

  end
  else
  begin
    // hits y first
    tin1 := tiny;
    tin2 := tinx;

  end;

  if tin1 <= 1.0 then
  begin
    if 0.0 < tin1 then
    begin
      x^ := trunc(xin);
      y^ := trunc(yin);

      Inc(ptrcomp(x), sizeof(int));
      Inc(ptrcomp(y), sizeof(int));
      Inc(np);

    end;

    if tin2 <= 1.0 then
    begin
      toutx := (xout - x1) / deltax;
      touty := (yout - y1) / deltay;

      if toutx < touty then
        tout1 := toutx
      else
        tout1 := touty;

      if (tin2 > 0.0) or (tout1 > 0.0) then
        if tin2 <= tout1 then
        begin
          if tin2 > 0.0 then
          begin
            if tinx > tiny then
            begin
              x^ := trunc(xin);
              y^ := trunc(y1 + tinx * deltay);

            end
            else
            begin
              x^ := trunc(x1 + tiny * deltax);
              y^ := trunc(yin);

            end;

            Inc(ptrcomp(x), sizeof(int));
            Inc(ptrcomp(y), sizeof(int));
            Inc(np);

          end;

          if tout1 < 1.0 then
            if toutx < touty then
            begin
              x^ := trunc(xout);
              y^ := trunc(y1 + toutx * deltay);

            end
            else
            begin
              x^ := trunc(x1 + touty * deltax);
              y^ := trunc(yout);

            end
          else
          begin
            x^ := x2;
            y^ := y2;

          end;

          Inc(ptrcomp(x), sizeof(int));
          Inc(ptrcomp(y), sizeof(int));
          Inc(np);

        end
        else
        begin
          if tinx > tiny then
          begin
            x^ := trunc(xin);
            y^ := trunc(yout);

          end
          else
          begin
            x^ := trunc(xout);
            y^ := trunc(yin);

          end;

          Inc(ptrcomp(x), sizeof(int));
          Inc(ptrcomp(y), sizeof(int));
          Inc(np);

        end;

    end;

  end;

  Result := np;

end;

{ CLIP_LIANG_BARSKY_D }
function clip_liang_barsky_d(x1, y1, x2, y2: double; clip_box: rect_d_ptr; x, y: double_ptr): unsigned;
const
  nearzero = 1e-30;

var
  deltax, deltay, xin, xout, yin, yout, tinx, tiny, toutx, touty, tin1, tin2, tout1: double;

  np: unsigned;

begin
  deltax := x2 - x1;
  deltay := y2 - y1;

  np := 0;

  // bump off of the vertical
  if deltax = 0.0 then
    if x1 > clip_box^.x1 then
      deltax := -nearzero
    else
      deltax := nearzero;

  // bump off of the horizontal
  if deltay = 0.0 then
    if y1 > clip_box^.y1 then
      deltay := -nearzero
    else
      deltay := nearzero;

  if deltax > 0.0 then
  begin
    // points to right
    xin := clip_box^.x1;
    xout := clip_box^.x2;

  end
  else
  begin
    xin := clip_box^.x2;
    xout := clip_box^.x1;

  end;

  if deltay > 0.0 then
  begin
    // points up
    yin := clip_box^.y1;
    yout := clip_box^.y2;

  end
  else
  begin
    yin := clip_box^.y2;
    yout := clip_box^.y1;

  end;

  tinx := (xin - x1) / deltax;
  tiny := (yin - y1) / deltay;

  if tinx < tiny then
  begin
    // hits x first
    tin1 := tinx;
    tin2 := tiny;

  end
  else
  begin
    // hits y first
    tin1 := tiny;
    tin2 := tinx;

  end;

  if tin1 <= 1.0 then
  begin
    if 0.0 < tin1 then
    begin
      x^ := xin;
      y^ := yin;

      Inc(ptrcomp(x), sizeof(int));
      Inc(ptrcomp(y), sizeof(int));
      Inc(np);

    end;

    if tin2 <= 1.0 then
    begin
      toutx := (xout - x1) / deltax;
      touty := (yout - y1) / deltay;

      if toutx < touty then
        tout1 := toutx
      else
        tout1 := touty;

      if (tin2 > 0.0) or (tout1 > 0.0) then
        if tin2 <= tout1 then
        begin
          if tin2 > 0.0 then
          begin
            if tinx > tiny then
            begin
              x^ := xin;
              y^ := y1 + tinx * deltay;

            end
            else
            begin
              x^ := x1 + tiny * deltax;
              y^ := yin;

            end;

            Inc(ptrcomp(x), sizeof(int));
            Inc(ptrcomp(y), sizeof(int));
            Inc(np);

          end;

          if tout1 < 1.0 then
            if toutx < touty then
            begin
              x^ := xout;
              y^ := y1 + toutx * deltay;

            end
            else
            begin
              x^ := x1 + touty * deltax;
              y^ := yout;

            end
          else
          begin
            x^ := x2;
            y^ := y2;

          end;

          Inc(ptrcomp(x), sizeof(int));
          Inc(ptrcomp(y), sizeof(int));
          Inc(np);

        end
        else
        begin
          if tinx > tiny then
          begin
            x^ := xin;
            y^ := yout;

          end
          else
          begin
            x^ := xout;
            y^ := yin;

          end;

          Inc(ptrcomp(x), sizeof(int));
          Inc(ptrcomp(y), sizeof(int));
          Inc(np);

        end;

    end;

  end;

  Result := np;

end;

end.


