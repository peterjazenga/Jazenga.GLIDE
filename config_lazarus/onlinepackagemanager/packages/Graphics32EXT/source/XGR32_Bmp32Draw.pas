
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Bmp32Draw;

{$MODE Delphi}

interface

uses
    LCLIntf, LCLType, Types, Graphics,
    GR32, GR32_Blend, GR32_Filters, GR32_Resamplers,
    XGR32_Color, XGR32_Algebra;

{$DEFINE ALFA_BLEND}


procedure PaintSpiral(Buffer: TBitmap32; const Center: TPoint; Radius : Single; Step: Single; const Args: array of TColor32);
procedure PaintCircle(Buffer: TBitmap32;const Center: TPoint; Radius : Single; const Args: array of TColor32); overload;
procedure PaintCircle(Buffer: TBitmap32;const Center: TPoint; Radius: Single; const Clr32:  TColor32);overload;
procedure PaintGradient(Buffer: TBitmap32; const ARect: TRect; Clr32:  TColor32; Contrast : integer);
procedure PaintGradientRound(Buffer: TBitmap32; const ARect: TRect;   const Center : TPoint; Radius : Integer;
                             Clr32: TColor32; Contrast : integer);
procedure PaintTarget(Buffer: TBitmap32; const ARect: TRect;
                       const Center : TPoint; Radius : Integer; const Clr32:  TColor32);


procedure Ellipse(Buffer: TBitmap32; EllipseRect: TRect; AA: boolean;
                  clLine32: TColor32; Fill: boolean = false; clFill32: TColor32 = clBlack32);
procedure Arc(Buffer: TBitmap32; aRect: TRect; aStart, aEnd : single;
               LineColor32: TColor32);overload;
// Set angles in radians
procedure Arc(Buffer: TBitmap32; Radius: integer; Center: TPoint;
              AngleStart, AngleEnd : single; LineColor32: TColor32);overload;

procedure FillMoireRect(Buffer: TBitmap32; Clr32: TColor32);
procedure FillMoireRectS(Buffer: TBitmap32; Rect: TRect; Clr32: TColor32);

// drawing grid with float number step
procedure FillGridRectS(Buffer: TBitmap32; Rect: TRect; Step: Integer; Clr32: TColor32);

// Paint circumference with color Clr32
procedure FillRectS(Buffer: TBitmap32; const ARect: TRect; Clr32:  TColor32; Contrast : integer);
//
procedure FillTargetS(Buffer: TBitmap32; const ARect: TRect; const Center : TPoint; Radius : Integer; const Clr32:  TColor32);
// Fill round with vertical gradient
procedure FillRoundVS(Buffer: TBitmap32; const Center : TPoint; Radius : Integer; Clr32: TColor32; Contrast : integer);
// Fill round with gradient from center
procedure FillRoundCS( Buffer: TBitmap32; const Center: TPoint; Radius: Single;
                      const Clr32: TColor32; Contrast: Integer; AA: boolean = false);overload;
// Fill round with gradient from center
procedure FillRoundCS(Buffer: TBitmap32; const Center: TPoint; Radius: Single;
                      const Clrs32: array of TColor32; AA: boolean = false);overload;


implementation

uses math;

procedure PaintSpiral(Buffer: TBitmap32; const Center: TPoint; Radius: Single;
  Step: Single; const Args: array of TColor32);
var
  Theta: Single;
begin
  with Buffer do
  begin
   StippleStep := Step/2;
   SetStipple(Args);
  end;

  with Center do
  begin
   Theta := 0;
   Buffer.MoveToF(X, Y);
   while Theta < Radius {* 3.1415926535} do
   begin
    Buffer.LineToFSP(X + Cos(Theta) * Theta, Y + Sin(Theta) * Theta);
    Theta := Theta + Step;
   end;
  end;
end;

procedure PaintCircle(Buffer: TBitmap32; const Center: TPoint;
  Radius: Single; const Args: array of TColor32);
var
  Theta: Single;
  Step : Single;
begin
  if Radius <= 0 then Exit;

  with Center, Buffer do
  begin
   Theta := 0;
   if Radius<Pi then
     Step := 1/Radius
   else
     Step := Pi/Radius;

   StippleStep := 1/2;
   SetStipple(Args);

   MoveToF(X+Radius, Y);
   while Theta < Pi*2 do
   begin
    LineToFSP(X +Radius * Cos(Theta),Y + Radius * Sin(Theta));
    Theta := Theta + Step;
   end;
   LineToFSP(X+Radius, Y);
  end;
end;

procedure PaintCircle(Buffer: TBitmap32;const Center: TPoint;
  Radius: Single; const Clr32:  TColor32);
var
  Theta: Single;
  Step : Single;
begin
  if Radius <= 0 then Exit;

  with Center do
  begin
   Theta := 0;
   if Radius<Pi then
     Step := 1/Radius
   else
     Step := Pi/Radius;
   Buffer.PenColor := Clr32;

   Buffer.MoveToF(X+Radius, Y);
   while Theta < Pi*2 do
   begin
    Buffer.LineToFS(X +Radius * Cos(Theta),Y + Radius * Sin(Theta));
    Theta := Theta + Step;
   end;
   Buffer.LineToFS(X+Radius, Y);
  end;
end;

procedure PaintGradient(Buffer: TBitmap32;const ARect: TRect;
        Clr32:  TColor32; Contrast: integer);
var
  LineColor: TColor32;
  I, CY, H: Integer;
begin
  if IsRectEmpty(ARect) then Exit;

  if Contrast <> 0 then
  begin
    with ARect do
    try
      H := Bottom - Top;
      CY := (Top + Bottom) div 2;
      for I := Top to Bottom - 1 do
      begin
        LineColor := Lighten(Clr32, (CY - I) * Contrast div H);
        {$IFDEF ALFA_BLEND}
         Buffer.HorzLineTS(Left, I, Right - 1, LineColor);
        {$ELSE}
         Buffer.HorzLineS(Left, I, Right - 1, LineColor);
        {$ENDIF}
      end;
    finally
      EMMS; // the low-level blending function was used EMMS is required
    end;
  end
  else Buffer.FillRectS(ARect, Clr32);
end;

procedure PaintGradientRound(Buffer: TBitmap32;
  const ARect: TRect; const Center: TPoint; Radius: Integer;
  Clr32: TColor32; Contrast : integer);
var SaveRect : TRect;
    y, y1, y2, x1, x2 : integer;
    sqrX{, sqrR} : integer;
    LineColor: TColor32;
    CY, H: Integer;
begin

 if IsRectEmpty(ARect) then Exit;
 if Radius <= 0 then Exit;
 if Radius > 46340 then Exit; // SqRt(2147483647)
 if Center.X > 46340 then Exit;
 if Center.Y > 46340 then Exit;
// if Contrast <= 0


 with Buffer do
 try
  // Ρξυπΰνθμ ξαλΰρςό
  SaveRect := ClipRect;
  ClipRect := ARect;
  // Νΰχΰλόνϋε οεπεμεννϋε
  H := Radius * 2;
  CY := Center.Y;
  LineColor := Clr32;
  y1 := Center.Y - Radius;
  if y1 < SaveRect.Top then y1 := SaveRect.Top;
  y2 := Center.Y + Radius;
  if y2 > SaveRect.Bottom - 1 then y2 := SaveRect.Bottom - 1;
  // Κβΰδπΰς πΰδθσρΰ
  Radius := sqr(Radius);
  for y := y1 to y2 do
   begin
    sqrX := Radius - sqr(y - Center.Y);
    if sqrX >= 0 then
     begin
      x2 := Trunc(sqrt(sqrX+0.001)+0.5); // Not  Trunc(sqrt(sqrX)+0.5);
      if x2 = 0 then Inc(x2);
      //x2 := Round(sqrt(sqrX));
      x1 := Center.X - x2;
      x2 := Center.X + x2;

      if Contrast <> 0 then
       LineColor := Lighten(Clr32, (CY - y) * Contrast div H);
      {$IFDEF ALFA_BLEND}
       HorzLineTS(x1, y, x2, LineColor);
      {$ELSE}
       HorzLineS(x1, y, x2, LineColor);
      {$ENDIF}
     end;
   end;
 finally
  EMMS; // the low-level blending function was used EMMS is required
  ClipRect := SaveRect;
 end;
end;

procedure PaintTarget(Buffer: TBitmap32; const ARect: TRect;
  const Center: TPoint; Radius: Integer; const Clr32:  TColor32);
var SaveRect : TRect;
    x1, y, x2, y1, y2 : integer;
    XX: Double;
begin

 if GR32.IsRectEmpty(ARect) then Exit;
 if Radius <= 0 then Exit;
 if Radius > 46340 then Exit; // SqRt(2147483647)
 if Center.X > 46340 then Exit;
 if Center.Y > 46340 then Exit;


 with Buffer do
 try

  SaveRect := ClipRect;
  ClipRect := ARect;

  y1 := SaveRect.Top;
  y2 := SaveRect.Bottom - 1;

  Radius := sqr(Radius);

  for y := y1 to y2 do
   begin
    XX := Radius - sqr(y - Center.Y);

    if XX <= 0 then
     begin
      HorzLineS(ARect.Left, y, ARect.Right, Clr32);
      //Continue;
     end
    else
     begin
      x2 := Trunc(sqrt(XX)+0.5);
      x1 := Center.X - x2;
      HorzLineS(ARect.Left, y, x1, Clr32);
      x2 := Center.X + x2;
      HorzLineS(x2, y, ARect.Right, Clr32);
     end;
   end;
 finally
  ClipRect := SaveRect;
 end;
end;


procedure FillRoundCS(Buffer: TBitmap32;
  const Center: TPoint; Radius: Single;
  const Clr32: TColor32; Contrast: Integer; AA : boolean);
begin
  FillRoundCS(Buffer, Center, Radius,
    [Lighten(Clr32, Contrast div 2), Clr32, Lighten(Clr32, -Contrast div 2)],
    AA);
end;

procedure FillRoundCS(Buffer: TBitmap32; const Center: TPoint; Radius: Single;
  const Clrs32: array of TColor32; AA : boolean);
var
  x, y: integer;
  dist: single;
  pos : integer;
  len : integer;
  shift, temp: Extended;
  rct: TRect;
  Clr32: TColor32;
begin
   len := Length(Clrs32);
   if len = 0 then Exit;
   if Radius< 0 then Exit;

   IntersectRect(rct, Buffer.ClipRect,
        Rect(Floor(Center.X - Radius), Floor(Center.Y - Radius),
             Ceil(Center.X + Radius)+1, Ceil(Center.Y + Radius)+1)
        );

   // ξςπθρσεμ κπσγ
   if (rct.Right >= rct.Left) and (rct.Bottom >= rct.Top) then
     begin
       try
         with Buffer do
         // πΰρχες δθρςΰνφθθ
         for x:= rct.Left to rct.Right-1 do
          for y:= rct.Top to rct.Bottom-1 do
            begin
              {$R-}
              dist := Distance(Point(x, y), Center);
              if dist <= Radius then
                begin
                  temp  := dist / Radius * (len - 1);
                  pos   := Floor(temp);
                  shift := Frac(temp);

                  if (pos < len - 1)then
                    begin
                      // linear color interpolation
                      Clr32:= CombineReg(Clrs32[pos+1], Clrs32[pos], Trunc(shift * 255));
                    end
                  else
                    begin
                      Clr32 := Clrs32[len-1];
                    end;

                  BlendMem(Clr32, Bits[X + Y * Width]);
                  EMMS;
                end
                else if (dist < Radius + 1) and AA then
                begin
                  shift := Frac(dist - Radius);
                  if shift <> 0 then
                  begin
                    Clr32:= CombineReg(Clrs32[len-1] and $00FFFFFF, Clrs32[len-1], Floor(shift * 255));
                    BlendMem(Clr32, Bits[X + Y * Width]);
                    EMMS;
                  end;
                end;
               {$R+}
            end;
       finally
         EMMS; // the low-level blending function was used EMMS is required
       end;
     end;
end;

procedure FillMoireRect(Buffer: TBitmap32; Clr32: TColor32);
var
  Y, X, Row, Width, Height: integer;
  PBits: PColor32Array;
begin
  {$R-}
  PBits := Buffer.Bits;
  try
    Height := Buffer.Height;
    Width  := Buffer.Width - 1;

    Y := (Height - 1) - 1;
    while Y >= 0 do
    begin
      Row := Y * Buffer.Width;
      X := IfThen
           (
            (Width-1) mod 2 = 0, // width is even number
            (Width-1) div 2 * 2 - Y mod 2,// width is even number
            (Width-1) div 2 * 2 + Y mod 2 // width is odd number
           );
      while X >=0 do
      begin
        BlendMem(Clr32, PBits[X + Row]);
        dec(X, 2);
      end;
      dec(Y)
    end;
  finally
    EMMS;
  end;
  {$R+}
end;

procedure FillMoireRectS(Buffer: TBitmap32; Rect: TRect; Clr32: TColor32);
var
  Y, X, Row: integer;
  PBits: PColor32Array;
  ClipRect: TRect;
begin
  {$R-}
  PBits := Buffer.Bits;
  try
    ClipRect := Buffer.ClipRect;
    if Rect.Bottom > ClipRect.Bottom then Rect.Bottom := ClipRect.Bottom;
    if Rect.Right  > ClipRect.Right  then Rect.Right  := ClipRect.Right;
    if Rect.Top    < ClipRect.Top    then Rect.Top    := ClipRect.Top;
    if Rect.Left   < ClipRect.Left   then Rect.Left   := ClipRect.Left;
    //
    Y := Rect.Top;
    while Y < Rect.Bottom do
    begin
      Row := Y * Buffer.Width;
      X   := Rect.Left + (Y - Rect.Top) mod 2;
      while X < Rect.Right do
      begin
        BlendMem(Clr32, PBits[X + Row]);
        Inc(X, 2);
      end;
      Inc(Y);
    end;
  finally
    EMMS;
  end;
  {$R+}
end;

procedure FillGridRectS(Buffer: TBitmap32; Rect: TRect; Step: Integer; Clr32: TColor32);
var
  Y, X, Row: integer;
  PBits: PColor32Array;
  ClipRect: TRect;
begin
  {$R-}
  if Step <= 0 then Exit;
  //
  PBits := Buffer.Bits;
  try
    ClipRect := Buffer.ClipRect;
    if Rect.Bottom > ClipRect.Bottom then Rect.Bottom := ClipRect.Bottom;
    if Rect.Right  > ClipRect.Right  then Rect.Right  := ClipRect.Right;
    if Rect.Top    < ClipRect.Top    then Rect.Top    := ClipRect.Top;
    if Rect.Left   < ClipRect.Left   then Rect.Left   := ClipRect.Left;
    //
    Y := Rect.Top;
    while Y < Rect.Bottom do
    begin
      Row := Y * Buffer.Width;
      X   := Rect.Left;
      while X < Rect.Right do
      begin
        BlendMem(Clr32, PBits[X + Row]);
        Inc(X, Step);
      end;
      Inc(Y, Step);
    end;
  finally
    EMMS;
  end;
  {$R+}
end;


procedure FillRectS(Buffer: TBitmap32;const ARect: TRect;
        Clr32:  TColor32; Contrast: integer);
var
  LineColor: TColor32;
  I, CY, H: Integer;
begin
  if IsRectEmpty(ARect) then Exit;

  if Contrast <> 0 then
  begin
    with ARect do
    try
      H := Bottom - Top;
      CY := (Top + Bottom) div 2;
      for I := Top to Bottom - 1 do
      begin
        LineColor := Lighten(Clr32, (CY - I) * Contrast div H);
        {$IFDEF ALFA_BLEND}
         Buffer.HorzLineTS(Left, I, Right - 1, LineColor);
        {$ELSE}
         Buffer.HorzLineS(Left, I, Right - 1, LineColor);
        {$ENDIF}
      end;
    finally
      EMMS; // the low-level blending function was used EMMS is required
    end;
  end
  else Buffer.FillRectS(ARect, Clr32);
end;

procedure FillRoundVS(Buffer: TBitmap32;
 const Center: TPoint;
  Radius: Integer;
  Clr32: TColor32; Contrast : integer);
var SaveRect : TRect;
    y, y1, y2, x1, x2 : integer;
    sqrX : integer;
    LineColor: TColor32;
    CY, Hght: Integer;
    XFloat: Single;
begin
  if Radius <= 0 then Exit;
  if Radius > 46340 then Exit; // SqRt(2147483647)
  if Center.X > 46340 then Exit;
  if Center.Y > 46340 then Exit;

  with Buffer do
    try
      SaveRect := ClipRect;
      Hght := Radius * 2;
      CY := Center.Y;
      LineColor := Clr32;
      y1 := Center.Y - Radius;
      if y1 < SaveRect.Top then y1 := SaveRect.Top;
      y2 := Center.Y + Radius;
      if y2 > SaveRect.Bottom - 1 then y2 := SaveRect.Bottom - 1;
      Radius := sqr(Radius);
      for y := y1 to y2 do
      begin
        sqrX := Radius - sqr(y - Center.Y);
        if sqrX >= 0 then
        begin
          XFloat := sqrt(sqrX+0.001);
          x2 := Floor(XFloat + 0.5);
          if x2 = 0 then Inc(x2);
          x1 := Center.X - x2;
          x2 := Center.X + x2;

          if Contrast <> 0 then
           LineColor := Lighten(Clr32, (CY - y) * Contrast div Hght);
           {$IFDEF ALFA_BLEND}
             HorzLineTS(x1, y, x2, LineColor);
           {$ELSE}
             HorzLineS(x1, y, x2, LineColor);
           {$ENDIF}
        end;
      end;
    finally
      EMMS; // the low-level blending function was used EMMS is required
      ClipRect := SaveRect;
    end;
end;

procedure FillTargetS(Buffer: TBitmap32; const ARect: TRect;
  const Center: TPoint; Radius: Integer; const Clr32:  TColor32);
var SaveRect : TRect;
    x1, y, x2, y1, y2 : integer;
    XX: Double;
begin
  if GR32.IsRectEmpty(ARect) then Exit;
  if Radius <= 0 then Exit;
  if Radius > 46340 then Exit; // SqRt(2147483647)
  if Center.X > 46340 then Exit;
  if Center.Y > 46340 then Exit;

  with Buffer do
    try
      SaveRect := ClipRect;
      ClipRect := ARect;
      y1 := SaveRect.Top;
      y2 := SaveRect.Bottom - 1;
      Radius := sqr(Radius);

      for y := y1 to y2 do
       begin
        XX := Radius - sqr(y - Center.Y);
        if XX <= 0 then
         begin
          HorzLineS(ARect.Left, y, ARect.Right, Clr32);
         end
        else
         begin
          x2 := Trunc(sqrt(XX)+0.5);
          x1 := Center.X - x2;
          HorzLineS(ARect.Left, y, x1, Clr32);
          x2 := Center.X + x2;
          HorzLineS(x2, y, ARect.Right, Clr32);
         end;
       end;
    finally
      ClipRect := SaveRect;
    end;
end;

//------------------------------------------------------------------------------
procedure Ellipse(Buffer: TBitmap32; EllipseRect: TRect; AA: boolean;
    clLine32: TColor32; Fill: boolean; clFill32: TColor32);
var
    t1, t2, t3, t4, t5, t6, t7, t8, t9: integer;
    d1, d2, x, y: integer;
    rx, ry: integer;
    Center: TPoint;
    e: single;
begin
  Center.X := (EllipseRect.Right  + EllipseRect.Left) div 2;
  Center.Y := (EllipseRect.Bottom + EllipseRect.Top) div 2;
  rx := (EllipseRect.Right  - EllipseRect.Left) div 2;
  ry := (EllipseRect.Bottom - EllipseRect.Top) div 2;

  t1:= rx * rx;
  t2:= t1 * 2;
  t3:= t2 * 2;
  t4:= ry * ry;
  t5:= t4 * 2;
  t6:= t5 * 2;
  t7:= rx * t5;
  t8:= t7 * 2;
  t9:= 0;
  d1:= t2 - t7 + (t4 div 2);
  d2:=(t1 div 2) - t8 + t5;

  x:= rx;
  y:= 0;
  e:= rx;
  while (d2 < 0) do
  begin
    if not AA then
    //no antialias
    begin
      if Fill then
      begin
        Buffer.HorzLineS(Center.X-x, Center.Y+y, Center.X+x, clFill32);
        Buffer.HorzLineS(Center.X-x, Center.Y-y, Center.X+x, clFill32);
      end;
      Buffer.SetPixelTS(Center.X+x, Center.Y+y, clLine32);
      Buffer.SetPixelTS(Center.X+x, Center.Y-y, clLine32);
      Buffer.SetPixelTS(Center.X-x, Center.Y+y, clLine32);
      Buffer.SetPixelTS(Center.X-x, Center.Y-y, clLine32);
    end else
    begin
    //with antialias
      if Fill then
      begin
        Buffer.HorzLineS(Center.X-x+1, Center.Y+y, Center.X+x-1, clFill32);
        Buffer.HorzLineS(Center.X-x+1, Center.Y-y, Center.X+x-1, clFill32);
      end;
      Buffer.PixelFS[Center.X+e-1, Center.Y+y]:= clLine32;
      Buffer.PixelFS[Center.X+e-1, Center.Y-y]:= clLine32;
      Buffer.PixelFS[Center.X-e+1, Center.Y+y]:= clLine32;
      Buffer.PixelFS[Center.X-e+1, Center.Y-y]:= clLine32;
      e:= sqrt((t1 * t4 - t1 * y * y) / t4);
    end;

    t9:= t9+t3;
    inc(y);
    if d1<0 then
    begin
      d1:= d1+t9+t2;
      d2:= d2+t9;
    end else
    begin
      dec(x);
      t8:= t8-t6;
      d1:= d1+t9+t2-t8;
      d2:= d2+t9+t5-t8;
    end;
  end;

  while (x >= 0) do
  begin
    if not AA then
    begin
      Buffer.SetPixelTS(Center.X + x, Center.Y + y, clLine32);
      Buffer.SetPixelTS(Center.X + x, Center.Y - y, clLine32);
      Buffer.SetPixelTS(Center.X - x, Center.Y + y, clLine32);
      Buffer.SetPixelTS(Center.X - x, Center.Y - y, clLine32);
    end else
    begin
      e:=sqrt((t1*t4-t4*x*x)/t1);
      Buffer.PixelFS[Center.X + x, Center.Y + e]:= clLine32;
      Buffer.PixelFS[Center.X + x, Center.Y - e]:= clLine32;
      Buffer.PixelFS[Center.X - x, Center.Y + e]:= clLine32;
      Buffer.PixelFS[Center.X - x, Center.Y - e]:= clLine32;
    end;

    dec(x);

    t8:=t8-t6;
    if (d2<0) then
    begin
      inc(y);
      t9:=t9+t3;
      d2:=d2+t9+t5-t8;
    end else
    begin
      d2:=d2+t5-t8;
    end;
    if Fill then
    begin
      Buffer.VertLineS(Center.X+x, Center.Y-y+1, Center.Y+y-1, clFill32);
      Buffer.VertLineS(Center.X-x, Center.Y-y+1, Center.Y+y-1, clFill32);
    end;
  end;
end;

procedure Arc(Buffer:TBitmap32; aRect: TRect; aStart, aEnd : single;
              LineColor32: TColor32);
var
    Center: TPoint;
    Theta: Single;
    Step : Single;
    RadX, RadY: integer;
    Direction: boolean;
begin
  // if Radius <= 0 then Exit;
  Center.X := (aRect.Right+aRect.Left) div 2;
  Center.Y := (aRect.Top+aRect.Bottom) div 2;
  RadX     := (aRect.Right-aRect.Left) div 2;
  RadY     := (aRect.Bottom-aRect.Top) div 2;

  aStart := RadNormalize(aStart);
  aEnd   := RadNormalize(aEnd);
  Direction := aEnd > aStart;

  if IsZero( Sqrt(RadX * RadY), 0.1) then Exit;

  with Center, Buffer do
  begin
    if (RadX < Pi)or (RadY < Pi) then
         Step := 1 / Sqrt(RadX * RadY)
    else Step := Pi/ Sqrt(RadX * RadY);

    StippleStep := 1/2;
    SetStipple(LineColor32);

    if Direction then
    begin
      Theta := aStart;
      MoveToF(X + RadX * Cos(Theta), Y - RadY * Sin(Theta));
      while Theta < aEnd do
      begin
        LineToFSP(X + RadX * Cos(Theta), Y - RadY * Sin(Theta));
        Theta := Theta + Step;
      end;
      LineToFSP(X + RadX * Cos(aEnd), Y - RadY * Sin(aEnd));
    end
    else
    begin
      Theta := aStart;
      MoveToF(X + RadX * Cos(Theta), Y - RadY * Sin(Theta));
      while Theta < aEnd + 2 * Pi do
      begin
        LineToFSP(X + RadX * Cos(Theta), Y - RadY * Sin(Theta));
        Theta := Theta + Step;
      end;
      LineToFSP(X + RadX * Cos(aEnd), Y - RadY * Sin(aEnd));
    end;
  end;
end;

procedure Arc(Buffer: TBitmap32; Radius: integer; Center: TPoint;
    AngleStart, AngleEnd : single; LineColor32: TColor32);
begin
  Arc(Buffer,
      Rect(Center.X - Radius, Center.Y - Radius,
           Center.X + Radius, Center.Y + Radius),
      AngleStart, AngleEnd, LineColor32
      );
end;




end.

