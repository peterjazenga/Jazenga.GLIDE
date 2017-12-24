unit TplPanelTextureUnit;

{$MODE Delphi}

interface
 {$WARNINGS OFF}
uses
  LCLIntf, LCLType, LMessages, Forms, Types, Messages, SysUtils, Classes, Controls,
  ComCtrls, ExtCtrls, Graphics, Math, StdCtrls;

const
  MaxKernelSize = 100;
  MaxPixelCount = 32768;
  Version       = 'EZT14';
type
  TplMouseEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TRGBArray = array[0..32767] OF TRGBTriple;
  pRGBArray = ^TRGBArray;
  PRow = ^TRow;
  TRow = array[0..1000000] of TRGBTriple;
  PPRows = ^TPRows;
  TPRows = array[0..1000000] of PRow;
  TKernelSize = 1..MaxKernelSize;
  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of single;
  end;

  TTextureType = (ttNone,ttCenter,ttTile,ttStrech,ttGradient,ttBlend);
  TShadeAction = (saNone,saWindow,saText,saShape);
  TGradType    = (gtTopBottom,gtLeftRight,gtCircle,gtHorizontal,gtVertical,gtSquare);
  TShadeObject = record
     Name    : string[255];
     Act     : TShadeAction;
     Visible : byte;
     L,T,W,H : integer;
     Inherit : string[255];
  end;

  TShadeObjects = array of TShadeObject;

  TEZShades = Class(TMemoryStream);

  TplTextureSettings = Class(TPersistent)
  private
    FShadeHeight   : integer;
    FShadeDiffusion: integer;
    FShadeAngle    : integer;
    FShadeColor    : TColor;
    FSmooth        : boolean;
    FTexture       : TBitmap;
    FTType         : TTextureType;
    FScale         : double;
    FGrad1         : TColor;
    FGrad2         : TColor;
    FMove          : boolean;
    FGradType      : TGradType;
    FX0            : integer;
    FY0            : integer;
    FShadeShow     : boolean;
    FEZShape       : TBitmap;
    FSize          : boolean;
    EZShadeOb      : TShadeObjects;
    FPaint         : TNotifyEvent;
    FBCol          : TColor;
    FBlendCol      : TColor;
    FBlend         : byte;
    OldS           : boolean;
    OldT           : TTextureType;
  public
    BlockRepaint   : boolean;
    FRegion        : THandle;
    Hw             : HWND;
    function  CreateRegion(Bmp: TBitmap): THandle;
    procedure SetGradType(Value:TGradType);
    procedure SetOffsetX(Value:integer);
    procedure SetOffsety(Value:integer);
    procedure SetSize(Value:boolean);
    procedure SetShadeHeight(Value:integer);
    procedure SetShadeDiffusion(Value:integer);
    procedure SetShadeAngle(Value:integer);
    procedure SetShadeColor(Value:TColor);
    procedure SetShadeShow(Value:boolean);
    procedure SetShape(Value:TBitmap);
    procedure SetTexture(Value:TBitmap);
    procedure SetTType(Value:TTextureType);
    procedure SetSmooth(Value:boolean);
    procedure SetScale(Value:double);
    procedure SetGrad1(Value:TColor);
    procedure SetGrad2(Value:TColor);
    procedure SetBCol(Value:TColor);
    procedure SetBlendCol(Value:TColor);
    procedure SetBlend(Value:byte);
    procedure Region;
    constructor Create;
    destructor Destroy; override;
  published
    property AlphaBlendColor:TColor    read FBlendCol            write SetBlendCol;
    property AlphaBlend:byte           read FBlend               write SetBlend;
    property BorderColor:TColor        read FBCol                write SetBCol;
    property Moveable:boolean          read FMove                write FMove;
    property Sizeable:boolean          read FSize                write SetSize;
    property GradientColor1:TColor     read FGrad1               write SetGrad1;
    property GradientColor2:TColor     read FGrad2               write SetGrad2;
    property GradientType:TGradType    read FGradType            write SetGradType;
    property GradientX0:integer        read FX0                  write SetOffsetX;
    property GradientY0:integer        read FY0                  write SetOffsetY;
    property Texture:TBitmap           read FTexture             write SetTexture;
    property TextureType:TTextureType  read FTType               write SetTType;
    property TextureScale:double       read FScale               write SetScale;
    property TextureSmooth:boolean     read FSmooth              write SetSmooth;
    property ShadeAngle:integer        read FShadeAngle          write SetShadeAngle;
    property ShadeColor:TColor         read FShadeColor          write SetShadeColor;
    property ShadeDiffusion:integer    read FShadeDiffusion      write SetShadeDiffusion;
    property ShadeHeight:integer       read FShadeHeight         write SetShadeHeight;
    property ShadeShow:boolean         read FShadeShow           write SetShadeShow;
    property Shape:TBitmap             read FEZShape             write SetShape;
    property OnChange:TNotifyEvent     read FPaint               write FPaint;
  end;

  TplPanelTexture = class(TPanel)
  private
    Shades         : TEZShades;
    TempBmp        : TBitmap;
    Temp           : TBitmap;
    FMouseD        : TplMouseEvent;
    FMouseU        : TplMouseEvent;
    FBIn           : TPanelBevel;
    FBOut          : TPanelBevel;
    IsSizeing      : boolean;
    Pos            : TPoint;
    FCtl3D         : boolean;
    FCaption       : string;
    First          : boolean;
    Count          : integer;
    FColor         : TColor;
    FSettings      : TplTextureSettings;
    BackCopy       : boolean;
    function  GetShades:TEZShades;
    procedure SetShades(Value:TEZShades);
    procedure SetBevelIn(Value:TPanelBevel);
    procedure SetBevelOut(Value:TPanelBevel);
    procedure SetCtl3D(Value:boolean);
    procedure SetCaption(Value:string);
    procedure SetColor(Value:TColor);
    procedure DrawBorder;
    procedure WriteData(Stream : TStream);
    procedure ReadData(Stream : TStream);
    procedure DrawWindowShade(Obj:TObject);
    procedure DrawTextShade(Obj:TObject);
    procedure DrawShapeShade(Obj:TObject);
    procedure DrawShapeOwn(Obj:TObject);
    procedure DrawShade;
    procedure ApplyShade(L,T:integer;Bmp:TBitmap);
    procedure DrawGrad(Col1,Col2:TColor);
    procedure DrawChange(Sender:TObject);
    procedure BackToBmp;
    procedure BlendIt(Col:TColor;Alpha:byte;BlendBmp:TBitmap);
  protected
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure DefineProperties(Filer : TFiler); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Redraw;
    function  UpdateShadeObjects:boolean;
    procedure UpdateTexture;
    procedure UpdateOffspring;
    procedure SaveSettings(Stream:TMemoryStream);
    procedure LoadSettings(Stream:TMemoryStream);
    function  SetShade(CompName:string;Shaded:boolean):boolean;
    function  IsShaded(CompName:string):boolean;
  published
    property Shadeobjects:TEZShades      read GetShades            write SetShades;
    property Settings:TplTextureSettings read FSettings            write FSettings;
    property BevelInner: TPanelBevel     read FBIn                 write SetBevelIn;
    property BevelOuter: TPanelBevel     read FBOut                write SetBevelOut;
    property Caption:string              read FCaption             write SetCaption;
    property Color : TColor              read FColor               write SetColor;
    property Ctl3D:boolean               read FCtl3D               write SetCtl3D;
    property OnMouseDown:TplMouseEvent     read FMouseD              write FMouseD;
    property OnMouseUp:TplMouseEvent       read FMouseU              write FMouseU;
  end;

  procedure LoadShades(var ShadeObjects:TShadeObjects;Shades:TEZShades);
  procedure SaveShades(var Shades:TEZShades;ShadeObjects:TShadeObjects);

implementation

function Norm24BitCol(Col:TColor):TColor;                              // Returns the corresponding 24-bit color if Col is a system color (negative value)
begin
{$WARNINGS OFF}
  if (Col < 0) then Result := GetSysColor(Col+$80000000)
  else Result := Col;
{$WARNINGS ON}
end;
 
procedure StreamToBitmap(Bitmap:TBitmap;Stream:TMemoryStream);
var
 Sz,N : integer;
begin
 Stream.Read(Sz,SizeOf(integer));
 N := Stream.Position + Sz;                                                     // Get Position after loading bitmap
 if Sz > 0 then begin
    Bitmap.LoadFromStream(Stream);
    Stream.Position := N;                                                       // Set to good position
 end;
end;
 
procedure BitmapToStream(Bitmap:TBitmap;Stream:TMemoryStream);
var
 Temp : TMemoryStream;
 Sz   : integer;
begin
 Temp := TMemoryStream.Create;
 Bitmap.SaveToStream(Temp);
 Sz := Temp.Size;
 Temp.Free;
 Stream.Write(Sz,SizeOf(Integer));
 if Sz > 0 then Bitmap.SaveToStream(Stream);
end;
 
procedure StringToStream(Str:String;Stream:TMemoryStream);overload;
var
 C,I : byte;
begin
  C := Length(Str);
  Stream.write(C,SizeOf(byte));
  for I := 1 to C do Stream.write(Str[I],SizeOf(char));
end;
 
function StreamToString(Stream:TMemoryStream):string;overload;
var
 C,I : byte;
 Ch : char;
begin
  result := '';
  Stream.Read(C,SizeOf(byte));
  for I := 1 to C do begin
     Stream.Read(Ch,SizeOf(char));
     result := result + ch;
  end;
end;
 
function NewPos(X,Y,X0,Y0:Double;Angle:Double):TPoint;
var
 X1,Y1:Double;
 a : Double;
begin
 a := -DegToRad(Angle);
 X1 :=  X - X0;
 Y1 :=  Y - Y0;
 Result.X :=  Round( Cos(a)*(X1) + Sin(a)*(Y1) + X0);
 Result.Y :=  Round(-Sin(a)*(X1) + Cos(a)*(Y1) + Y0);
end;
 
procedure StringToStream(Str:String;Stream:TEZShades);overload;
var
 C,I : byte;
begin
  C := Length(Str);
  Stream.write(C,SizeOf(byte));
  for I := 1 to C do Stream.write(Str[I],SizeOf(char));
end;
 
function StreamToString(Stream:TEZShades):string;overload;
var
 C,I : byte;
 Ch : char;
begin
  result := '';
  Stream.Read(C,SizeOf(byte));
  for I := 1 to C do begin
     Stream.Read(Ch,SizeOf(char));
     result := result + ch;
  end;
end;
 
procedure LoadShades(var ShadeObjects:TShadeObjects;Shades:TEZShades);
var
  I,J : integer;
  C   : array [0..5] of Char;
  B   : Boolean;
begin
  Shades.Position := 0;

  Shades.Read(C,SizeOf(C));
  if (C = Version) then B := True else B := False;
  if not B then Shades.Position := 0;

  Shades.Read(J,SizeOf(Integer));
  SetLength(ShadeObjects,J);
  if J > 0 then for I := 0 to J-1 do begin
    ShadeObjects[I].Name := StreamToString(Shades);
    Shades.Read(ShadeObjects[I].Act,SizeOf(TShadeAction));
    Shades.Read(ShadeObjects[I].Visible,SizeOf(Byte));
    Shades.Read(ShadeObjects[I].L,SizeOf(Integer));
    Shades.Read(ShadeObjects[I].T,SizeOf(Integer));
    Shades.Read(ShadeObjects[I].W,SizeOf(Integer));
    Shades.Read(ShadeObjects[I].H,SizeOf(Integer));
    if B then ShadeObjects[I].Inherit := StreamToString(Shades);
  end;
end;
 
procedure SaveShades(var Shades:TEZShades;ShadeObjects:TShadeObjects);
var
  I,J : integer;
  C   : array [0..5] of Char;
begin
  Shades.Clear;
  Shades.Position := 0;
  C := Version;
  J := Length(ShadeObjects);
  Shades.Write(C,SizeOf(C));
  Shades.Write(J,SizeOf(Integer));
  if J > 0 then for I := 0 to J-1 do begin
    StringToStream(ShadeObjects[I].Name,Shades);
    Shades.Write(ShadeObjects[I].Act,SizeOf(TShadeAction));
    Shades.Write(ShadeObjects[I].Visible,SizeOf(Byte));
    Shades.Write(ShadeObjects[I].L,SizeOf(Integer));
    Shades.Write(ShadeObjects[I].T,SizeOf(Integer));
    Shades.Write(ShadeObjects[I].W,SizeOf(Integer));
    Shades.Write(ShadeObjects[I].H,SizeOf(Integer));
    StringToStream(ShadeObjects[I].Inherit,Shades);
  end;
end;
 
function TrimInt(i, Min, Max: Integer): Integer;
begin
  if      i>Max then Result:=Max
  else if i<Min then Result:=Min
  else               Result:=i;
end;
procedure MakeGaussianKernel(var K: TKernel; radius: double; MaxData, DataGranularity: double);
var
  j: integer;
  temp, delta: double;
  KernelSize: TKernelSize;
begin
  for j := Low(K.Weights) to High(K.Weights) do
  begin
    temp := j / radius;
    K.Weights[j] := exp(- temp*temp/2);
  end;
  temp := 0;
  for j := Low(K.Weights) to High(K.Weights) do
    temp := temp + K.Weights[j];
  for j := Low(K.Weights) to High(K.Weights) do
    K.Weights[j] := K.Weights[j] / temp;
  KernelSize := MaxKernelSize;
  delta := DataGranularity / (2 * MaxData);
  temp := 0;
  while (temp < delta) and (KernelSize > 1) do
  begin
    temp := temp + 2 * K.Weights[KernelSize];
    dec(KernelSize);
  end;
  K.Size := KernelSize;
  temp := 0;
  for j := - K.Size to K.Size do
    temp := temp + K.Weights[j];
  for j := - K.Size to K.Size do
    K.Weights[j] := K.Weights[j] / temp;
end;
{"Gaussian Blur in Delphi" from The Delphi Pool}
function TrimReal(Lower, Upper: integer; x: double): integer;
begin
  if (x < upper) and (x >= lower) then
    result:= trunc(x)
  else
  if x > Upper then
    result := Upper
  else
    result := Lower;
end;
{"Gaussian Blur in Delphi" from The Delphi Pool}
procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
var
  j, n: integer;
  tr, tg, tb: double;
  w: double;
begin
  for j := 0 to High(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;
    for n := - K.Size to K.Size do
    begin
      w := K.Weights[n];
      with theRow[TrimInt( j - n,0, High(theRow))] do
      begin
        tb := tb + w * rgbtBlue;
        tg := tg + w * rgbtGreen;
        tr := tr + w * rgbtRed;
      end;
    end;
    with P[j] do
    begin
      rgbtBlue := TrimReal(0, 255, tb);
      rgbtGreen := TrimReal(0, 255, tg);
      rgbtRed := TrimReal(0, 255, tr);
    end;
  end;
  Move(P[0], theRow[0], (High(theRow) + 1) * Sizeof(TRGBTriple));
end;
{"Gaussian Blur in Delphi" from The Delphi Pool}
procedure GBlur(theBitmap: TBitmap; radius: double);
var
  Row, Col: integer;
  theRows: PPRows;
  K: TKernel;
  ACol: PRow;
  P:PRow;
begin
  if (radius > 0.001) then begin
  if (theBitmap.HandleType <> bmDIB) or (theBitmap.PixelFormat <> pf24Bit) then
    raise exception.Create( 'GBlur only works for 24-bit bitmaps' );
  MakeGaussianKernel(K, radius, 255, 1);
  GetMem(theRows, theBitmap.Height * SizeOf(PRow));
  GetMem(ACol, theBitmap.Height * SizeOf(TRGBTriple));
  {record the location of the bitmap data:}
  for Row := 0 to theBitmap.Height - 1 do
    theRows[Row] := theBitmap.Scanline[Row];
    {blur each row:}
    P := AllocMem(theBitmap.Width * SizeOf(TRGBTriple));
  for Row := 0 to theBitmap.Height - 1 do
    BlurRow(Slice(theRows[Row]^, theBitmap.Width), K, P);
    {now blur each column}
    ReAllocMem(P, theBitmap.Height * SizeOf(TRGBTriple));
  for Col := 0 to theBitmap.Width - 1 do
  begin
    {first read the column into a TRow:}
    for Row := 0 to theBitmap.Height - 1 do
      ACol[Row] := theRows[Row][Col];
    BlurRow(Slice(ACol^, theBitmap.Height), K, P);
    {now put that row, um, column back into the data:}
    for Row := 0 to theBitmap.Height - 1 do
      theRows[Row][Col] := ACol[Row];
  end;
  FreeMem(theRows);
  FreeMem(ACol);
  ReAllocMem(P, 0);
end;
end;
 
procedure IntensityToGradient(var Pix:TRGBTriple;Intensity:Double;Min,Max:TColor);
begin
  Pix.rgbtRed   := Trunc(GetRValue(Min)*(1-Intensity)+GetRValue(Max)*Intensity);
  Pix.rgbtGreen := Trunc(GetGValue(Min)*(1-Intensity)+GetGValue(Max)*Intensity);
  Pix.rgbtBlue  := Trunc(GetBValue(Min)*(1-Intensity)+GetBValue(Max)*Intensity);
end;
 // SmoothResize has been obtained from the library janFX.pas, written by Jan Verhoeven (2-july-2000). Source: http://www.torry.net section Graphics - Effects
procedure SmoothResize(Src, Dst: TBitmap);
var
  x,y,xP,yP,yP2,xP2,t,t3,t13,z,z2,iz2,w1,w2,w3,w4:Integer;
  Read,Read2,pc:PByteArray;
  Col1r,col1g,col1b,Col2r,col2g,col2b:   byte;
begin
  if (Src.PixelFormat <> pf24Bit) or (Dst.PixelFormat <> pf24Bit) then Exit;    // small modification
  xP2:=((src.Width-1)shl 15)div Dst.Width;
  yP2:=((src.Height-1)shl 15)div Dst.Height;
  yP:=0;
  for y:=0 to Dst.Height-1 do
  begin
    xP:=0;
    Read:=src.ScanLine[yP shr 15];
    if yP shr 16<src.Height-1 then
      Read2:=src.ScanLine [yP shr 15+1]
    else
      Read2:=src.ScanLine [yP shr 15];
    pc:=Dst.scanline[y];
    z2:=yP and $7FFF;
    iz2:=$8000-z2;
    for x:=0 to Dst.Width-1 do
    begin
      t:=xP shr 15;
      t3:=t*3;
      t13:=t3+3;
      Col1r:=Read[t3];
      Col1g:=Read[t3+1];
      Col1b:=Read[t3+2];
      Col2r:=Read2[t3];
      Col2g:=Read2[t3+1];
      Col2b:=Read2[t3+2];
      z:=xP and $7FFF;
      w2:=(z*iz2)shr 15;
      w1:=iz2-w2;
      w4:=(z*z2)shr 15;
      w3:=z2-w4;
      pc[x*3+2]:=
        (Col1b*w1+Read[t13+2]*w2+
         Col2b*w3+Read2[t13+2]*w4)shr 15;
      pc[x*3+1]:=
        (Col1g*w1+Read[t13+1]*w2+
         Col2g*w3+Read2[t13+1]*w4)shr 15;
      pc[x*3]:=
        (Col1r*w1+Read2[t13]*w2+
         Col2r*w3+Read2[t13]*w4)shr 15;
      Inc(xP,xP2);
    end;
    Inc(yP,yP2);
  end;
end;
 
procedure OutputLabel2Canv(Owner:TWinControl;TxtLabel:TLabel;ImageHolder:TCanvas;Offset:integer);
var
  richedit_outputarea : TRect;
 // fmtRange            : TFormatRange;
 // RichHolder          : TRichEdit;
  Bmp                 : TBitmap;
begin
 if (TxtLabel.Caption <> '') then begin
   Bmp                    := TBitmap.Create;
   Bmp.PixelFormat        := pf24Bit;
   Bmp.Width              := TxtLabel.Width;
   Bmp.Height             := TxtLabel.Height;
   Bmp.Canvas.Brush.Color := clWhite;
   Bmp.Canvas.Brush.Style := bsSolid;
   Bmp.Canvas.FillRect(Rect(0,0,Bmp.Width,Bmp.Height));
 {
   if TxtLabel.WordWrap then begin
     RichHolder             := TRichEdit.Create(Owner);
     RichHolder.Left        := -4000;
     RichHolder.Visible     := False;
     RichHolder.Parent      := Owner;

     RichHolder.Lines.Add(TxtLabel.Caption);
     RichHolder.SelectAll;
     RichHolder.SelAttributes.Color := clBlack;
     RichHolder.SelAttributes.Style := TxtLabel.Font.Style;
     RichHolder.SelAttributes.Name  := TxtLabel.Font.Name;
     RichHolder.SelAttributes.Size  := TxtLabel.Font.Size;

     RichHolder.WordWrap    := True;
     RichHolder.Width       := TxtLabel.Width;
     RichHolder.Height      := TxtLabel.Height;

     FillChar(fmtRange, SizeOf(TFormatRange), 0);
     richedit_outputarea    := Rect(0,0,TxtLabel.Width*1440 div Screen.PixelsPerInch,TxtLabel.Height*1440 div Screen.PixelsPerInch);
     fmtRange.hDC           := Bmp.Canvas.Handle;
     fmtRange.hdcTarget     := Bmp.Canvas.Handle;
     fmtRange.rc            := richedit_outputarea;
     fmtRange.rcPage        := richedit_outputarea;
     fmtRange.chrg.cpMin    := 0;
     fmtRange.chrg.cpMax    := -1;
     SetMapMode(Bmp.Canvas.Handle, MM_TEXT);
     RichHolder.Perform(EM_FORMATRANGE,1,Longint(@fmtRange));
     RichHolder.Perform(EM_FORMATRANGE,0,0);

     RichHolder.Free;
   end else begin   }

     Bmp.Canvas.Font.Size  := TxtLabel.Font.Size;
     Bmp.Canvas.Font.Style := TxtLabel.Font.Style;
     Bmp.Canvas.Font.Name  := TxtLabel.Font.Name;
     Bmp.Canvas.Font.Color := clBlack;
     Bmp.Canvas.Brush.Color:= clWhite;
     Bmp.Canvas.TextOut(0,0,TxtLabel.Caption);
 //  end;
   ImageHolder.Draw(OffSet,OffSet,Bmp);
   Bmp.Free;
 end;
end;
////////////////////////////////////////////////////////////////////////////////
constructor TplTextureSettings.Create;
begin
  inherited Create;
  FTexture       := TBitmap.Create;
  FEZShape       := TBitmap.Create;
  FEZShape.PixelFormat := pf24Bit;
  FTType         := ttGradient;
  FGrad1         := clGray;
  FGrad2         := clBtnFace;
  FBCol          := clWindowFrame;
  FMove          := false;
  FSize          := false;
  FScale         := 1;
  FShadeHeight   := 5;
  FShadeDiffusion:= 3;
  FShadeAngle    := 315;
  FShadeColor    := clBlack;
  FShadeShow     := True;
  FGradType      := gtTopBottom;
  FBlendCol      := clBlack;
  FBlend         := 128;
  BlockRepaint   := False;
  SetLength(EZShadeOb,0);
end;
 
destructor TplTextureSettings.Destroy;
begin
  FTexture.free;
  FEZShape.Free;
  if (FRegion <> 0) then DeleteObject(FRegion);
  inherited Destroy;
end;
 
function TplTextureSettings.CreateRegion(Bmp: TBitmap): THandle;
var
  X, Y, StartX: Integer;
  Excl: THandle;
  Row: PRGBArray;
  TransparentColor: TRGBTriple;
begin
  Bmp.PixelFormat := pf24Bit;

  Result := CreateRectRGN(0, 0, Bmp.Width, Bmp.Height);

  for Y := 0 to Bmp.Height - 1 do
  begin
    Row := Bmp.Scanline[Y];

    StartX := -1;

    if Y = 0 then
      TransparentColor := Row[0];

    for X := 0 to Bmp.Width - 1 do
    begin
      if (Row[X].rgbtRed = TransparentColor.rgbtRed) and
        (Row[X].rgbtGreen = TransparentColor.rgbtGreen) and
        (Row[X].rgbtBlue = TransparentColor.rgbtBlue) then
      begin
        if StartX = -1 then StartX := X;
      end
      else
      begin
        if StartX > -1 then
        begin
          Excl := CreateRectRGN(StartX, Y, X + 1, Y + 1);
          try
            CombineRGN(Result, Result, Excl, RGN_DIFF);
            StartX := -1;
          finally
            DeleteObject(Excl);
          end;
        end;
      end;
    end;

    if StartX > -1 then
    begin
      Excl := CreateRectRGN(StartX, Y, Bmp.Width, Y + 1);
      try
        CombineRGN(Result, Result, Excl, RGN_DIFF);
      finally
        DeleteObject(Excl);
      end;
    end;
  end;
end;
 
procedure TplTextureSettings.Region;
begin
  if not FEZShape.Empty then begin
   { if (GetWindowRgn(Hw,FRegion) <> 0) then } DeleteObject(FRegion); // ct9999
    FRegion := CreateRegion(FEZShape);
    SetWindowRGN(Hw,FRegion,True);
  end else if (FEZShape.Empty) or (FRegion <> 0) then begin
    DeleteObject(FRegion);
    SetWindowRGN(Hw,0,True);
  end;
end;
 
procedure TplTextureSettings.SetOffsetX(Value:integer);
begin
  FX0 := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetOffsetY(Value:integer);
begin
  FY0 := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetGradType(Value:TGradType);
begin
  FGradType := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetTexture(Value:TBitmap);
begin
  FTexture.Assign(Value);
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetTType(Value:TTextureType);
begin
  OldT         := FTType;
  OldS       := FShadeShow;
  if (Value = ttBlend) then FShadeShow := False
  else FShadeShow := OldS;

  FTType := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetSmooth(Value:boolean);
begin
  FSmooth := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetGrad1(Value:TColor);
begin
  FGrad1 := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetBCol(Value:TColor);
begin
  FBCol := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetBlendCol(Value:TColor);
begin
  FBlendCol := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetBlend(Value:byte);
begin
  FBlend := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetGrad2(Value:TColor);
begin
  FGrad2 := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetScale(Value:double);
begin
  if (Value < 0.2) then FScale := 0.2
  else if (Value > 5)  then FScale := 5
  else FScale := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetShadeHeight(Value:integer);
begin
  if (Value > -1) and (Value < 100) then
  FShadeHeight := value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetShadeDiffusion(Value:integer);
begin
  if (Value > -1) and (Value < 10) then
  FShadeDiffusion := value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetShadeAngle(Value:integer);
begin
  if (Value > -1) and (value < 360) then
  FShadeAngle := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetShadeColor(Value:TColor);
begin
  FShadeColor := Value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetShape(Value:TBitmap);
begin
  FEZShape.Assign(Value);
  Region;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetSize(Value:boolean);
begin
  FSize := value;
  if Assigned(FPaint) then FPaint(Self);
end;
 
procedure TplTextureSettings.SetShadeShow(Value:boolean);
begin
  if (FTType = ttBlend) then FShadeShow := False
  else FShadeShow := value;
  if Assigned(FPaint) then FPaint(Self);
end;
////////////////////////////////////////////////////////////////////////////////
constructor TplPanelTexture.Create(AOwner: TComponent);
var
  J : integer;
begin
  inherited Create(AOwner);

  TempBmp              := TBitmap.Create;
  TempBmp.PixelFormat  := pf24Bit;
  Temp                 := TBitmap.Create;
  Temp.PixelFormat     := pf24Bit;

  Shades         := TEZShades.Create;
  Shades.Clear;
  J              := 0;
  Shades.Write(J,SizeOf(integer));
  Height         := 100;
  Width          := 228;
//  DoubleBuffered := false;
  FBIn           := bvNone;
  FBOut          := bvRaised;
  FCtl3D         := True;
  FCaption       := Name;
  FColor         := clBtnFace;
  FSettings      := TplTextureSettings.Create;
  FSettings.FX0  := Width div 2;
  FSettings.FY0  := Height div 2;
  FSettings.OnChange := DrawChange;
  First          := true;
  Count          := 0;
  BackCopy       := True;
end;
 
destructor TplPanelTexture.Destroy;
begin
  TempBmp.Free;
  Temp.Free;
  Shades.Free;
  FSettings.Free;
  inherited Destroy;
end;
 
procedure TplPanelTexture.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := LRESULT(False);
end;
 
procedure TplPanelTexture.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  //-------
  procedure DoEvent;
  begin
    if FSettings.FSize and (X >= Width-8) and (Y >= Height-8) then begin
       IsSizeing := true;
       Pos := Point(X,Y);
    end;
    if Assigned(FMouseD) then FMouseD(Self,Button,Shift,X,Y);
  end;
  //--------
const
  SC_DragMove = $F012;
begin
  if FSettings.FMove and not (FSettings.FSize and (X >= Width-8) and (Y >= Height-8)) then begin
    ReleaseCapture;
    Perform(LM_SYSCOMMAND, SC_DragMove, 0);
    DoEvent;
    DrawChange(Self);
    UpdateOffspring;
    if Assigned(FMouseU) then FMouseU(Self,Button,Shift,X,Y);
  end else DoEvent;
end;
 
procedure TplPanelTexture.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FSettings.FSize and IsSizeing then begin
     if (Width + X-Pos.X > 0) then Width  := Width + X-Pos.X;
     if (Height + Y-Pos.Y> 0) then Height := Height + Y-Pos.Y;
     IsSizeing := false;
     TWinControl(Parent).Refresh;
     DrawChange(Self);
  end;
  if Assigned(FMouseU) then FMouseU(Self,Button,Shift,X,Y);
end;
 
procedure TplPanelTexture.WriteData;
var
  ASize : longint;
begin
  ASize := Shades.Size;
  Stream.Write (ASize, sizeof (ASize));
  if ASize > 0 then Stream.Write ((Shades as TEZShades).Memory^, ASize);
end;
 
procedure TplPanelTexture.DefineProperties;
begin
  inherited;
  Filer.DefineBinaryProperty('TheData',ReadData, WriteData, true);
end;
 
procedure TplPanelTexture.ReadData;
var
  ASize : longint;
begin
  Stream.Read(ASize, sizeof (ASize));
  if ASize > 0 then begin 
      (Shades as TEZShades).SetSize (ASize);
      Stream.Read((Shades as TEZShades).Memory^, ASize);
  end;
end;
 
function TplPanelTexture.GetShades:TEZShades;
begin
  result := Shades;
end;
 
procedure TplPanelTexture.SetShades(Value:TEZShades);
begin
  if (csDesigning in ComponentState) then begin
    if (Value <> nil) then (Shades as TEZShades).LoadFromStream(Value)
    else (Shades as TEZShades).SetSize (0);
  end;
end;
 
procedure TplPanelTexture.SetColor(Value:TColor);
begin
  FColor := Value;
  UpdateTexture;
  Invalidate;
end;
 
procedure TplPanelTexture.SetBevelIn(Value:TPanelBevel);
begin
  FBIn := Value;
  UpdateTexture;
  Invalidate;
end;
 
procedure TplPanelTexture.SetBevelOut(Value:TPanelBevel);
begin
  FBOut := Value;
  UpdateTexture;
  Invalidate;
end;
 
procedure TplPanelTexture.SetCtl3D(Value:boolean);
begin
  FCtl3D := value;
  UpdateTexture;
  Invalidate;
end;
 
procedure TplPanelTexture.SetCaption(Value:String);
begin
  FCaption := value;
  UpdateTexture;
  Invalidate;
end;
 
procedure TplPanelTexture.Resize;
begin
  if not IsSizeing then begin
    UpdateTexture;
    Invalidate;
  end;
end;
 
procedure TplPanelTexture.ReDraw;
begin
  UpdateTexture;
  Invalidate;
end;
 
procedure TplPanelTexture.DrawChange(Sender:TObject);
begin
//  with Owner as TControl do begin Redraw; end;
  if (FSettings.FTType = ttBlend) and (FSettings.OldT <> FSettings.FTType) then BackCopy := True
  else BackCopy := False;
  FSettings.OldT := FSettings.FTType;
  if not First and not FSettings.BlockRepaint then begin
    UpdateTexture;
    Invalidate;
  end;
end;
 
procedure TplPanelTexture.Paint;
begin
  Inc(Count);
  if (FSettings.Hw <> Handle) then begin
    FSettings.Hw := Handle;
    FSettings.Region;
  end else FSettings.Hw := Handle;
  if First or UpdateShadeObjects then begin
    Invalidate;
    UpdateShadeObjects;
    UpdateTexture;
  end else begin
    Canvas.Draw(0,0,Temp);
  end;
  First := false;
end;
 
function TplPanelTexture.UpdateShadeObjects:boolean;
  //------
  function Shadable(S:string):boolean;
  begin
    if (S = 'TMenuItem') or (S = 'TMenu') or (S = 'TMainMenu') or (S = 'TSplitter') or (S = 'TBevel') then Result := False
    else Result := True;
  end;
  //------
  function UseInherit(Comp:TComponent;ClassN:string):string;
  begin
    Result := 'None';
    if Shadable(ClassN) then begin
      if      Comp.InheritsFrom(TShape)       then Result := 'Shape'
      else if Comp.InheritsFrom(TLabel)       then Result := 'Label'
      else if Comp.InheritsFrom(TControl)     then Result := 'Window';
      if (Comp.ClassName = 'TplPanelTexture') then Result := 'EZTex';
    end;
  end;
  //------
  procedure NoBlendeShade(TS:TShadeObjects);
  var
    I : integer;
    TT : TTextureType;
  begin
    for I := 0 to Length(TS)-1 do begin
       if (Owner.FindComponent(TS[I].Name).ClassName = 'TplPanelTexture') then begin
          with Owner.FindComponent(TS[I].Name) as TplPanelTexture do TT := Settings.TextureType;
          if (TT = ttBlend) then TS[I].Act := saNone;
       end;
    end;
  end;
  //------
  function CheckShadeAction(CompName:string;CurrAct:TShadeAction):TShadeAction;
  var
    S : string;
  begin
    S := Owner.FindComponent(CompName).ClassName;
    if (CurrAct = saWindow) or (CurrAct = saNone) then Result := CurrAct else Result := saNone;
    if (S = 'TplPanelTexture') then if (CurrAct <> saText)  then result := CurrAct else Result := saNone;
    if (S = 'TShape')          then if (CurrAct <> saText)  then result := CurrAct else Result := saNone;
    if (S = 'TLabel')          then if (CurrAct <> saShape) then result := CurrAct else Result := saNone;
  end;
  //------
var
  I,J : integer;
  S1,S2,S3 : string;
  TS : TShadeObjects;
begin
  Result := false;
  LoadShades(FSettings.EZShadeOb,Shades);
  SetLength(TS,0);
  for I := 1 to Owner.ComponentCount-1 do begin
    S3 := Owner.Components[I].ClassName;
    if Owner.Components[I].HasParent then begin
      S1 := Owner.Components[I].Name;
      with Owner.Components[I] as TControl do
      S2 := Parent.Name;
      if (S2 = Self.Name) then with Owner.Components[I] as TControl do begin
        J := Length(TS);
        SetLength(TS,J+1);
        TS[J].Name    := S1;
        TS[J].Act     := saNone;
        if Visible then TS[J].Visible := 1 else TS[J].Visible := 0;
        TS[J].L       := Left;
        TS[J].T       := Top;
        TS[J].W       := Width;
        TS[J].H       := Height;
        TS[J].Inherit := UseInherit(Owner.Components[I],S3);
      end;
    end;
  end;

  if (Length(FSettings.EZShadeOb) <> Length(TS)) then Result := true;

  for I := 0 to Length(FSettings.EZShadeOb)-1 do for J := 0 to Length(TS)-1 do
  if (FSettings.EZShadeOb[I].Name = TS[J].Name) then begin
     TS[J].Act     := CheckShadeAction(FSettings.EZShadeOb[I].Name,FSettings.EZShadeOb[I].Act);
     if (FSettings.EZShadeOb[I].Act <> saNone) then begin
       if (TS[J].Visible <> FSettings.EZShadeOb[I].Visible) then Result := true;
       if (TS[J].L       <> FSettings.EZShadeOb[I].L      ) then Result := true;
       if (TS[J].T       <> FSettings.EZShadeOb[I].T      ) then Result := true;
       if (TS[J].W       <> FSettings.EZShadeOb[I].W      ) then Result := true;
       if (TS[J].H       <> FSettings.EZShadeOb[I].H      ) then Result := true;
     end;
  end;
  NoBlendeShade(TS);
  SaveShades(Shades,TS);
  LoadShades(FSettings.EZShadeOb,Shades);
end;
 
procedure TplPanelTexture.BackToBmp;
var
  BlendBmp:TBitmap;
  DC: hDC;
  W,H: Integer;
  Hw : HWND;
  Vis : boolean;
begin
  if not (csDesigning in ComponentState) or BackCopy then begin
    BackCopy := False;
    W := Width;
    H := Height;

    BlendBmp                    := TBitmap.Create;
    BlendBmp.PixelFormat        := pf24Bit;
    BlendBmp.Width              := W;
    BlendBmp.Height             := H;
    BlendBmp.Canvas.Brush.Style := bsSolid;
    BlendBmp.Canvas.Brush.Color := clBlack;
    BlendBmp.Canvas.FillRect(Rect(0,0,Width,Height));

    Hw                          := TWinControl(Parent).Handle;
    SetActiveWindow(Hw);
    GetDC(Hw);
    Vis                         := Visible;
    Visible                     := False;
    TWinControl(Parent).Refresh;
    DC                          := GetDC(Handle);
    BitBlt(BlendBmp.Canvas.Handle,0,0,W,H,DC,0,0,SrcCopy);
    ReleaseDC(Handle, DC);
    Visible                     := Vis;
    BlendIt(FSettings.FBlendCol,255-FSettings.FBlend,BlendBmp);
    BlendBmp.Free;
  end;
end;
 
procedure TplPanelTexture.BlendIt(Col:TColor;Alpha:byte;BlendBmp:TBitmap);
var
 Bl,Tm : PRGBArray;
 X,Y   : integer;
begin
  Temp.Width := Width;
  Temp.Height:= Height;
  for Y := 0 to Height - 1 do begin
    Bl := BlendBmp.ScanLine[Y];
    Tm := Temp.ScanLine[Y];
    for X := 0 to Width - 1 do begin
       Tm[X].rgbtRed := (Alpha * Bl[X].rgbtRed + (255 - Alpha) * GetRValue(Col)) div 255;
       Tm[X].rgbtGreen := (Alpha * Bl[X].rgbtGreen + (255 - Alpha)* GetGValue(Col)) div 255;
       Tm[X].rgbtBlue := (Alpha * Bl[X].rgbtBlue + (255 - Alpha) * GetBValue(Col)) div 255;
     end
   end;
end;
 
procedure TplPanelTexture.DrawBorder;
var
  Re1,Re2 : TRect;
  Col1,Col2,Col3,Col4:TColor;
  w : integer;
begin
  Canvas.Pen.Style := psSolid;
  w   := BevelWidth;
  Col1 := FColor;Col2 := FColor;Col3 := FColor;Col4 := FColor;
  case BevelOuter of
      bvNone    : case BevelInner of
                    bvNone    : begin Col1 := FColor; Col2 := FColor; Col3 := FColor; Col4 := FColor end;
                    bvLowered : begin Col1 := clBtnShadow; Col2 := clBtnHighlight; Col3 := FColor; Col4 := FColor end;
                    bvSpace   : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := FColor; Col4 := FColor end;
                    bvRaised  : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := FColor; Col4 := FColor end;
                  end;
      bvLowered : case BevelInner of
                    bvNone    : begin Col1 := clBtnShadow; Col2 := clBtnHighlight; Col3 := FColor; Col4 := FColor end;
                    bvLowered : begin Col1 := clBtnShadow; Col2 := clBtnHighlight; Col3 := clBtnShadow; Col4 := clBtnHighlight end;
                    bvSpace   : begin Col1 := clBtnShadow; Col2 := clBtnHighlight; Col3 := clBtnHighlight; Col4 := clBtnShadow end;
                    bvRaised  : begin Col1 := clBtnShadow; Col2 := clBtnHighlight; Col3 := clBtnHighlight; Col4 := clBtnShadow end;
                  end;
      bvSpace   : case BevelInner of
                    bvNone    : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := FColor; Col4 := FColor end;
                    bvLowered : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := clBtnShadow; Col4 := clBtnHighlight end;
                    bvSpace   : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := clBtnHighlight; Col4 := clBtnShadow end;
                    bvRaised  : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := clBtnHighlight; Col4 := clBtnShadow end;
                  end;
      bvRaised  : case BevelInner of
                    bvNone    : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := FColor; Col4 := FColor end;
                    bvLowered : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := clBtnShadow; Col4 := clBtnHighlight end;
                    bvSpace   : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := clBtnHighlight; Col4 := clBtnShadow end;
                    bvRaised  : begin Col1 := clBtnHighlight; Col2 := clBtnShadow; Col3 := clBtnHighlight; Col4 := clBtnShadow end;
                  end;
  end;

  Re1 := Rect(0,0,Width,Height);
  Re2 := Rect(w,w,Width-w,Height-w);
  if (BevelOuter <> bvNone) and (Col3 = FColor) and (Col4 = FColor) then Frame3D(Temp.Canvas,Re1,Col1,Col2,w);
  if (Col3 <> FColor) and (Col4 <> FColor) then Frame3D(Temp.Canvas,Re2,Col3,Col4,w);
end;
 
procedure TplPanelTexture.UpdateOffspring;
var
  I : integer;
  TT : TTextureType;
begin
  if (FSettings.FTType = ttBlend) then begin
    for I := 1 to Owner.ComponentCount-1 do begin
       if (Owner.FindComponent(Owner.Components[I].Name).ClassName = 'TplPanelTexture') then begin
          with Owner.FindComponent(Owner.Components[I].Name) as TplPanelTexture do TT := Settings.TextureType;
          if (TT = ttBlend) then with Owner.FindComponent(Owner.Components[I].Name) as TplPanelTexture do UpdateTexture;
       end;
    end;
  end;
end;
 
procedure TplPanelTexture.UpdateTexture;
var
  X,Y : integer;
  Re1 : TRect;
begin
  if (Count > 0) then begin
    Temp.Width := Width;
    Temp.Height := Height;
    FSettings.Region;
    if (not Assigned(FSettings.FTexture) or (FSettings.FTexture.Empty)) and ((FSettings.FTType <> ttGradient) and (FSettings.FTType <> ttBlend)) then FSettings.FTtype := ttNone;
    if (FSettings.FTexture.PixelFormat <> pf24Bit) then FSettings.FTexture.PixelFormat := pf24Bit;
    if (TempBmp.PixelFormat <> pf24Bit) then TempBmp.PixelFormat := pf24Bit;
    if (FSettings.FTType <> ttNone) and (FSettings.FTType <> ttGradient) then begin
      if (FSettings.FScale = 1) then begin
         TempBmp.Width  := FSettings.FTexture.Width;
         TempBmp.Height := FSettings.FTexture.Height;
         TempBmp.Canvas.StretchDraw(Rect(0,0,FSettings.FTexture.Width,FSettings.FTexture.Height),FSettings.FTexture);
      end else if FSettings.FSmooth then begin
         TempBmp.Width  := Trunc(FSettings.FTexture.Width*FSettings.FScale);
         TempBmp.Height := Trunc(FSettings.FTexture.Height*FSettings.FScale);
         SmoothResize(FSettings.FTexture,TempBmp);
      end else begin
         TempBmp.Width  := Trunc(FSettings.FTexture.Width*FSettings.FScale);
         TempBmp.Height := Trunc(FSettings.FTexture.Height*FSettings.FScale);
         TempBmp.Canvas.StretchDraw(Rect(0,0,TempBmp.Width,TempBmp.Height),FSettings.FTexture);
      end;
    end;
    Case FSettings.FTType of
      ttTile  : begin
                  if (TempBmp.Width > 0) and (TempBmp.Height > 0) then
                  for X := 0 to 1 + Width div TempBmp.Width do
                  for Y := 0 to 1 + Height div TempBmp.Height do
                    Temp.Canvas.Draw(X*TempBmp.Width,Y*TempBmp.Height,TempBmp);
                end;
      ttCenter: begin
                  Temp.Canvas.Brush.Style := bsSolid;
                  Temp.Canvas.Brush.Color := FColor;
                  Temp.Canvas.FillRect(Rect(0,0,Width,Height));
                  Temp.Canvas.Draw((Width - TempBmp.Width) div 2,(Height - TempBmp.Height) div 2,TempBmp);
                end;
      ttStrech: begin
                  if FSettings.FSmooth then begin
                    SmoothResize(FSettings.FTexture,Temp);
                  end else Temp.Canvas.StretchDraw(Rect(0,0,Width,Height),FSettings.FTexture);
                end;
      ttNone  : begin
                  Temp.Canvas.Brush.Style := bsSolid;
                  Temp.Canvas.Brush.Color := FColor;
                  Temp.Canvas.FillRect(Rect(0,0,Width,Height));
                end;
      ttGradient : DrawGrad(FSettings.FGrad1,FSettings.FGrad2);
      ttBlend : if not (csDesigning in ComponentState) then BackToBmp else begin
                  Temp.Canvas.Brush.Style := bsSolid;
                  Temp.Canvas.Brush.Color := clBtnFace;
                  Temp.Canvas.Rectangle(0,0,Width,Height);
                  Temp.Canvas.Brush.Style := bsDiagCross;
                  Temp.Canvas.Brush.Color := FSettings.FBlendCol;
                  Temp.Canvas.Rectangle(0,0,Width,Height);
                end;
    end;

    Temp.Canvas.Brush.Style := bsClear;
    Temp.Canvas.Font        := Font;
    Temp.Canvas.TextOut((Width - Canvas.TextWidth(Caption)) div 2,(Height - Canvas.TextHeight(Caption)) div 2,Caption);

    Temp.Canvas.Pen.Color := FSettings.FBCol;
    if Ctl3D then DrawBorder else Temp.Canvas.Rectangle(Rect(0,0,Width,Height));
    if FSettings.FSize then begin
      Temp.Canvas.Brush.Style := bsSolid;
      Temp.Canvas.Brush.Color := clBtnFace;
      Temp.Canvas.FillRect(Rect(Width-8,Height-8,Width,Height));
      Temp.Canvas.Pen.Color := FSettings.FBCol;
      Re1 := Rect(Width-8,Height-8,Width,Height);
      if Ctl3D then Frame3D(Temp.Canvas,Re1,clBtnHighlight,clBtnShadow,1)
      else Temp.Canvas.Rectangle(Re1);
    end;

    if FSettings.FShadeShow then DrawShade;
  end;
end;
 
procedure TplPanelTexture.DrawGrad(Col1,Col2:TColor);
  //---------
  function SquareDis(X,Y:double):double;
    //---------
    function SideDis(Xa,Ya,Xb,Yb,Xc,Yc:double):Double;
    var
      L : double;
    begin
      Result := 0;
      L  :=  SQRT(((XB-XA)*(XB-XA)+(YB-YA)*(YB-YA)));
      if (L <> 0) then Result := ((YA-YC)*(XB-XA)-(XA-XC)*(YB-YA))/(L*L);
    end;
    //---------
    function SaveAbsDiv(C,D:double):double;
    begin
      if (abs(D) < 1E-3) then Result := 0 else Result := Abs(C/D);
    end;
    //---------
  var
    D1,D2,D3,D4 : Double;
  begin
    D1 := SideDis(0,0,FSettings.FX0,FSettings.FY0,X,Y);
    D2 := SideDis(Width,0,FSettings.FX0,FSettings.FY0,X,Y);
    D3 := SideDis(0,Height,FSettings.FX0,FSettings.FY0,X,Y);
    D4 := SideDis(Width,Height,FSettings.FX0,FSettings.FY0,X,Y);

    Result := 0;
    if (D1 >= 0) and (D2 <= 0) then Result := 1-SaveAbsDiv(Y,(FSettings.FY0))
    else if (D1 <= 0) and (D3 >= 0) then Result := 1-SaveAbsDiv(X,(FSettings.FX0))
    else if (D2 >= 0) and (D4 <= 0) then Result := SaveAbsDiv((FSettings.FX0-X),(Width-FSettings.FX0))
    else if (D3 <= 0) and (D4 >= 0) then Result := SaveAbsDiv((FSettings.FY0-Y),(Height-FSettings.FY0));
  end;
  //---------
  function MaxDis:integer;
  Var
    D : array [1..4] of Double;
    I : integer;
  begin
    D[1] := Sqrt(sqr(0-FSettings.FX0)+sqr(0-FSettings.FY0));
    D[2] := Sqrt(sqr(Width-FSettings.FX0)+sqr(0-FSettings.FY0));
    D[3] := Sqrt(sqr(0-FSettings.FX0)+sqr(Height-FSettings.FY0));
    D[4] := Sqrt(sqr(Width-FSettings.FX0)+sqr(Height-FSettings.FY0));
    Result := 0;
    for I := 1 to 4 do if (Round(D[I]) > Result) then Result := Round(D[I]);
  end;
  //---------
var
  X,Y,XD,YD,M : integer;
  Row : pRGBArray;
  Col :TRGBTriple;
begin
  if (Col1 < 0) then Col1 := Norm24BitCol(Col1);
  if (Col2 < 0) then Col2 := Norm24BitCol(Col2);
  XD := FSettings.FX0; if (XD < Width  div 2) then XD := Width - FSettings.FX0;
  YD := FSettings.FY0; if (YD < Height div 2) then YD := Height - FSettings.FY0;
  M := MaxDis;
  for Y := 0 to Temp.Height-1 do begin
    Row := Temp.ScanLine[Y];
    for X := 0 to Temp.Width-1 do begin
      case FSettings.FGradType of
         gtLeftRight : IntensityToGradient(Col,X/Width,Col1,Col2);
         gtTopBottom : IntensityToGradient(Col,Y/Height,Col1,Col2);
         gtCircle    : IntensityToGradient(Col,sqrt(sqr(X-FSettings.FX0)+sqr(Y-FSettings.FY0))/M,Col2,Col1);
         gtHorizontal: IntensityToGradient(Col,abs(Y-FSettings.FY0)/(YD),Col2,Col1);
         gtVertical  : IntensityToGradient(Col,abs(X-FSettings.FX0)/(XD),Col2,Col1);
         gtSquare    : IntensityToGradient(Col,SquareDis(X,Y),Col2,Col1);
      end;
      Row[X] := Col;
    end;
  end;
end;
 
procedure TplPanelTexture.DrawShade;
var
 I : integer;
begin
 for I := 0 to Length(FSettings.EZShadeOb)-1 do begin
    if (FSettings.EZShadeOb[I].Visible = 1) then
    case FSettings.EZShadeOb[I].Act of
      saWindow  : DrawWindowShade(Owner.FindComponent(FSettings.EZShadeOb[I].Name));
      saText    : DrawTextShade(Owner.FindComponent(FSettings.EZShadeOb[I].Name));
      saShape   : if (Owner.FindComponent(FSettings.EZShadeOb[I].Name).ClassName = 'TplPanelTexture')
                  then DrawShapeOwn(Owner.FindComponent(FSettings.EZShadeOb[I].Name))
                  else DrawShapeShade(Owner.FindComponent(FSettings.EZShadeOb[I].Name));
    end;
 end;
end;
 
function TplPanelTexture.SetShade(CompName:string;Shaded:boolean):boolean;
Var
 I,C : integer;
 A   : TShadeAction;
 //-------
 function GetAct(Index:integer):TShadeAction;
 var
   S : string;
 begin
   S := FSettings.EZShadeOb[Index].Inherit;
   if (FSettings.EZShadeOb[C].Visible = 1) then begin
     if      (S = 'Window') then Result := saWindow
     else if (S = 'Shape')  then Result := saShape
     else if (S = 'Label')  then Result := saText
     else if (S = 'EZTex')  then Result := saShape
     else Result := saNone;
   end else Result := saNone;
 end;
 //-------
Begin
 LoadShades(FSettings.EZShadeOb,Shades);
 C := -1;
 for I := 0 to Length(FSettings.EZShadeOb)-1 do
  if FSettings.EZShadeOb[I].Name = CompName then C := I;

 if (C > -1) then begin
   A := GetAct(C);
   if (A = saNone) then Result := False else Result := True;
   if Shaded then FSettings.EZShadeOb[C].Act := A
   else FSettings.EZShadeOb[C].Act := saNone;
 end else Result := False;

 SaveShades(Shades,FSettings.EZShadeOb);
 ReDraw;
end;
 
function  TplPanelTexture.IsShaded(CompName:string):boolean;
Var
 I,C : integer;
begin
  C := -1;
  Result := False;

  for I := 0 to Length(FSettings.EZShadeOb)-1 do
    if FSettings.EZShadeOb[I].Name = CompName then C := I;
  if (C > -1) then if (FSettings.EZShadeOb[C].Act <> saNone) then Result := True;
end;
 
procedure TplPanelTexture.ApplyShade(L,T:integer;Bmp:TBitmap);
var
  X,Y,Br : integer;
  Pos : TPoint;
  Row,RowT : pRGBArray;
  Am,An : double;
  Col : TColor;
begin
  Br  := Round(FSettings.FShadeDiffusion)*2;
  if (FSettings.FShadeHeight > 0) then Pos := NewPos(0,FSettings.FShadeHeight,0,0,FSettings.FShadeAngle) else Pos := Point(0,0);
  Col := Norm24BitCol(FSettings.FShadeColor);
  for Y := 0 to Bmp.Height-1 do if (Y+T+Pos.Y-Br >= 0) and (Y+T+Pos.Y-Br < Height) then begin
      Row  := Bmp.ScanLine[Y];
      RowT := Self.Temp.ScanLine[Y+T+Pos.Y-Br];
      for X :=0 to Bmp.Width-1 do if (X+L+Pos.X-Br >= 0) and (X+L+Pos.X-Br < Width) then begin
          Am := Row[X].rgbtRed/255;
          An := 1-Row[X].rgbtRed/255;
          with RowT[X+L+Pos.X-Br] do begin
            rgbtRed   := Trunc(rgbtRed*Am)   + Trunc(GetRValue(Col)*An);
            rgbtGreen := Trunc(rgbtGreen*Am) + Trunc(GetGValue(Col)*An);
            rgbtBlue  := Trunc(rgbtBlue*Am)  + Trunc(GetBValue(Col)*An);
          end;
      end;
  end;
end;
 
procedure TplPanelTexture.DrawWindowShade(Obj:TObject);
var
  Br,X2,Y2,X1,Y1 : integer;
  L,T,W,H:integer;
  Pos : TPoint;
  Bmp1,Bmp2 : TBitmap;
begin
  if (Obj <> nil) then begin
    with Obj as TControl do begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
    end;

    Br  := Round(FSettings.FShadeDiffusion)*2;
    if (FSettings.FShadeHeight > 0) then Pos := NewPos(0,FSettings.FShadeHeight,0,0,FSettings.FShadeAngle) else Pos := Point(0,0);
    X1  := Br;
    Y1  := Br;
    X2  := W + Br;
    Y2  := H + Br;

    Bmp1 := TBitmap.Create;
    Bmp1.PixelFormat := pf24Bit;
    Bmp1.Canvas.Brush.Style := bsSolid;
    Bmp1.Width  := W + 2*Br;
    Bmp1.Height := H + 2*Br;
    Bmp1.Canvas.Brush.Color := clWhite;
    Bmp1.Canvas.FillRect(Rect(0,0,W+Br*2,H+Br*2));
    Bmp1.Canvas.Brush.Color := clBlack;
    Bmp1.Canvas.FillRect(Rect(Br,Br,W+Br,H+Br));

    Temp.Canvas.Brush.Color := Norm24BitCol(FSettings.FShadeColor);
    Temp.Canvas.FillRect(Rect(L+Pos.X+Br,T+Pos.Y+Br,L+W+Pos.X-Br,T+H+Pos.Y-Br));

    Bmp2 := TBitmap.Create;
    Bmp2.PixelFormat := pf24Bit;
    Bmp2.Canvas.Brush.Style := bsSolid;

    // Left
    if (X1 + Br >= 0) and (X1 - Br <= Width) then begin
      Bmp2.Width  := Br*2;
      Bmp2.Height := H+Br*2;
      Bmp2.Canvas.FillRect(Rect(0,0,Br*2,H+Br*2));
      Bmp2.Canvas.CopyRect(Rect(0,0,Br*2,H+Br*2),Bmp1.Canvas,Rect(X1-Br,Y1-Br,X1+Br,Y2+Br));
      GBlur(Bmp2,FSettings.FShadeDiffusion);
      ApplyShade(X1-Br+L,Y1-Br+T,Bmp2);
    end;

    // Right
    if (X2 + Br >= 0) and (X2 - Br + L <= Width) then begin
      Bmp2.Width  := Br*2;
      Bmp2.Height := H+Br*2;
      Bmp2.Canvas.FillRect(Rect(0,0,Br*2,H+Br*2));
      Bmp2.Canvas.CopyRect(Rect(0,0,Br*2,H+Br*2),Bmp1.Canvas,Rect(X2-Br,Y1-Br,X2+Br,Y2+Br));
      GBlur(Bmp2,FSettings.FShadeDiffusion);
      ApplyShade(X2-Br+L,Y1-Br+T,Bmp2);
    end;

    // Top
    if (Y1 + Br >= 0) and (Y1 - Br <= Height) then begin
      Bmp2.Width  := W-2*Br;
      Bmp2.Height := Br*2;
      Bmp2.Canvas.FillRect(Rect(0,0,W,Br*2));
      Bmp2.Canvas.CopyRect(Rect(0,0,W,Br*2),Bmp1.Canvas,Rect(X1+Br,Y1-Br,X2-Br,Y1+Br));
      GBlur(Bmp2,FSettings.FShadeDiffusion);
      ApplyShade(X1+Br+L,Y1-Br+T,Bmp2);
    end;

    // Bottom
    if (Y2 + Br >= 0) and (Y2 - Br + T <= Height) then begin
      Bmp2.Width  := W-2*Br;
      Bmp2.Height := Br*2;
      Bmp2.Canvas.FillRect(Rect(0,0,W,Br*2));
      Bmp2.Canvas.CopyRect(Rect(0,0,W,Br*2),Bmp1.Canvas,Rect(X1+Br,Y2-Br,X2-Br,Y2+Br));
      GBlur(Bmp2,FSettings.FShadeDiffusion);
      ApplyShade(X1+Br+L,Y2-Br+T,Bmp2);
    end;

    Bmp1.Free;
    Bmp2.Free;
  end;
end;
 
procedure TplPanelTexture.DrawTextShade(Obj:TObject);
var
  Br : integer;
  Bmp1 : TBitmap;
  L,T,W,H : integer;
  Text : string;
  Wind : boolean;
begin
  if (Obj <> nil) then begin
    with Obj as TLabel do begin
      L    := Left;
      T    := Top;
      W    := Width;
      H    := Height;
      Text := Caption;
      Wind := Transparent;
    end;

    if not Wind then DrawWindowShade(Obj)
    else begin
      Br  := Round(FSettings.FShadeDiffusion)*2;

      Bmp1 := TBitmap.Create;
      Bmp1.PixelFormat := pf24Bit;
      Bmp1.Canvas.Brush.Style := bsSolid;
      Bmp1.Width  := W + 2*Br;
      Bmp1.Height := H + 2*Br;
      Bmp1.Canvas.Brush.Color := clWhite;
      Bmp1.Canvas.FillRect(Rect(0,0,W+Br*2,H+Br*2));
      OutputLabel2Canv(Self,TLabel(Obj),Bmp1.Canvas,Br);
      GBlur(Bmp1,FSettings.FShadeDiffusion);
      ApplyShade(L,T,Bmp1);

      Bmp1.Free;
    end;
  end;
end;
 
procedure TplPanelTexture.DrawShapeShade(Obj:TObject);
var
  Br,cS : integer;
  Bmp1 : TBitmap;
  L,T,W,H:integer;
  Bru:TBrush;
  P:TPen;
  Sh:TShapeType;
begin
  if (Obj <> nil) then begin
    with Obj as TShape do begin
      L   := Left;
      T   := Top;
      W   := Width;
      H   := Height;
      Bru := Brush;
      P   := Pen;
      Sh  := Shape;
    end;

    if (Bru.Style = bsSolid) and (Sh = stRectangle) then begin
       DrawWindowShade(Obj);
    end else begin
      Br  := Round(FSettings.FShadeDiffusion)*2;
      Bmp1 := TBitmap.Create;
      Bmp1.PixelFormat := pf24Bit;
      Bmp1.Canvas.Brush.Style := bsSolid;
      Bmp1.Width  := W + 2*Br;
      Bmp1.Height := H + 2*Br;
      Bmp1.Canvas.Brush.Color := clWhite;
      Bmp1.Canvas.FillRect(Rect(0,0,W+Br*2,H+Br*2));

      Bmp1.Canvas.Pen.Color   := clBlack;
      Bmp1.Canvas.Brush.Color := clBlack;
      Bmp1.Canvas.Pen.Style   := P.Style;
      Bmp1.Canvas.Pen.Width   := P.Width;
      Bmp1.Canvas.Brush.Style := Bru.Style;
      if (W > H) then cS := H
      else cS := W;;

      Case Sh of
         stCircle        : Bmp1.Canvas.Ellipse(Bmp1.Width div 2 - cS div 2,Bmp1.Height div 2 - cS div 2,Bmp1.Width div 2 + cS div 2,Bmp1.Height div 2 + cS div 2);
         stEllipse       : Bmp1.Canvas.Ellipse(Bmp1.Width div 2 - W div 2,Bmp1.Height div 2 - H div 2,Bmp1.Width div 2 + W div 2,Bmp1.Height div 2 + H div 2);
         stRectangle     : Bmp1.Canvas.Rectangle(Bmp1.Width div 2 - W div 2,Bmp1.Height div 2 - H div 2,Bmp1.Width div 2 + W div 2,Bmp1.Height div 2 + H div 2);
         stSquare        : Bmp1.Canvas.Rectangle(Bmp1.Width div 2 - cS div 2,Bmp1.Height div 2 - cS div 2,Bmp1.Width div 2 + cS div 2,Bmp1.Height div 2 + cS div 2);
         stRoundSquare   : Bmp1.Canvas.RoundRect(Bmp1.Width div 2 - cS div 2,Bmp1.Height div 2 - cS div 2,Bmp1.Width div 2 + cS div 2,Bmp1.Height div 2 + cS div 2,10,10);
         stRoundRect     : Bmp1.Canvas.RoundRect(Bmp1.Width div 2 - W div 2,Bmp1.Height div 2 - H div 2,Bmp1.Width div 2 + W div 2,Bmp1.Height div 2 + H div 2,cS div 4,cS div 4);
      end;

      GBlur(Bmp1,FSettings.FShadeDiffusion);
      ApplyShade(L,T,Bmp1);
      Bmp1.Free;
    end;
  end;
end;
 
procedure TplPanelTexture.DrawShapeOwn(Obj:TObject);
var
  Br,X,Y : integer;
  Bmp1 : TBitmap;
  Row,RowT : pRGBArray;
  Col : TColor;
  L,T,W,H:integer;
  ShapeBmp:TBitmap;
  B : boolean;
  TT : TTextureType;
  //----------
  procedure GetVar;
  begin
    with Obj as TplPanelTexture do if FSettings.Shape.Empty or (FSettings.Shape = nil) then B := False else B := True;
    with Obj as TplPanelTexture do begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
      TT:= Settings.TextureType;
      if B then begin
        ShapeBmp.PixelFormat := FSettings.Shape.PixelFormat;
        ShapeBmp.Width       := FSettings.Shape.Width;
        ShapeBmp.Height      := FSettings.Shape.Height;
        ShapeBmp.Canvas.Draw(0,0,FSettings.Shape);
        B := True;
      end;
    end;
  end;
  //----------
begin
  if (Obj <> nil) then begin
    ShapeBmp := TBitmap.Create;
    GetVar;
    if not (TT = ttBlend) then begin
      if not B then begin
         DrawWindowShade(Obj);
      end else begin
        Br  := Round(FSettings.FShadeDiffusion)*2;
        Bmp1 := TBitmap.Create;
        Bmp1.PixelFormat := pf24Bit;
        Bmp1.Canvas.Brush.Style := bsSolid;
        if (ShapeBmp.Width > W) then Bmp1.Width  := W + 2*Br else Bmp1.Width  := ShapeBmp.Width + 2*Br;
        if (ShapeBmp.Height > H) then Bmp1.Height  := H + 2*Br else Bmp1.Height  := ShapeBmp.Height + 2*Br;

        Col := ShapeBmp.Canvas.Pixels[0,0];
        for Y := 0 to Bmp1.Height-1-Br*2 do begin
            Row := ShapeBmp.ScanLine[Y];
            RowT := Bmp1.ScanLine[Y+Br];
            for X := 0 to Bmp1.Width-1-Br*2 do begin
                if (Row[X].rgbtBlue  = GetBValue(Col)) and (Row[X].rgbtGreen = GetGValue(Col)) and (Row[X].rgbtRed   = GetRValue(Col)) then begin
                  RowT[X+Br].rgbtBlue  := 255;
                  RowT[X+Br].rgbtGreen := 255;
                  RowT[X+Br].rgbtRed   := 255;
                end else begin
                  RowT[X+Br].rgbtBlue  := 0;
                  RowT[X+Br].rgbtGreen := 0;
                  RowT[X+Br].rgbtRed   := 0;
                end;
            end;
        end;
        GBlur(Bmp1,FSettings.FShadeDiffusion);
        ApplyShade(L,T,Bmp1);
        Bmp1.Free;
      end;
    end;
    ShapeBmp.Free;
  end;
end;
 
procedure TplPanelTexture.SaveSettings(Stream:TMemoryStream);
var
  C   : array [0..2] of Char;
  I,J : Integer;
begin
  c := 'ET3';
  // ET1
  Stream.Write(c,SizeOf(C));
  Stream.Write(FColor,SizeOf(TColor));
  Stream.Write(FBIn,SizeOf(byte));
  Stream.Write(FBOut,SizeOf(byte));
  StringToStream(FCaption,Stream);
  Stream.Write(FSettings.FBCol,SizeOf(TColor));
  Stream.Write(FCtl3D,SizeOf(byte));
  Stream.Write(FSettings.FMove,SizeOf(byte));
  Stream.Write(FSettings.FSize,SizeOf(byte));
  Stream.Write(FSettings.FGrad1,SizeOf(TColor));
  Stream.Write(FSettings.FGrad2,SizeOf(TColor));
  BitmapToStream(FSettings.FTexture,Stream);
  Stream.Write(FSettings.FTType,SizeOf(byte));
  Stream.Write(FSettings.FScale,SizeOf(double));
  Stream.Write(FSettings.FSmooth,SizeOf(byte));
  Stream.Write(FSettings.FShadeAngle,SizeOf(integer));
  Stream.Write(FSettings.FShadeColor,SizeOf(TColor));
  Stream.Write(FSettings.FShadeDiffusion,SizeOf(double));
  Stream.Write(FSettings.FShadeHeight,SizeOf(integer));
  Stream.Write(FSettings.FShadeShow,SizeOf(byte));
  BitmapToStream(FSettings.FEZShape,Stream);
  // ET2
  Stream.Write(FSettings.FBlendCol,SizeOf(TColor));
  Stream.Write(FSettings.FBlend,SizeOf(byte));
  Stream.Write(FSettings.FGradType,SizeOf(byte));
  Stream.Write(FSettings.FX0,SizeOf(integer));
  Stream.Write(FSettings.FY0,SizeOf(integer));
  // ET3
  J := Length(FSettings.EZShadeOb);
  Stream.Write(J,SizeOf(Integer));
  if J > 0 then for I := 0 to J-1 do begin
    StringToStream(FSettings.EZShadeOb[I].Name,Stream);
    Stream.Write(FSettings.EZShadeOb[I].Act,SizeOf(TShadeAction));
    Stream.Write(FSettings.EZShadeOb[I].Visible,SizeOf(Byte));
    Stream.Write(FSettings.EZShadeOb[I].L,SizeOf(Integer));
    Stream.Write(FSettings.EZShadeOb[I].T,SizeOf(Integer));
    Stream.Write(FSettings.EZShadeOb[I].W,SizeOf(Integer));
    Stream.Write(FSettings.EZShadeOb[I].H,SizeOf(Integer));
    StringToStream(FSettings.EZShadeOb[I].Inherit,Stream);
  end;
end;
 
procedure TplPanelTexture.LoadSettings(Stream:TMemoryStream);
var
  C   : array [0..2] of Char;
  I,J : integer;
begin
 Stream.Read(C,SizeOf(C));
 if (C = 'ET2') or (C = 'ET1') or (C = 'ET3') then begin
  Stream.Read(FColor,SizeOf(TColor));
  Stream.Read(FBIn,SizeOf(byte));
  Stream.Read(FBOut,SizeOf(byte));
  FCaption := StreamToString(Stream);
  Stream.Read(FSettings.FBCol,SizeOf(TColor));
  Stream.Read(FCtl3D,SizeOf(byte));
  Stream.Read(FSettings.FMove,SizeOf(byte));
  Stream.Read(FSettings.FSize,SizeOf(byte));
  Stream.Read(FSettings.FGrad1,SizeOf(TColor));
  Stream.Read(FSettings.FGrad2,SizeOf(TColor));
  StreamToBitmap(FSettings.FTexture,Stream);
  Stream.Read(FSettings.FTType,SizeOf(byte));
  Stream.Read(FSettings.FScale,SizeOf(double));
  Stream.Read(FSettings.FSmooth,SizeOf(byte));
  Stream.Read(FSettings.FShadeAngle,SizeOf(integer));
  Stream.Read(FSettings.FShadeColor,SizeOf(TColor));
  Stream.Read(FSettings.FShadeDiffusion,SizeOf(double));
  Stream.Read(FSettings.FShadeHeight,SizeOf(integer));
  Stream.Read(FSettings.FShadeShow,SizeOf(byte));
  StreamToBitmap(FSettings.FEZShape,Stream);
  if (C = 'ET2') or (C = 'ET3') then begin
    Stream.Read(FSettings.FBlendCol,SizeOf(TColor));
    Stream.Read(FSettings.FBlend,SizeOf(byte));
    Stream.Read(FSettings.FGradType,SizeOf(byte));
    Stream.Read(FSettings.FX0,SizeOf(integer));
    Stream.Read(FSettings.FY0,SizeOf(integer));
  end;
  if (C = 'ET3') then begin
    Stream.Read(J,SizeOf(Integer));
    SetLength(FSettings.EZShadeOb,J);
    if J > 0 then for I := 0 to J-1 do begin
      FSettings.EZShadeOb[I].Name := StreamToString(Stream);
      Stream.Read(FSettings.EZShadeOb[I].Act,SizeOf(TShadeAction));
      Stream.Read(FSettings.EZShadeOb[I].Visible,SizeOf(Byte));
      Stream.Read(FSettings.EZShadeOb[I].L,SizeOf(Integer));
      Stream.Read(FSettings.EZShadeOb[I].T,SizeOf(Integer));
      Stream.Read(FSettings.EZShadeOb[I].W,SizeOf(Integer));
      Stream.Read(FSettings.EZShadeOb[I].H,SizeOf(Integer));
      FSettings.EZShadeOb[I].Inherit := StreamToString(Stream);
    end;
  end;
  ReDraw;
 end;
end;

end.

