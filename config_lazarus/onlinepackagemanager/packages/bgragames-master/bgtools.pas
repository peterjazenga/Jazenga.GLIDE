unit bgTools;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF}Classes, SysUtils, Forms, Controls, Graphics,
  uos_flat, Variants;

type
  TRectangle = record
    x, y, Width, Height: NativeInt;
  end;

procedure RecordToVariant(var ARec; Size: integer; var v: olevariant);
procedure VariantToRecord(v: olevariant; var ARec; Size: integer);

procedure PlaySound(channelID: NativeInt; FileName: string);
procedure PlaySoundLoop(channelID: NativeInt; FileName: string; proc: TProc);

function DoRectangle(x, y, Width, Height: NativeInt): TRectangle;
function BoundInt(AValue, AMin, AMax: NativeInt): NativeInt;
function HitTest(r1, r2: TRectangle): boolean;
function HitTestPrecise(r1, r2: TRectangle): boolean;
function IntTowards(AValue, ATarget: NativeInt): NativeInt;

procedure ToggleFullScreen(Form: TForm; ARect: TRect);
procedure CenterControl(Control: TControl);

function CalculateAspectRatioH(const W1, H1, W2: integer): integer; //result H2
function CalculateAspectRatioW(const W1, H1, H2: integer): integer; //result W2
function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;

procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);

procedure ScaleAspectRatio(Control: TControl; OriginalParentW, OriginalParentH: integer);
procedure ScaleAspectRatio(Control: TControl; DestW, DestH: integer;
  Stretch, Proportional, Center: boolean);

{$IFDEF Windows}
procedure SetScreenResolution(const Width, Height: integer); overload;
procedure SetScreenResolution(const Width, Height, colorDepth: integer); overload;
procedure GetDisplaySettings(s: TStringList);
procedure SetDisplaySettings(Index: integer; Notify: boolean = False);
{$ENDIF}

implementation

procedure RecordToVariant(var ARec; Size: integer; var v: olevariant);
var
  p: PByteArray;
  prec: Pointer;
  i: integer;
begin
  v := VarArrayCreate([0, Size - 1], varByte);
  p := VarArrayLock(v);
  prec := @ARec;
  for i := 0 to Size - 1 do
    p^[i] := (PByteArray(prec))^[i];
  VarArrayUnLock(v);
end;

procedure VariantToRecord(v: olevariant; var ARec; Size: integer);
var
  i: integer;
  p: PByteArray;
  prec: Pointer;
begin
  prec := @ARec;
  p := VarArrayLock(v);
  for i := 0 to Size - 1 do
    (PByteArray(prec))^[i] := p^[i];
  VarArrayUnLock(v);
end;

procedure PlaySound(channelID: NativeInt; FileName: string);
begin
  uos_CreatePlayer(channelID);
  uos_AddFromFile(channelID, PChar(FileName));
  uos_AddIntoDevOut(channelID);
  uos_Play(channelID);
end;

procedure PlaySoundLoop(channelID: NativeInt; FileName: string; proc: TProc);
begin
  uos_CreatePlayer(channelID);
  uos_EndProc(channelID, proc);
  uos_AddFromFile(channelID, PChar(FileName));
  uos_AddIntoDevOut(channelID);
  uos_Play(channelID);
end;

function DoRectangle(x, y, Width, Height: NativeInt): TRectangle;
begin
  Result.x := x;
  Result.y := y;
  Result.Width := Width;
  Result.Height := Height;
end;

function BoundInt(AValue, AMin, AMax: NativeInt): NativeInt;
begin
  if AValue <= AMin then
    Result := AMin
  else
  if AValue >= AMax then
    Result := AMax
  else
    Result := AValue;
end;

function HitTest(r1, r2: TRectangle): boolean;
begin
  Result := ((r1.X + r1.Width > r2.X) and (r1.X < r2.X + r2.Width)) and
    ((r1.Y + r1.Height > r2.Y) and (r1.Y < r2.Y + r2.Height));
end;

function HitTestPrecise(r1, r2: TRectangle): boolean;
begin
  Result := ((r1.X + r1.Width >= r2.X) and (r1.X <= r2.X + r2.Width)) and
    ((r1.Y + r1.Height >= r2.Y) and (r1.Y <= r2.Y + r2.Height));
end;

function IntTowards(AValue, ATarget: NativeInt): NativeInt;
begin
  if AValue < ATarget then
    Result := AValue + 1
  else if AValue > ATarget then
    Result := AValue - 1
  else
    Result := AValue;
end;

procedure ToggleFullScreen(Form: TForm; ARect: TRect);
begin
  Form.SetBounds(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  if Form.WindowState <> wsMaximized then
  begin
    Form.WindowState := wsMaximized;
  end
  else
  begin
    Form.WindowState := wsNormal;
  end;
end;

procedure CenterControl(Control: TControl);
begin
  if not Control.HasParent then
    Exit;
  Control.SetBounds(
    Round((Control.Parent.Width - Control.Width) div 2),
    Round((Control.Parent.Height - Control.Height) div 2),
    Control.Width, Control.Height);
end;

function CalculateAspectRatioH(const W1, H1, W2: integer): integer;
begin
  Result := Round(H1 / W1 * W2);
end;

function CalculateAspectRatioW(const W1, H1, H2: integer): integer;
begin
  Result := Round(W1 / H1 * H2);
end;

function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
var
  w: integer;
  h: integer;
begin
  // Stretch or Proportional when Image (Width or Height) is bigger than Destination
  if Stretch or (Proportional and ((ImageW > DestW) or (ImageH > DestH))) then
  begin
    // Proportional when Image (Width or Height) is bigger than 0
    if Proportional and (ImageW > 0) and (ImageH > 0) then
    begin
      w := DestW;
      h := CalculateAspectRatioH(ImageW, ImageH, DestW);
      if h > DestH then
      begin
        h := DestH;
        w := CalculateAspectRatioW(ImageW, ImageH, DestH);
      end;
      ImageW := w;
      ImageH := h;
    end
    // Stretch not Proportional or when Image (Width or Height) is 0
    else
    begin
      ImageW := DestW;
      ImageH := DestH;
    end;
  end;

  Result := Rect(0, 0, ImageW, ImageH);

  // Center: Destination (Width or Height) - Image divided by 2
  if Center then
  begin
    Result.Left := Round((DestW - ImageW) div 2);
    Result.Top := Round((DestH - ImageH) div 2);
  end;
end;

procedure HighDPI(FromDPI: integer);
var
  i: integer;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;

  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
end;

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i: integer;
  WinControl: TWinControl;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;

  with Control do
  begin
    Left := ScaleX(Left, FromDPI);
    Top := ScaleY(Top, FromDPI);
    Width := ScaleX(Width, FromDPI);
    Height := ScaleY(Height, FromDPI);
  end;

  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount = 0 then
      exit;

    with WinControl.ChildSizing do
    begin
      HorizontalSpacing := ScaleX(HorizontalSpacing, FromDPI);
      LeftRightSpacing := ScaleX(LeftRightSpacing, FromDPI);
      TopBottomSpacing := ScaleY(TopBottomSpacing, FromDPI);
      VerticalSpacing := ScaleY(VerticalSpacing, FromDPI);
    end;

    for i := 0 to WinControl.ControlCount - 1 do
      ScaleDPI(WinControl.Controls[i], FromDPI);
  end;
end;

procedure ScaleAspectRatio(Control: TControl; OriginalParentW, OriginalParentH: integer);
var
  l, t, w, h: integer;
begin
  l := MulDiv(Control.Left, Control.Parent.Width, OriginalParentW);
  t := MulDiv(Control.Top, Control.Parent.Height, OriginalParentH);
  w := MulDiv(Control.Width, Control.Parent.Width, OriginalParentW);
  h := MulDiv(Control.Height, Control.Parent.Height, OriginalParentH);
  Control.SetBounds(l, t, w, h);
end;

procedure ScaleAspectRatio(Control: TControl; DestW, DestH: integer;
  Stretch, Proportional, Center: boolean);
var
  i: integer;
  r: TRect;
  WinControl: TWinControl;
  w, h: integer;
begin
  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    w := WinControl.Width;
    h := WinControl.Height;

    r := CalculateDestRect(WinControl.Width, WinControl.Height, DestW,
      DestH, Stretch, Proportional, Center);
    WinControl.SetBounds(r.Left, r.Top, r.Right, r.Bottom);

    if WinControl.ControlCount = 0 then
      exit;

    for i := 0 to WinControl.ControlCount - 1 do
      ScaleAspectRatio(WinControl.Controls[i], w, h);
  end;
end;

{$IFDEF WINDOWS}
procedure SetScreenResolution(const Width, Height: integer); overload;
var
  mode: TDevMode;
begin
  zeroMemory(@mode, sizeof(TDevMode));
  mode.dmSize := sizeof(TDevMode);
  mode.dmPelsWidth := Width;
  mode.dmPelsHeight := Height;
  mode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
  ChangeDisplaySettings(mode, 0);
end;

procedure SetScreenResolution(const Width, Height, colorDepth: integer); overload;
var
  mode: TDevMode;
begin
  zeroMemory(@mode, sizeof(TDevMode));
  mode.dmSize := sizeof(TDevMode);
  mode.dmPelsWidth := Width;
  mode.dmPelsHeight := Height;
  mode.dmBitsPerPel := colorDepth;
  mode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
  ChangeDisplaySettings(mode, 0);
end;

procedure GetDisplaySettings(s: TStringList);
var
  cnt: integer;
  DevMode: TDevMode;
  t: string;
begin
  cnt := 0;
  while EnumDisplaySettings(nil, cnt, DevMode) do
  begin
    with DevMode do
    begin
      case dmDisplayFixedOutput of
        0: t := 'Default';
        1: t := 'Stretch';
        2: t := 'Center';
      end;
      s.Add(
        IntToStr(dmPelsWidth) + 'x' + IntToStr(dmPelsHeight) + ' ' +
        IntToStr(dmBitsPerPel) + ' Bits @' + IntToStr(dmDisplayFrequency) + 'hz ' + t);
    end;
    Inc(cnt);
  end;
end;

procedure SetDisplaySettings(Index: integer; Notify: boolean = False);
var
  DevMode: TDeviceMode;
  liRetValue: longint;
begin
  if EnumDisplaySettings(nil, Index, DevMode) then
    liRetValue := ChangeDisplaySettings(DevMode, CDS_UPDATEREGISTRY);

  if Notify then
    SendMessage(HWND_BROADCAST, WM_DISPLAYCHANGE, SPI_SETNONCLIENTMETRICS, 0);
end;


{$ENDIF}

end.
