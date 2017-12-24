unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, bgTileMap, bgPanel, BGRABitmap, BGRABitmapTypes, types;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bgMap: TBGPanel;
    btnClear: TButton;
    cbTileID: TComboBox;
    cbZoom: TComboBox;
    toolsPanel: TPanel;
    mainScrollBox: TScrollBox;
    procedure bgMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure bgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure bgMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure bgMapMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure bgMapMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure bgMapRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure btnClearClick(Sender: TObject);
    procedure cbZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCacheBitmap: TBGRABitmap;
    FTileMap: TBGTileMap;
    FMouseIsDown: boolean;
    FLastPainted: NativeInt;
    FCursorRect: TRect;
    FMustRedraw: boolean;
    FDefaultTileWidth: NativeInt;
    FDefaultTileHeight: NativeInt;
    procedure SetFCacheBitmap(AValue: TBGRABitmap);
    procedure SetFCursorRect(AValue: TRect);
    procedure SetFDefaultTileHeight(AValue: NativeInt);
    procedure SetFDefaultTileWidth(AValue: NativeInt);
    procedure SetFLastPainted(AValue: NativeInt);
    procedure SetFMouseIsDown(AValue: boolean);
    procedure SetFMustRedraw(AValue: boolean);
    procedure SetFTileMap(AValue: TBGTileMap);
  public
    procedure PaintTile(Eraser: boolean = False);
  public
    property TileMap: TBGTileMap read FTileMap write SetFTileMap;
    property MouseIsDown: boolean read FMouseIsDown write SetFMouseIsDown default False;
    property LastPainted: NativeInt read FLastPainted write SetFLastPainted default -1;
    property CursorRect: TRect read FCursorRect write SetFCursorRect;
    property MustRedraw: boolean read FMustRedraw write SetFMustRedraw;
    property CacheBitmap: TBGRABitmap read FCacheBitmap write SetFCacheBitmap;
    property DefaultTileWidth: NativeInt read FDefaultTileWidth
      write SetFDefaultTileWidth;
    property DefaultTileHeight: NativeInt read FDefaultTileHeight
      write SetFDefaultTileHeight;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: NativeInt;
begin
  MouseIsDown := False;
  LastPainted := -1;

  TileMap := TBGTileMap.Create;

  // just drag and drop a level to the exe to load it
  if ParamStr(1) <> '' then
  begin
    SetCurrentDir(ExtractFilePath(ParamStr(1)));
    TileMap.LoadFromINIFile(ParamStr(1));
  end
  else
    TileMap.LoadFromINIFile('map.ini');

  DefaultTileWidth := TileMap.Map.TileWidth;
  DefaultTileHeight := TileMap.Map.TileHeight;

  CacheBitmap := TBGRABitmap.Create(TileMap.Map.Width * TileMap.Map.TileWidth,
    TileMap.Map.Height * TileMap.Map.TileHeight);
  MustRedraw := True;

  bgMap.Width := CacheBitmap.Width;
  bgMap.Height := CacheBitmap.Height;

  for i := -1 to ((TileMap.TileSet.Bitmap.Width div TileMap.TileSet.TileWidth) *
      (TileMap.TileSet.Bitmap.Height div TileMap.TileSet.TileHeight)) - 1 do
  begin
    cbTileID.Items.Add(IntToStr(i));
  end;

  cbTileID.Caption := cbTileID.Items[0];
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CacheBitmap.Free;

  TileMap.Map.TileWidth := DefaultTileWidth;
  TileMap.Map.TileHeight := DefaultTileHeight;

  if ParamStr(1) <> '' then
    TileMap.SaveToINIFile(ParamStr(1))
  else
    TileMap.SaveToINIFile('map.ini');

  TileMap.Free;
end;

procedure TfrmMain.SetFTileMap(AValue: TBGTileMap);
begin
  if FTileMap = AValue then
    Exit;
  FTileMap := AValue;
end;

procedure TfrmMain.PaintTile(Eraser: boolean = False);
var
  pos: TPoint;
  x, y, n: NativeInt;
  r: TRect;
begin
  pos := ScreenToClient(Mouse.CursorPos);
  pos.x := pos.x + mainScrollBox.HorzScrollBar.Position;
  pos.y := pos.y + mainScrollBox.VertScrollBar.Position;
  n := 0;
  CursorRect := Rect(-TileMap.Map.TileWidth, -TileMap.Map.TileHeight, 0, 0);
  for y := 0 to TileMap.Map.Height - 1 do
  begin
    for x := 0 to TileMap.Map.Width - 1 do
    begin
      r.Left := x * TileMap.Map.TileWidth;
      r.Top := y * TileMap.Map.TileHeight;
      r.Right := r.Left + TileMap.Map.TileWidth;
      r.Bottom := r.Top + TileMap.Map.TileHeight;
      if ((pos.x >= r.Left) and (pos.x < r.Right)) and
        ((pos.y >= r.Top) and (pos.y < r.Bottom)) then
      begin
        CursorRect := r;
        if MouseIsDown and (LastPainted <> n) then
        begin
          if Eraser then
            TileMap.Layers[0].Data[n] := '-1'
          else
            TileMap.Layers[0].Data[n] := cbTileID.Caption;
          LastPainted := n;
          MustRedraw := True;
        end;
        break;
        break;
      end;
      Inc(n);
    end;
  end;
  bgMap.DiscardBitmap;
end;

procedure TfrmMain.SetFMouseIsDown(AValue: boolean);
begin
  if FMouseIsDown = AValue then
    Exit;
  FMouseIsDown := AValue;
end;

procedure TfrmMain.SetFMustRedraw(AValue: boolean);
begin
  if FMustRedraw = AValue then
    Exit;
  FMustRedraw := AValue;
end;

procedure TfrmMain.SetFLastPainted(AValue: NativeInt);
begin
  if FLastPainted = AValue then
    Exit;
  FLastPainted := AValue;
end;

procedure TfrmMain.SetFCursorRect(AValue: TRect);
begin
  {if FCursorRect = AValue then
    Exit;}
  FCursorRect := AValue;
end;

procedure TfrmMain.SetFDefaultTileHeight(AValue: NativeInt);
begin
  if FDefaultTileHeight = AValue then
    Exit;
  FDefaultTileHeight := AValue;
end;

procedure TfrmMain.SetFDefaultTileWidth(AValue: NativeInt);
begin
  if FDefaultTileWidth = AValue then
    Exit;
  FDefaultTileWidth := AValue;
end;

procedure TfrmMain.SetFCacheBitmap(AValue: TBGRABitmap);
begin
  if FCacheBitmap = AValue then
    Exit;
  FCacheBitmap := AValue;
end;

procedure TfrmMain.bgMapRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if MustRedraw then
  begin
    //writeln('Redraw Map');
    TileMap.DrawMap(Bitmap);
    CacheBitmap.PutImage(0, 0, Bitmap, dmSet);
    MustRedraw := False;
  end
  else
  begin
    Bitmap.PutImage(0, 0, CacheBitmap, dmSet);
  end;

  if cbTileID.Caption <> '-1' then
  begin
    TileMap.DrawTile(Bitmap, CursorRect.Left, CursorRect.Top,
      StrToInt(cbTileID.Caption), 255);
  end;

  Bitmap.Rectangle(CursorRect.Left, CursorRect.Top, CursorRect.Right,
    CursorRect.Bottom, BGRAWhite, BGRA(255, 255, 255, 100), dmDrawWithTransparency);
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
var
  i: NativeInt;
begin
  if QuestionDlg('Clear', 'Clear all tiles?', mtConfirmation,
    [mbYes, 'Yes', mbNo, 'No', mbCancel, 'Cancel'], '') = 0 then
  begin
    for i := 0 to TileMap.Layers[0].Data.Count - 1 do
      TileMap.Layers[0].Data[i] := '-1';
    MustRedraw := True;
    bgMap.DiscardBitmap;
  end;
end;

procedure TfrmMain.cbZoomChange(Sender: TObject);

  procedure ZoomMap(p: real);
  begin
    TileMap.Map.TileHeight := trunc(DefaultTileHeight * p);
    TileMap.Map.TileWidth := trunc(DefaultTileWidth * p);
  end;

begin
  case cbZoom.Text of
    '25%': ZoomMap(0.25);
    '50%': ZoomMap(0.5);
    '75%': ZoomMap(0.75);
    '100%': ZoomMap(1);
    '125%': ZoomMap(1.25);
    '150%': ZoomMap(1.5);
    '175%': ZoomMap(1.75);
    '200%': ZoomMap(2);
    '300%': ZoomMap(3);
    '400%': ZoomMap(4);
  end;

  CacheBitmap.SetSize(TileMap.Map.Width * TileMap.Map.TileWidth,
    TileMap.Map.Height * TileMap.Map.TileHeight);
  MustRedraw := True;

  bgMap.Width := CacheBitmap.Width;
  bgMap.Height := CacheBitmap.Height;
end;

procedure TfrmMain.bgMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  MouseIsDown := True;
  PaintTile(ssRight in Shift);
end;

procedure TfrmMain.bgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  PaintTile(ssRight in Shift);
end;

procedure TfrmMain.bgMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  MouseIsDown := False;
  LastPainted := -1;
end;

procedure TfrmMain.bgMapMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  if ssShift in Shift then
    if cbTileID.ItemIndex - 1 >= 0 then
    begin
      cbTileID.ItemIndex := cbTileID.ItemIndex - 1;
      PaintTile;
    end;

  if ssCtrl in Shift then
    if cbZoom.ItemIndex - 1 >= 0 then
    begin
      cbZoom.ItemIndex := cbZoom.ItemIndex - 1;
      cbZoomChange(Self);
      PaintTile;
    end;
end;

procedure TfrmMain.bgMapMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  if ssShift in Shift then
    if cbTileID.ItemIndex + 1 < cbTileID.Items.Count then
    begin
      cbTileID.ItemIndex := cbTileID.ItemIndex + 1;
      PaintTile;
    end;

  if ssCtrl in Shift then
    if cbZoom.ItemIndex - 1 < cbZoom.Items.Count then
    begin
      cbZoom.ItemIndex := cbZoom.ItemIndex + 1;
      cbZoomChange(Self);
      PaintTile;
    end;
end;

end.
