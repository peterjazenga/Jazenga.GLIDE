type
  TSdpoFastFormOnMouseDownEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TSdpoFastFormOnMouseUpEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TSdpoFastFormOnMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;
  TSdpoFastFormOnKeyDownEvent = procedure(Sender: TObject; Key: LongWord; Shift: TShiftState) of object;
  TSdpoFastFormOnKeyUpEvent = procedure(Sender: TObject; Key: LongWord; Shift: TShiftState) of object;
  TSdpoFastFormOnCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TSdpoFastFormOnShowEvent = procedure(Sender: TObject) of object;

  { TSdpoFastForm }

  TSdpoFastForm = class(TComponent)
  private
    { private declarations }
    FOnMouseDown: TSdpoFastFormOnMouseDownEvent;
    FOnMouseMove: TSdpoFastFormOnMouseMoveEvent;
    FOnMouseUp: TSdpoFastFormOnMouseUpEvent;
    FOnKeyDown: TSdpoFastFormOnKeyDownEvent;
    FOnKeyUp: TSdpoFastFormOnKeyUpEvent;
    FOnClose: TSdpoFastFormOnCloseEvent;
    FOnShow: TSdpoFastFormOnShowEvent;

    FormBitmap: TBitmap;
    FormIntfImage: TLazIntfImage;
    FWidth: integer;
    FHeight: integer;
    FForm: TForm;
    FCanvas: TCanvas;
    FCaption: string;
    FAlwaysOnTop: boolean;
    FPos_Y,FPos_X: Integer;
    FPosition: TFFPosition;
    FImageType: TFFImageType;
    FVisible: boolean;
    FIconFileName: string;

    // events
    procedure FFormChangeBounds(Sender: TObject);
    procedure FFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FFormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure FFormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FFormShow(Sender: TObject);

    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);

    procedure WindowOpen;
    procedure WindowClose;
    procedure SetVisible(state: boolean);

    procedure SetPosX(Value: Integer);
    procedure SetPosY(Value: Integer);
  public
    { public declarations }
    Data: pointer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint;
    procedure Sync;
    procedure DrawToPixmap;
    procedure PaintFromPixmap;

    procedure Show;
    procedure Close;
    property Canvas: TCanvas read FCanvas write FCanvas;

    procedure SetCaption(cap: string);
    procedure SetAlwaysOnTop(setting: boolean);

    procedure BringToFront;

    procedure SetDebugList(dl: TStrings);
    procedure AddDebug(s: string);

    //property Data: pointer read GetData;  //Data: pointer
    //property Image: PGdkImage read FImage;
    //property Window: PGtkWindow read GetWindow;
    //property Canvas: TSdpoFastFormCanvas read GetCanvas;
    property Visible: boolean read FVisible;

  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Caption: string read FCaption write SetCaption;
    property TopPos: integer read FPos_Y write SetPosY;
    property LeftPos: integer read FPos_X write SetPosX;
    property Position: TFFPosition read FPosition write FPosition;
    property ImageType: TFFImageType read FImageType write FImageType;
    property AlwaysOnTop: boolean read FAlwaysOnTop write SetAlwaysOnTop;
    property Icon: string read FIconFileName write FIconFileName;

    //
    property OnMouseDown: TSdpoFastFormOnMouseDownEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TSdpoFastFormOnMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TSdpoFastFormOnMouseUpEvent read FOnMouseUp write FOnMouseUp;
    property OnKeyUp: TSdpoFastFormOnKeyUpEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyDown: TSdpoFastFormOnKeyDownEvent read FOnKeyDown write FOnKeyDown;
    property OnClose: TSdpoFastFormOnCloseEvent read FOnClose write FOnClose;
    property OnShow: TSdpoFastFormOnShowEvent read FOnShow write FOnShow;
  end;

procedure Register;

implementation

constructor TSdpoFastForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForm := TForm.Create(Self);
  FCanvas := FForm.Canvas;

  FWidth:=100;
  FHeight:=100;
  FForm.BorderIcons:=[biMinimize,biSystemMenu];
  FForm.Height:=FHeight;
  FForm.Width:=FWidth;
  FForm.BorderStyle:=bsSingle;

  FormBitmap:=TBitmap.Create;
  FormBitmap.TransparentMode:=tmFixed;
  FormBitmap.Transparent:=false;
  FormBitmap.PixelFormat:=pf32bit;
  FormBitmap.Height:=FHeight;
  FormBitmap.Width:=FWidth;
  FormIntfImage := FormBitmap.CreateIntfImage;
  Data := FormIntfImage.PixelData;

  FForm.OnChangeBounds := @FFormChangeBounds;
end;

destructor TSdpoFastForm.Destroy;
begin
  FormIntfImage.Free;
  FormBitmap.Free;
  FForm.Free;
  inherited Destroy;
end;

procedure TSdpoFastForm.SetPosX(Value: Integer);
begin
  FPos_X := Value;
  if FVisible = true then
    FForm.Left := FPos_X;
end;

procedure TSdpoFastForm.SetPosY(Value: Integer);
begin
  FPos_Y := Value;
  if FVisible = true then
    FForm.Top := FPos_Y;
end;

procedure TSdpoFastForm.SetVisible(state: boolean);
begin
  if FVisible=state then exit;

  if state then WindowOpen
  else WindowClose;
end;

procedure TSdpoFastForm.Show;
begin
  SetVisible(true);
end;

procedure TSdpoFastForm.Close;
begin
  SetVisible(false);
end;

procedure TSdpoFastForm.WindowOpen;
begin
  if Position = poCenter then
    FForm.Position:=poDesktopCenter
  else if Position = poSelectable then begin
    FForm.Top := TopPos;
    FForm.Left := LeftPos;
  end else
    FForm.Position:=poDefaultPosOnly;

  if Assigned(FOnMouseDown) then
    FForm.OnMouseDown := @FFormMouseDown;
  if Assigned(FOnMouseUp) then
    FForm.OnMouseUp := @FFormMouseUp;
  if Assigned(FOnMouseMove) then
    FForm.OnMouseMove := @FFormMouseMove;
  if Assigned(FOnKeyDown) then
    FForm.OnKeyDown := @FFormKeyDown;
  if Assigned(FOnKeyUp) then
    FForm.OnKeyUp := @FFormKeyUp;
  if Assigned(FOnClose) then
    FForm.OnClose := @FFormClose;
  if Assigned(FOnShow) then
    FForm.OnShow := @FFormShow;

  FForm.Show;
  FVisible := True;
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TSdpoFastForm.WindowClose;
var CloseAction: TCloseAction;
begin
  CloseAction := caHide;

  //if Assigned(FOnClose) then FOnClose(Self,CloseAction);

  if CloseAction = caHide then begin
    FForm.Close;
    FVisible := False;
  end;
end;

procedure TSdpoFastForm.SetHeight(Value: Integer);
begin
  FForm.Height:=Value;

  FHeight:=Value;

  FormIntfImage.Free;
  FormBitmap.Free;

  FormBitmap:=TBitmap.Create;
  FormBitmap.PixelFormat:=pf32bit;
  FormBitmap.Height:=FHeight;
  FormBitmap.Width:=FWidth;
  FormIntfImage := FormBitmap.CreateIntfImage;
  Data := FormIntfImage.PixelData;
end;

procedure TSdpoFastForm.SetAlwaysOnTop(setting: boolean);
begin
  // TODO
end;

procedure TSdpoFastForm.BringToFront;
begin
  // TODO
end;

procedure TSdpoFastForm.SetDebugList(dl: TStrings);
begin
  // TODO
end;

procedure TSdpoFastForm.AddDebug(s: string);
begin
  // TODO
end;

procedure TSdpoFastForm.SetCaption(cap: string);
begin
  FForm.Caption:=Cap;
  FCaption:=Cap;
end;

procedure TSdpoFastForm.SetWidth(Value: Integer);
begin
  FForm.Width:=Value;

  FWidth:=Value;

  FormIntfImage.Free;
  FormBitmap.Free;

  FormBitmap:=TBitmap.Create;
  FormBitmap.PixelFormat:=pf32bit;
  FormBitmap.Height:=FHeight;
  FormBitmap.Width:=FWidth;
  FormIntfImage := FormBitmap.CreateIntfImage;
  Data := FormIntfImage.PixelData;
end;

procedure TSdpoFastForm.Paint;
var
  bmp, old: HBitmap;
  msk: HBitmap;
  tmpDC: HDC;
begin
  FormIntfImage.CreateBitmaps(bmp, msk, false);
  tmpDC := CreateCompatibleDC(FForm.Canvas.Handle);
  old := SelectObject(tmpDC, bmp);
  BitBlt(FForm.Canvas.Handle, 0, 0, FormIntfImage.Width, FormIntfImage.Height, tmpDC, 0, 0, SRCCOPY);
  DeleteObject(SelectObject(tmpDC, old));
  DeleteObject(msk);
  DeleteDC(tmpDC);
end;

procedure TSdpoFastForm.Sync;
begin
  // empty
end;

procedure TSdpoFastForm.DrawToPixmap;
begin
  Paint;
end;

procedure TSdpoFastForm.PaintFromPixmap;
begin
  // empty
end;

// Form Events

procedure TSdpoFastForm.FFormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TSdpoFastForm.FFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FOnClose) then FOnClose(Self, CloseAction);
  if CloseAction = caHide then begin
    FVisible := False;
  end;
end;

procedure TSdpoFastForm.FFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

procedure TSdpoFastForm.FFormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

procedure TSdpoFastForm.FFormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TSdpoFastForm.FFormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TSdpoFastForm.FFormShow(Sender: TObject);
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TSdpoFastForm.FFormChangeBounds(Sender: TObject);
begin
  if FForm.Top <> FPos_Y then
    FPos_Y := FForm.Top;
  if FForm.Left <> FPos_X then
    FPos_X := FForm.Left;
end;
