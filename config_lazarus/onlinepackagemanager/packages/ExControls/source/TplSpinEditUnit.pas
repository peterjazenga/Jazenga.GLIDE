{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSpinEditUnit;

interface

uses
    LCLIntf, LCLType, LMessages,
    Classes, Controls, Messages, SysUtils, Forms,
    TplEditUnit, TplSpinButtonUnit;

type

TplSpinEditInteger = class(TCustomFlatEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FButton: TplSpinButton;
    FEditorEnabled: Boolean;
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
  protected
    function IsValidChar (Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    procedure KeyPress (var Key: Char); override;
    procedure CreateParams (var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure SetBtnBounds;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TplSpinButton read FButton;
    property ColorFocused;
    property ColorBorder;
    property ColorFlat;
    property AdvColorFocused;
    property AdvColorBorder;
    property UseAdvColors;

  published
    property AutoSelect;
    property AutoSize;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Value: LongInt read GetValue write SetValue;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

TplSpinEditFloat = class(TCustomFlatEdit)
  private
    FPrecision, FDigits: Integer;
    FFloatFormat: TFloatFormat;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FButton: TplSpinButton;
    FEditorEnabled: Boolean;
    function GetMinHeight: Integer;
    function GetValue: Extended;
    function CheckValue (Value: Extended): Extended;
    procedure SetValue (Value: Extended);
    procedure SetPrecision (Value: Integer);
    procedure SetDigits (Value: Integer);
    procedure SetFloatFormat (Value: TFloatFormat);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
  protected
    function IsValidChar (Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    procedure KeyPress (var Key: Char); override;
    procedure CreateParams (var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure SetBtnBounds;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TplSpinButton read FButton;
  published
    property AutoSelect;
    property AutoSize;
    property DragCursor;
    property DragMode;
    property Digits: Integer read FDigits write SetDigits;
    property Precision: Integer read FPrecision write SetPrecision;
    property FloatFormat: TFloatFormat read FFloatFormat write SetFloatFormat;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: Extended read FIncrement write FIncrement;
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Value: Extended read GetValue write SetValue;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

  end;

implementation

const
  cnBtnOffsetX=0;
  cnBtnOffsetY=2;
  cnBtnWidth  = 22;

//=============== TplSpinEditInteger ===================================

constructor TplSpinEditInteger.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption]+[csAcceptsControls];

  self.SetInitialBounds(0,0,70,16);

  FButton := TplSpinButton.Create(Self);
  FButton.Width:= cnBtnWidth;
  FButton.Height := 8;
  FButton.Visible := True;
  FButton.FocusControl := Self;
  FButton.OnUpClick := @UpClick;
  FButton.OnDownClick := @DownClick;

  InsertControl(FButton) ;   // SOS Parent property Don't work here

  Value := 0;
  FIncrement := 1;
  FEditorEnabled := True;
end;

destructor TplSpinEditInteger.Destroy;
begin
  FButton.free;
  FButton := nil;
  inherited Destroy;
end;

procedure TplSpinEditInteger.KeyDown (var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      UpClick(Self);
    VK_DOWN:
      DownClick(Self);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TplSpinEditInteger.KeyPress (var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
  //  MessageBeep(0)
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

function TplSpinEditInteger.IsValidChar (Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TplSpinEditInteger.CreateParams (var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;


procedure TplSpinEditInteger.UpClick (Sender: TObject);
begin
  if ReadOnly then
  begin
    //MessageBeep(0)
  end else
  begin
    Value := GetValue + FIncrement;
  end;
end;

procedure TplSpinEditInteger.DownClick (Sender: TObject);
begin
  if ReadOnly then
  begin
    //MessageBeep(0)
  end else
  begin
    Value := GetValue - FIncrement;
  end;
end;

procedure TplSpinEditInteger.CMExit(var Message: TLMessage);
begin
  inherited;
  if CheckValue(Value) <> Value then
    SetValue(Value);
end;

function TplSpinEditInteger.GetValue: LongInt;
begin
  try
    Result := StrToInt(Text);
  except
    Result := FMinValue;
  end;
end;

procedure TplSpinEditInteger.SetValue (NewValue: LongInt);
begin
  Text := IntToStr(CheckValue(NewValue));
end;

function TplSpinEditInteger.CheckValue (NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else
      if NewValue > FMaxValue then
        Result := FMaxValue;
  end;
end;

procedure TplSpinEditInteger.CMEnter(var Message: TLMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

function TplSpinEditInteger.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight+ 3;
end;

procedure TplSpinEditInteger.SetBtnBounds;
begin
  FButton.SetBounds(Width - cnBtnWidth - cnBtnOffsetX, 1, cnBtnWidth , Height - cnBtnOffsetY); //button Height
end;

procedure TplSpinEditInteger.WMSize(var Message: TLMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight;

  if FButton <> nil then
    begin
      SetBtnBounds;
    end;
end;

procedure TplSpinEditInteger.Loaded;
begin
  inherited;
  SetBtnBounds;
end;

procedure TplSpinEditInteger.CreateWnd;
begin
  inherited;
  SetBtnBounds;
end;

//===================== TplSpinEditFloat =======================================

constructor TplSpinEditFloat.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);

  self.SetInitialBounds(0,0,70,16);

  FButton := TplSpinButton.Create (Self);
  FButton.Width := cnBtnWidth;
  FButton.Height := 8;
  FButton.Visible := True;
  FButton.FocusControl := Self;
  FButton.OnUpClick := @UpClick;
  FButton.OnDownClick := @DownClick;

  InsertControl(FButton) ;   // SOS Parent property Don't work here

  Text := '0' + DecimalSeparator + '00';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 0.5;
  FEditorEnabled := True;
  FDigits := 2;
  FPrecision := 9;

end;

destructor TplSpinEditFloat.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TplSpinEditFloat.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      UpClick(Self);
    VK_DOWN:
      DownClick(Self);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TplSpinEditFloat.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
   // MessageBeep(0)
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

function TplSpinEditFloat.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TplSpinEditFloat.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;


procedure TplSpinEditFloat.UpClick(Sender: TObject);
begin
  if ReadOnly then
  begin
    //MessageBeep(0)
  end else
  begin
    Value := Value + FIncrement;
  end;
end;

procedure TplSpinEditFloat.DownClick(Sender: TObject);
begin
  if ReadOnly then
  begin
   // MessageBeep(0)
  end else
  begin
    Value := Value - FIncrement;
  end;
end;

procedure TplSpinEditFloat.CMExit(var Message: TLMessage);
begin
  inherited;
  if CheckValue(Value) <> Value then
    SetValue(Value);
end;

function TplSpinEditFloat.GetValue: Extended;
var
  s: string;
begin
  try
    s := Text;
    while Pos(CurrencyString, S) > 0 do
      Delete(S, Pos(CurrencyString, S), Length(CurrencyString));
    while Pos(' ', S) > 0 do
      Delete(S, Pos(' ', S), 1);
    while Pos(ThousandSeparator, S) > 0 do
      Delete(S, Pos(ThousandSeparator, S), Length(ThousandSeparator));

    //Delete negative numbers in format Currency
    if Pos('(', S) > 0 then
    begin
      Delete(S, Pos('(', S), 1);
      if Pos(')', S) > 0 then
        Delete(S, Pos(')', S), 1);
      Result := StrToFloat(S)*-1;
    end
    else
      Result := StrToFloat(S);
  except
    Result := FMinValue;
  end;
end;

procedure TplSpinEditFloat.SetFloatFormat(Value: TFloatFormat);
begin
  FFloatFormat := Value;
  Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TplSpinEditFloat.SetDigits(Value: Integer);
begin
  FDigits := Value;
  Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TplSpinEditFloat.SetPrecision(Value: Integer);
begin
  FPrecision := Value;
  Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TplSpinEditFloat.SetValue(Value: Extended);
begin
  Text := FloatToStrF(CheckValue(Value), FloatFormat, Precision, Digits);
end;

function TplSpinEditFloat.CheckValue(Value: Extended): Extended;
begin
  Result := Value;
  if (FMaxValue <> FMinValue) then
  begin
    if Value < FMinValue then
      Result := FMinValue
    else
      if Value > FMaxValue then
        Result := FMaxValue;
  end;
end;

procedure TplSpinEditFloat.CMEnter(var Message: TLMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

function TplSpinEditFloat.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight +3;
end;

procedure TplSpinEditFloat.SetBtnBounds;
begin
  FButton.SetBounds(Width - cnBtnWidth - cnBtnOffsetX, 1, cnBtnWidth , Height - cnBtnOffsetY); //button Height
end;


procedure TplSpinEditFloat.WMSize(var Message: TLMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight;

    if FButton <> nil then
    begin
      SetBtnBounds;
    end;
end;

procedure TplSpinEditFloat.Loaded;
begin
  inherited;
  SetBtnBounds;
end;

procedure TplSpinEditFloat.CreateWnd;
begin
  inherited;
  SetBtnBounds;
end;

end.

