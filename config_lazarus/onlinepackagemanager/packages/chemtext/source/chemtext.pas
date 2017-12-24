(*******************************************************************************

                             chemtext.pas

 Motivated by chemtxt written by Patrick Spanel (Patrik.Spanel@jh-inst.cas.cz)
 Download his version from
   http://torry.net/vcl/science/packs/ChemText12.zip
 or
   http://delphi.icm.edu.pl/ftp/d10free/chemtxt.zip

 Adapted to Lazarus and extended by Werner Pamler

 License:
 LGPL with linking exception (like Lazarus)
 See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
 for details about the license.

*******************************************************************************)

unit chemtext;

interface

uses
  LclIntf, LCLType, Types, SysUtils, Classes, Graphics, StdCtrls;

type
  TChemArrow = (
    caASCIISingle, caASCIIDouble, caUTF8, caUTF8Single, caUTF8Double, caUTF8Half
  );

  TChemLabel = class(TCustomLabel)
  private
    FArrow: TChemArrow;
    procedure SetArrow(const AValue: TChemArrow);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
    procedure CalculateSize(out NeededWidth, NeededHeight: Integer);
    procedure DoMeasureTextPosition(var TextTop: integer;
      var TextLeft: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Arrow: TChemArrow read FArrow write SetArrow default caASCIISingle;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
//    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
//    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
//    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
//    property WordWrap;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
//    property OptimalFill;
  end;


{ The following rotuines can be used in an event handler, for example in
  OnDrawDataCell of DBGrid:

  procedure TForm1.DBGrid1DrawDataCell(Sender: TObject; const Rect: TRect;
    Field: TField; State: TGridDrawState);
  begin
    if Assigned(Field) then
      ChemTextOut((Sender as TDBGrid).Canvas, Rect.Left, Rect.Top, Field.DisplayText);
  end;
}

function ChemTextOut(ACanvas: TCanvas; X, Y: integer;
  const AText:String; Arrow: TChemArrow = caAsciiSingle; Measure: Boolean = false): TSize;

function ChemTextHeight(ACanvas: TCanvas; const AText: String;
  Arrow: TChemArrow = caAsciiSingle): Integer;

function ChemTextWidth(ACanvas: TCanvas; const AText: String;
  Arrow: TChemArrow = caAsciiSingle): Integer;

function ChemTextExtent(ACanvas: TCanvas; const AText: String;
  Arrow: TChemArrow = caAsciiSingle): TSize;

procedure Register;


implementation

{$R chemtext.res}

uses
  Themes, Math;

type
  TArrowDir = (adLeft, adRight, adBoth);

const
  SUBFONT_SIZE_MULTIPLIER = 75;
  SUBFONT_OFFSET_MULTIPLIER = 50;
  SUBFONT_DIVISOR = 100;
  ARROW_LINE: array[boolean] of char = ('-', '=');
  ESCAPE_CHAR = '\';

function ChemTextHeight(ACanvas: TCanvas; const AText: String;
  Arrow: TChemArrow = caAsciiSingle): Integer;
var
  ex: TSize;
begin
  ex := ChemTextExtent(ACanvas, AText, Arrow);
  Result := ex.CY;
end;

function ChemTextWidth(ACanvas: TCanvas; const AText: String;
  Arrow: TChemArrow = caAsciiSingle): Integer;
var
  ex: TSize;
begin
  ex := ChemTextExtent(ACanvas, AText, Arrow);
  Result := ex.CX;
end;

function ChemTextExtent(ACanvas: TCanvas; const AText: String;
  Arrow: TChemArrow = caAsciiSingle): TSize;
begin
  Result := ChemTextOut(ACanvas, 0, 0, AText, Arrow, true);
end;

function ChemTextOut(ACanvas: TCanvas;  X, Y:integer; const AText: String;
  Arrow: TChemArrow = caAsciiSingle; Measure: Boolean = false): TSize;
var
  lTextHeight: Integer;

  procedure DrawSub(var x: Integer; y: Integer; const s: String);
  var
    h: Integer;
    yoff: Integer;
  begin
    h := ACanvas.Font.Height;
    try
      ACanvas.Font.Height := MulDiv(h, SUBFONT_SIZE_MULTIPLIER, SUBFONT_DIVISOR);
      yoff := abs(MulDiv(h, SUBFONT_OFFSET_MULTIPLIER, SUBFONT_DIVISOR));
      if not Measure then
        ACanvas.TextOut(x, y + yoff, s);
      x := x + ACanvas.TextWidth(s);
      lTextHeight := Max(lTextHeight, yoff + ACanvas.TextHeight('0'));
    finally
      ACanvas.Font.Height := h;
    end;
  end;

  procedure DrawSup(var x: Integer; y: Integer; const s: String);
  var
    h: Integer;
  begin
    h := ACanvas.Font.Height;
    try
      ACanvas.Font.Height := MulDiv(h, SUBFONT_SIZE_MULTIPLIER, SUBFONT_DIVISOR);
      if not Measure then
        ACanvas.TextOut(x, y - 1, s);
      inc(x, ACanvas.TextWidth(s));
    finally
      ACanvas.Font.Height := h;
    end;
  end;

  procedure DrawNormal(var x: Integer; y: Integer; const s: String);
  begin
    if not Measure then
      ACanvas.TextOut(x, y, s);
    inc(x, ACanvas.TextWidth(s));
  end;

  procedure DrawArrow(var x: Integer; y: Integer; ADir: TArrowDir;
    ALen: Integer);
  const
    ARROWS: array[TChemArrow, TArrowDir] of string = (
      ('<%s', '%s>', '<%s>'),                      // caAsciiSingle
      ('<%s', '%s>', '<%s>'),                      // caAsciiDouble
      (#$E2#$86#$90, #$E2#$86#$92, #$E2#$87#$8C),  // caUTF8          ← → ⇌
      (#$E2#$86#$90, #$E2#$86#$92, #$E2#$86#$94),  // caUTF8Single    ← → ↔
      (#$E2#$87#$90, #$E2#$87#$92, #$E2#$87#$94),  // caUTF8Double    ⇐ ⇒  ⇔  ⇔
      (#$E2#$86#$BD, #$E2#$87#$80, #$E2#$87#$8C)   // caUTF8Half      ↽ ↼ ⇌
    );
  var
    i: Integer;
    s: String;
  begin
    if Arrow in [caASCIISingle, caASCIIDouble] then
    begin
      SetLength(s, ALen);
      for i:=1 to ALen do s[i] := ARROW_LINE[Arrow=caAsciiDouble];
      s := Format(ARROWS[Arrow, ADir], [s]);
    end else
      s := ARROWS[Arrow, ADir];

    if not Measure then
      ACanvas.TextOut(x, y, s);
    inc(x, ACanvas.TextWidth(s));
  end;

var
  x0: Integer;
  i, j: integer;
  s: string;
  subNos: boolean;        // "subscript numbers"
  escaping: Boolean;
begin
  Result := Size(0, 0);
  if AText = '' then
    exit;

  with ACanvas do begin
    if Font.Size = 0 then
      Font.Size := GetFontData(Font.Reference.Handle).Height;

    lTextHeight := TextHeight('Tg');

    x0 := X;
    subNos := false;
    escaping := false;
    i := 1;
    while i <= Length(AText) do begin
      if escaping then
      begin
        DrawNormal(X, Y, AText[i]);
        escaping := false;
      end else
        case AText[i] of
          '0'..'9':
            begin
              s := AText[i];
              j := i+1;
              while (j <= Length(AText)) and (AText[j] in ['0'..'9']) do
                inc(j);
              s := Copy(AText, i, j-i);
              if subNos then
                DrawSub(X, Y, s)
              else
                DrawNormal(X, Y, s);
              i := j-1;
              subNos := false;
            end;

          '<':
            begin
              j := i+1;
              while (j <= Length(AText)) and (AText[j] in ['-', '=']) do
                inc(j);
              if (AText[j] = '>') then
                DrawArrow(X, Y, adBoth, j-i-1)
              else begin
                DrawArrow(X, Y, adLeft, j-i-1);
                dec(j);
              end;
              i := j;
              subNos := false;
            end;

          '+':
            begin
              if (i > 1) and (AText[i-1] in ['A'..'Z','a'..'z','0'..'9','+',')']) then
                DrawSup(X, Y, '+')
              else
                DrawNormal(X, Y, '+');
              subNos := false;
            end;

          '-':
            begin
              begin
                j := i+1;
                while (j <= Length(AText)) and (AText[j] = '-') do inc(j);
                if (j <= Length(AText)) and (AText[j] = '>') then  // Arrow
                begin
                  DrawArrow(X, y, adRight, j-i);
                  i := j;
                end else                  // superscript -
                  DrawSup(X, Y, '-');
              end;
              subNos := false;
            end;

          ESCAPE_CHAR:
            escaping := true;

          else
            begin
              j := i+1;
              while (j <= Length(AText)) and
                    not (AText[j] in ['0'..'9', '+', '-', '<', ESCAPE_CHAR])
              do
                inc(j);
              s := Copy(AText, i, j-i);
              DrawNormal(X, Y, s);
              i := j-1;
              subNos := AText[i] in ['A'..'Z', 'a'..'z', ')'];
                // In these cases a subsequent number will be subscripted.
            end;
        end;
      inc(i);
    end;
  end;

  Result.CX := X - x0;
  Result.CY := lTextHeight;
end;


{ TChemText }

constructor TChemLabel.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TChemLabel.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  CalculateSize(PreferredWidth, PreferredHeight);
end;

procedure TChemlabel.CalculateSize(out NeededWidth, NeededHeight: Integer);
var
  ex: TSize;
begin
  Canvas.Font := Font;
  ex := ChemTextExtent(Canvas, Caption, FArrow);
  NeededWidth := ex.CX;
  NeededHeight := ex.CY;
end;

procedure TChemLabel.DoMeasureTextPosition(var TextTop: integer;
  var TextLeft: integer);
var
  lTextHeight: integer;
  lTextWidth: integer;
begin
  TextLeft := 0;
  TextTop := 0;
  if (Alignment <> taLeftJustify) or (Layout <> tlTop) then begin
    CalculateSize(lTextWidth, lTextHeight);
    case Alignment of
      taCenter       : TextLeft := (Width - lTextWidth) div 2;
      taRightJustify : TextLeft := Width - lTextWidth;
    end;
    case Layout of
      tlCenter       : TextTop := (Height - lTextHeight) div 2;
      tlBottom       : TextTop := Height - lTextHeight;
    end;
  end;
end;

procedure TChemLabel.Paint;
var
  textTop: Integer = 0;
  textLeft: Integer = 0;
  oldFontColor: TColor;
  labelText: String;
begin
  if not Transparent then
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Font;

  labelText := Caption;
  DoMeasureTextPosition(textTop, textLeft);

  oldFontColor := Font.Color;
  if not IsEnabled then
    if ThemeServices.ThemesEnabled then
      Canvas.Font.Color := clGrayText
    else
    begin
      Canvas.Font.Color := clBtnHighlight;
      ChemTextOut(Canvas, textLeft + 1, textTop + 1, labelText, FArrow);
      Canvas.Font.Color := clBtnShadow;
    end;
  ChemTextOut(Canvas, textLeft, textTop, labelText, FArrow);
  Canvas.Font.Color := oldFontColor;
end;

procedure TChemLabel.SetArrow(const AValue: TChemArrow);
begin
  if AValue = FArrow then
    exit;
  FArrow := AValue;
  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Misc', [TChemlabel]);
end;

end.
