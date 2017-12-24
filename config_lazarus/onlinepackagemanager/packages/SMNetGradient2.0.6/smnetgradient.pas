{ ----------------------------------------------------------------------------}
{ A Gradient Fill component for Delphi.                                       }
{ TGradientFill, Copyright 1995, Curtis White.  All Rights Reserved.          }
{ TNetGradient, Copyright 1997, Heiko Webers.  All Rights Reserved.           }
{ This component can be freely used and distributed in commercial and private }
{ environments, provided this notice is not modified in any way.               }
{ ----------------------------------------------------------------------------}
{ Feel free to contact me if you have any questions, comments or suggestions  }
{ at cwhite@teleport.com 						      }
{ Or me at heikowebers@usa.net                                                }
{ ----------------------------------------------------------------------------}
{ Date last modified:  18/10/2009                                             }
{ ----------------------------------------------------------------------------}
{ ----------------------------------------------------------------------------}
{ TNetGradient v2.05                                                          }
{ ----------------------------------------------------------------------------}
{ Description:                                                                }
{   A gradient fill like in the new Netscape Communicator Options Box.        }
{ Features:                                                                   }
{   The begin and end colors can be any colors.                               }
{   The fill direction can be set to Right-To-Left or Left-To-Right.          }
{   The number of colors, between 1 and 255 can be set for the fill.          }
{   The Caption can be anything and anywhere on TNetGradient.		      }
{ ----------------------------------------------------------------------------}
{ ----------------------------------------------------------------------------}
{ Revision History:                                                           }
{ 1.00:  Initial release                                                      }
{ 1.00:  Changed to TNetGradient                                              }
{ 1.01:  Border Update by Enzo Scozzaro www.scozzaro.it  www.thefox.it        }
{ 2.00:  +Caption Alignment, +Layout, +DataSource                             }
{ 2.01:  +SubCaption, +Font, +MarginLeft, +MarginTop, +SubCapField            }
{ 2.03:  -Bug TextLetf                                                        }
{ 2.04:  FillDirection: +ftTopToBottom, +ftBottomToTop                        }
{ 2.04:  +Begin/EndUpdate, Fix crash in frames, Fix memory leaks, Cleanup     }
{ ----------------------------------------------------------------------------}
 
unit SMNetGradient;

{$MODE Delphi}
 
interface
 
uses
   LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
   Dialogs, GraphType, Db, DBCtrls;

Const
  DefaultStyle     = DT_SINGLELINE or DT_END_ELLIPSIS or DT_EXPANDTABS;

type
  { Direction of fill }
  TFillDirection = (fdLeftToRight, fdRightToLeft, ftTopToBottom, ftBottomToTop);
  { Range of valid colors }
  TNumberOfColors = 1..255;
  TLabelBevel       = TBevelCut;

  TMargin           = 0..MaxInt;

  TNetGradient = class;

  TSubCaption = class(TPersistent)
  private
    { Private-Deklarationen }
    Parent:           TNetGradient;
    FCaption:         TCaption;
    FFont:            TFont;
    FHotTrack:        Boolean;
    FMarginLeft:      Integer;
    FMarginTop:       Integer;
    FVisible:         Boolean;
    procedure         OnFontChanged(Sender: TObject);
  protected
    { Protected declarations }
    procedure         SetCaption(const Value: TCaption); virtual;
    procedure         SetFont(Value: TFont); virtual;
    procedure         SetMarginLeft(Value: Integer); virtual;
    procedure         SetMarginTop(Value: Integer); virtual;
    procedure         SetVisible(Value: Boolean); virtual;
  public
    { Public declarations }
    constructor       Create(AOwner: TNetGradient); overload;
    destructor        Destroy; override;
  published
    { Published-Deklarationen }
    property          Caption: TCaption read FCaption write SetCaption;
    property          HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property          MarginLeft: Integer read FMarginLeft write SetMarginLeft default 5;
    property          MarginTop: Integer read FMarginTop write SetMarginTop default 0;
    property          Font: TFont read FFont write SetFont;
    property          Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TNetGradient }

  TNetGradient = class(TCustomControl)
  private
    //*** Enzo *** Bordi
    FBevelInner:      TLabelBevel;
    FBevelOuter:      TLabelBevel;

    //*** Emzp *** Allineamento Caption
    FAlignment  : TAlignment;
    FLayout     : TTextLayout;

    //*** Enzo *** Data Source - Field
    FDataLink : TFieldDataLink;

    //FMargin:          TMargin;

    { Variables for properties }
    FDirection: TFillDirection;
    FBeginColor: TColor;
    FEndColor: TColor;
    // FCenter: Boolean;
    FNumberOfColors: TNumberOfColors;
    FFont : TFont;
    FCaption : TCaption;
    FTextTop : Integer;
    FTextLeft: Integer;

    FSubCapField: Boolean;
    FSubCaption: TSubCaption;
    FUpdateCount: Integer;
    procedure OnFontChanged(Sender: TObject);
    procedure Changed;
    { Procedures for setting property values }
    procedure SetFillDirection(Value: TFillDirection);
    procedure SetBeginColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetNumberOfColors(Value: TNumberOfColors);
    procedure SetFont(AFont: TFont);
    procedure SetCaption(const Value: String);
    procedure SetTextTop(Value: Integer);
    procedure SetTextLeft(Value: Integer);
    { Fill procedure }
    procedure GradientFill;

    //*** Enzo *** Data Source

    function GetSubCapField: Boolean;
    Procedure SetSubCapField(Value:Boolean);

    procedure SetDataField (const Value : String);
    function GetDataField: String;

    function GetDataSource: TDataSource;
    procedure SetDataSource (Value: TDataSource);

    function GetField: TField;

    procedure DataChange(Sender: TObject);

  protected
    procedure Paint; override;

    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(Value: TTextLayout); virtual;

    procedure SetBevelInner(Value: TLabelBevel);
    procedure SetBevelOuter(Value: TLabelBevel);

   // procedure         DataChange(Sender:TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property CaptionAlignment: TAlignment read FAlignment write SetAlignment;
    property CaptionLayout: TTextLayout read FLayout write SetLayout default tlCenter;

    property BevelInner: TLabelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TLabelBevel read FBevelOuter write SetBevelOuter default bvRaised;

   { Starting color of fill }
    property BeginColor: TColor read FBeginColor write SetBeginColor
        default clBlue;
    { Ending color of fill }
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    { Direction of fill }
    property FillDirection: TFillDirection read FDirection write SetFillDirection default fdLeftToRight;
    { Number of colors to use in the fill (1 - 256) - default is 255.  If 1 }
    { then it uses the Begin Color.                                        }
    property NumberOfColors: TNumberOfColors read FNumberOfColors write SetNumberOfColors default 255;
    { Enable standard properties }
    property Font: TFont read FFont write SetFont;

    property Caption: String read FCaption write SetCaption;
    property TextTop: Integer read FTextTop write SetTextTop;
    property TextLeft: Integer read FTextLeft write SetTextLeft;
    property Align;
    property BorderSpacing;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    //*** Enzo ***
    property SubCapField: Boolean read FSubCapField write SetSubCapField default false;

    property DataField: String Read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property SubCaption: TSubCaption read FSubCaption;
  end;
 
implementation
 
{ TNetGradient }

{ Override the constructor to initialize variables }
constructor TNetGradient.Create(AOwner: TComponent);
begin
  { Inherit original constructor }
  inherited Create(AOwner);
  { Add new initializations }
  FLayout        := tlCenter;
  FBevelInner    := bvNone;
  FBevelOuter    := bvRaised;

  Height := 25;
  Width := 400;
  FBeginColor := clSilver;
  FEndColor := $00A56D39;
  FDirection := fdLeftToRight;
  FNumberOfColors:= 255;
  //FTextLeft:= 0;
  //FTextTop:= 0;
  FFont := TFont.Create;
  FFont.Style:= [fsbold];
  FFont.OnChange := OnFontChanged;

  FSubCaption  := TSubCaption.Create(Self);
  //Caption := Name;
  //FCaption := AOwner.Name;
  //ShowMessage((AOwner as TComponent).);
  //FCaption:= 'test';

  //*** Enzo ***
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  (*
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
  *)
end;

destructor TNetGradient.Destroy;
begin
  FSubCaption.Destroy;
  FFont.Destroy;
  FDataLink.Destroy;
  inherited Destroy;
end;

{ Set begin color when property is changed }
procedure TNetGradient.SetBeginColor(Value: TColor);
begin
  if Value <> FBeginColor then
  begin
    FBeginColor := Value;
    Changed;
  end;
end;
 
{ Set end color when property is changed }
procedure TNetGradient.SetEndColor(Value: TColor);
begin
  if Value <> FEndColor then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

{ Set the number of colors to be used in the fill }
procedure TNetGradient.SetNumberOfColors(Value: TNumberOfColors);
begin
  if Value <> FNumberOfColors then
  begin
    FNumberOfColors := Value;
    Changed;
  end;
end;
 
// Set the Font
procedure TNetGradient.SetFont(AFont: TFont);
begin
  if AFont <> FFont then
  begin
    FFont.Assign(AFont);
    Changed;
  end;
end;
 
// Set the Caption on NG
procedure TNetGradient.SetCaption(const Value: String);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed;
  end;
end;
 
// Set the Position of the Caption  (Top)
procedure TNetGradient.SetTextTop(Value: Integer);
begin
  if Value <> FTextTop then
  begin
    FTextTop := Value;
    Changed;
  end;
end;
 
// Set the Position of the Caption (Left)
procedure TNetGradient.SetTextLeft(Value: Integer);
begin
  if Value <> FTextLeft then
  begin
   FTextLeft := Value;
   Changed;
  end;
end;
 
{ Perform the fill when paint is called }
procedure TNetGradient.Paint;
begin
  GradientFill;
end;
 
{ Gradient fill procedure - the actual routine }
procedure TNetGradient.GradientFill;
var
  { Set up working variables }
  BeginRGBValue  : array[0..2] of Byte;    { Begin RGB values }
  RGBDifference  : array[0..2] of integer; { Difference between begin and end }
                                           { RGB values                       }
  ColorBand , rp,cr: TRect;    { Color band rectangular coordinates }
  I , x        : Integer;  { Color band index }
  R         : Byte;     { Color band Red value }
  G         : Byte;     { Color band Green value }
  B         : Byte;     { Color band Blue value }
  WorkBmp   : TBitmap;  { Off screen working bitmap }
  DrawStyle  : LongInt;
  TS         : TTextStyle;
  GlyphOffs  : Integer;
  //SbCapOffs  : Integer;

  procedure DoDrawText(const Text: string; ACanvas: TCanvas; AFont: TFont; var Rect: TRect; Flags: LongInt);
  begin
    with ACanvas do
    begin
      Font.Assign(AFont);
      Rect.Left:=Rect.Left;    //**
      if not(Enabled) then
      begin
        OffsetRect(Rect, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
        OffsetRect(Rect, -1, -1);   //**
        Font.Color := clBtnShadow;
        DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
      end
      else
        DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
    end;
  end;

begin
  { Create the working bitmap and set its width and height }
  WorkBmp := TBitmap.Create;
  WorkBmp.Width := Width;
  WorkBmp.Height := Height;

{ Use working bitmap to draw the gradient }
with WorkBmp do
begin
  Rp := GetClientRect;
  { Extract the begin RGB values }
  case FDirection of
 
     fdLeftToRight,ftTopToBottom:   begin
        { Set the Red, Green and Blue colors }
        BeginRGBValue[0] := GetRValue (ColorToRGB (FBeginColor));
        BeginRGBValue[1] := GetGValue (ColorToRGB (FBeginColor));
        BeginRGBValue[2] := GetBValue (ColorToRGB (FBeginColor));
        { Calculate the difference between begin and end RGB values }
        RGBDifference[0] := GetRValue (ColorToRGB (FEndColor)) -
                            BeginRGBValue[0];
        RGBDifference[1] := GetGValue (ColorToRGB (FEndColor)) -
                            BeginRGBValue[1];
        RGBDifference[2] := GetBValue (ColorToRGB (FEndColor)) -
                            BeginRGBValue[2];
      end;
 
      fdRightToLeft,ftBottomToTop:    begin
        { Set the Red, Green and Blue colors }
        BeginRGBValue[0] := GetRValue (ColorToRGB (FEndColor));
        BeginRGBValue[1] := GetGValue (ColorToRGB (FEndColor));
        BeginRGBValue[2] := GetBValue (ColorToRGB (FEndColor));
        { Calculate the difference between begin and end RGB values }
        RGBDifference[0] := GetRValue (ColorToRGB (FBeginColor)) -
                            BeginRGBValue[0];
        RGBDifference[1] := GetGValue (ColorToRGB (FBeginColor)) -
                            BeginRGBValue[1];
        RGBDifference[2] := GetBValue (ColorToRGB (FBeginColor)) -
                            BeginRGBValue[2];
      end;
  end;
 
  { Set the pen style and mode }
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode := pmCopy;
 
  case FDirection of
 
   { Calculate the color band's left and right coordinates }
   { for LeftToRight and RightToLeft fills }
    fdLeftToRight, fdRightToLeft:
      begin
        ColorBand.Top := 0;
        ColorBand.Bottom := Height;
      end;
    ftTopToBottom, ftBottomToTop:
      begin
        ColorBand.Left := 0;
        ColorBand.Right := Width;
      end;

  end;
 
  { Perform the fill }
  for I := 0 to FNumberOfColors do
    begin
    case FDirection of
 
     { Calculate the color band's left and right coordinates }
      fdLeftToRight, fdRightToLeft:
        begin
          ColorBand.Left    := MulDiv (I    , Width, FNumberOfColors);
          ColorBand.Right := MulDiv (I + 1, Width, FNumberOfColors);
        end;
        ftTopToBottom, ftBottomToTop:
      begin
        ColorBand.Top := MulDiv (I    , Height, FNumberOfColors);
        ColorBand.Bottom :=  MulDiv (I + 1, Height, FNumberOfColors);
      end;
    end;
 
    { Calculate the color band's color }
    if FNumberOfColors > 1 then
    begin
      R := BeginRGBValue[0] + MulDiv (I, RGBDifference[0], FNumberOfColors - 1);
      G := BeginRGBValue[1] + MulDiv (I, RGBDifference[1], FNumberOfColors - 1);
      B := BeginRGBValue[2] + MulDiv (I, RGBDifference[2], FNumberOfColors - 1);
    end
    else
    { Set to the Begin Color if set to only one color }
    begin
      R := BeginRGBValue[0];
      G := BeginRGBValue[1];
      B := BeginRGBValue[2];
    end;
 
    { Select the brush and paint the color band }
    Canvas.Brush.Color := RGB (R, G, B);
    Canvas.FillRect (ColorBand);
    end;
  end;
 
  { Copy the working bitmap to the main canvas }
  Canvas.Draw(0, 0, WorkBmp);
 
  // <TextOut>
   Canvas.Brush.Style:= bsClear;
   Canvas.Font.Assign(FFont);

   //Canvas.Textout(FTextLeft, FTextTop, FCaption);   *** Enzo *** Implemetation

   Canvas.Frame3D(Rp, 1,FBevelOuter);
   Canvas.Frame3d(rp, 1, FBevelInner);

    if Caption <> '' then  begin
            Font.Assign(Self.Font);
            Rp := GetClientRect;
            Inc(Rp.Left, FTextLeft);  //5=FMargin
            //if FGlyph.DisplayAlign = daLeft then
            //  Inc(Rp.Left, GlyphOffs)
            //else
            //  Dec(R.Right, GlyphOffs);
            //Dec(R.Right, SbCapOffs);
            case FAlignment of
              taLeftJustify : begin
                    DrawStyle := DefaultStyle or  DT_LEFT;
                     Inc(Rp.Left, FTextLeft);
              end;
              taRightJustify : begin
                    DrawStyle := DefaultStyle or  DT_RIGHT;
                    Inc(Rp.Right, FTextLeft);

              end;
              taCenter : begin
              DrawStyle := DefaultStyle or  DT_CENTER;
              Inc(Rp.Left, FTextLeft);
              end;
            end;
            //DrawStyle := DefaultStyle or FAlignment; //Enzo DT_LEFT;

            TS := Canvas.TextStyle;
            TS.Alignment:= CaptionAlignment;
            //TS.Layout:= tlCenter;
            TS.Opaque:= false;
            TS.Clipping:= false;
            TS.SystemFont:=Canvas.Font.IsDefault;

            CR := Rp;
            //cr.left := cr.Left +  FTextLeft;
            //rp.left :=rp.left + FTextLeft;
            rp.Top:= rp.Top + FTextTop;
            if FLayout <> tlTop then begin

                X := Canvas.TextHeight('W');
                DoDrawText(Caption, WorkBmp.Canvas, Self.Font, CR, DrawStyle or DT_CALCRECT);
                if FLayout = tlBottom then OffsetRect(Rp, 0, Height - CR.Bottom-2)
                else OffsetRect(Rp, 0, (Height - CR.Bottom) div 2);
              end;
           // if Rp.Right - Rp.Left >= Canvas.TextWidth(Caption[1]) then
           Canvas.TextRect(rp, rp.Left, rp.Top, Caption, TS);


        end;


   //*** Enzo **** SubCaption

   if (FSubCaption.Caption <> '') then  begin
      GlyphOffs := 0;
      //SbCapOffs := 0;
      Canvas.Font.Assign(FSubCaption.Font);
      Rp := GetClientRect;

      //DrawStyle := DefaultStyle or  DT_RIGHT;

      // if FLayout <> tlTop then begin
      // rp:=GetClientRect;
      X := canvas.TextHeight('W');

      Inc(Rp.Left, ClientWidth - canvas.TextWidth(FSubCaption.Caption) - FSubCaption.MarginLeft);
      if Rp.Left < SubCaption.FMarginLeft   + GlyphOffs then    Rp.Left := SubCaption.FMarginLeft   + GlyphOffs;
      //SbCapOffs := Rp.Right - Rp.Left;
      CR := Rp;
      Inc(cr.Left, SubCaption.FMarginLeft);
      rp.Top:= rp.Top+SubCaption.FMarginTop;
      DoDrawText(FSubCaption.Caption, WorkBmp.Canvas, Self.Font, CR, DT_CALCRECT);
      if FLayout = tlBottom then begin
         OffsetRect(Rp, 0, Height - X - Abs(canvas.TextHeight('W') - X) div 2);
      end else if FLayout = tlTop then begin
         Font.Assign(Self.Font);
      end else
         OffsetRect(Rp, 0, (Height - CR.Bottom) div 2);
      Canvas.TextRect(rp, rp.Left, rp.Top, FSubCaption.Caption);
      end;


   // Border Outer and Inner


   // Canvas.Frame3d(rp, 1, bvRaised);
  // </TextOut>
 
  { Release the working bitmap resources }
  WorkBmp.Free;
end;
 
{ Set the fill direction }
procedure TNetGradient.SetFillDirection(Value: TFillDirection);
begin
  if Value <> FDirection then
  begin
    FDirection := Value;
    Changed;
  end;
end;

//*** Enzo ***

procedure TNetGradient.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TNetGradient.SetLayout(Value: TTextLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TNetGradient.SetBevelInner(Value: TLabelBevel);
begin
  if Value <> FBevelInner then  begin
    FBevelInner := Value;
    Changed;
  end;
end;

procedure TNetGradient.SetBevelOuter(Value: TLabelBevel);
begin
  if Value <> FBevelOuter then
  begin
    FBevelOuter := Value;
    Changed;
  end;
end;

//*** Enzo ***

function TNetGradient.GetSubCapField: Boolean;
begin
  Result := FSubCapField;
end;

procedure TNetGradient.SetSubCapField(Value: Boolean);
begin
  FSubCapField := Value;
end;

function TNetGradient.GetDataField: STring;
begin
  Result := FDataLink.FieldName;
end;

procedure TNetGradient.SetDataField (const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TNetGradient.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TNetGradient.SetDataSource (Value: TDataSource);

procedure ChangeDataSource(AControl: TControl; Link: TDataLink;
  NewDataSource: TDataSource);
begin
  if Link.DataSource=NewDataSource then exit;
  if Link.DataSource<>nil then
    Link.DataSource.RemoveFreeNotification(AControl);
  Link.DataSource:=NewDataSource;
  if Link.DataSource<>nil then
    Link.DataSource.FreeNotification(AControl);
end;

begin
//*     enzo
  //FDataLink.DataSource := Value;
  ChangeDataSource(Self, FDataLink, Value);
  // useless                           e
  {if Value <> nil then
    Value.FreeNotification (Value);}
end;

function TNetGradient.GetField: TField;
begin
  Result := FDataLink.Field;
end;

// data link event handler
procedure TNetGradient.DataChange (Sender: TObject);
begin
  if FDataLink.DataSet.Active = true then begin
     //enzo
     if FSubCapField = False Then begin
        Caption := FDataLink.Field.AsString;
     end else begin
       fSubCaption.SetCaption( FDataLink.Field.DisplayText);
       //fSubCaption.SetCaption( FDataLink.Field.AsString);
     end;
  end;
end;

procedure TNetGradient.OnFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TNetGradient.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TNetGradient.Changed;
begin
  if (FUpdateCount = 0) and not (csLoading in ComponentState) then
  begin
    Invalidate;
  end;
end;

procedure TNetGradient.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  Changed;
end;

//**********

constructor TSubCaption.Create(AOwner: TNetGradient);
begin
  inherited Create;
  Parent         := AOwner;
  //FCaption       := '';
  FFont          := TFont.Create;
  FFont.OnChange := OnFontChanged;
  //FHotTrack      := False;
  FMarginLeft        := 5;
  //FMarginTop       := 0;
  FVisible       := True;
end;

destructor TSubCaption.Destroy;
begin
  FFont.Destroy;
  inherited Destroy;
end;    // Destroy

procedure TSubCaption.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Parent.Changed;
  end
end;

procedure TSubCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSubCaption.OnFontChanged(Sender: TObject);
begin
  Parent.Changed;
end;

procedure TSubCaption.SetMarginLeft(Value: Integer);
begin
  if Value <> FMarginLeft then
  begin
    FMarginLeft := Value;
    Parent.Changed;
  end;
end;

procedure TSubCaption.SetMarginTop(Value: Integer);
begin
  if Value <> FMarginTop then
  begin
    FMarginTop := Value;
    Parent.Changed;
  end;
end;

procedure TSubCaption.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Parent.Changed;
  end;
end;

end.
