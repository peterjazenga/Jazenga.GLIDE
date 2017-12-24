{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplComboBoxFontUnit;

{$MODE Delphi}

{$R TplComboBoxFontUnit.res}

interface

uses
  LCLIntf, LCLType, Messages, LMessages,
  Classes, Forms, Controls, Graphics, StdCtrls, plUtils,
  SysUtils, comctrls ,dialogs, TplComboBoxUnit;

type

TplFontComboBox = class(TplCustomComboBox)
  private
   FGlyph: TBitmap;
   FPreview: boolean;
   FSelection: TFontName;
   FDrawLabel: TDrawItemLabelEvent;
   procedure SetPreview(p: boolean);
   function  GetSelection:TFontName;
   procedure SetSelection(s: TFontName);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   procedure FontChanged(Sender: TObject);
   procedure EnabledChanged;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property ShowPreview: boolean read FPreview write SetPreview default true;
   property Selection: TFontName read GetSelection write SetSelection;
   property OnDrawItemLabel: TDrawItemLabelEvent read FDrawLabel write FDrawLabel;

  end;

implementation  

//================ TplFontComboBox ===========================================

constructor TplFontComboBox.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //DoubleBuffered := true;
 Style := csOwnerDrawFixed;
 FGlyph := TBitmap.Create;
 FGlyph.Handle := LoadBitmap(HInstance, 'CBFONTDEFAULT');
 FPreview := true;
 Font.OnChange := FontChanged;
 //ParentColor := false;
 Style := csOwnerDrawFixed;
end;

destructor TplFontComboBox.Destroy;
begin
 FGlyph.Free;
 inherited Destroy;
end;

procedure TplFontComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R, IR: TRect;
 NewFont: TFont;
begin

  if (csDesigning in ComponentState) then Exit;

if Items.Count=0 then exit;
if index<0 then exit;
if index>self.Items.Count-1 then exit;

 Canvas.Font := Font;
 if odSelected in State then
  begin
   Canvas.Brush.Color := clHighlight;
   Canvas.Pen.Color := clHighlightText;
   Canvas.Font.Color := clHighlightText;
  end
 else
  begin
   Canvas.Brush.Color := Color;
   Canvas.Pen.Color := clWindowText;
   Canvas.Font.Color := clWindowText;
  end;
 Canvas.FillRect(Rect);
 // set icon rect
 R := Rect;
 R.Left := 4;
 R.Right := R.Left + FGlyph.Width;
 R.Top := R.Top + ((R.Bottom - R.Top) - FGlyph.Height) div 2;
 R.Bottom := R.Top + FGlyph.Height;
 FGlyph.Transparent := true;
 Canvas.Draw(R.Left, R.Top, FGlyph);
 //DrawGlyph(Canvas.Handle, R.Left, R.Top);
 // draw text
 Canvas.Brush.Style := bsClear;
 Canvas.Font := Font;
 if FPreview then
  begin
   NewFont := TFont.Create;
   Canvas.Font := NewFont; // needed to fix bug when some symbol fonts are used (wingdings, webdings, ...)
   Canvas.Font.Name := Items.Strings[Index];
   NewFont.Free;
  end;
 IR := Rect;
 IR.Left := R.Right + 4;
 Canvas.Font.Color := clWindowText;
 if odSelected in State then
  Canvas.Font.Color := clHighlightText;
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 DrawText(Canvas.Handle, PChar(Items.Strings[index]), Length(Items.Strings[index]), IR, DT_VCENTER or DT_SINGLELINE);
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TplFontComboBox.CreateWnd;
var c: integer;
begin
 inherited CreateWnd;
 Items.Clear;
 //Screen.ResetFonts;

 for c:=0 to Screen.Fonts.Count - 1 do
  Items.Add(Screen.Fonts.Strings[c]);

 SetSelection('Times New Roman');
end;

procedure TplFontComboBox.SetPreview(p: boolean);
begin
 FPreview := p;
 Invalidate;
end;

function TplFontComboBox.GetSelection:TFontName;
begin
 result:='Times New Roman';
 if (csDesigning in ComponentState) then Exit;  // ct9999

 result:=TFontName(Items.Strings[ItemIndex]);
end;

procedure TplFontComboBox.SetSelection(s: TFontName);
var
 i: integer;
begin
 FSelection := s;
 if s = '' then
  ItemIndex := -1
 else
  for i := 0 to Items.Count - 1 do
   if SameText(s, Items.Strings[i]) then
    begin
     ItemIndex := i;
     Exit;
    end;
end;

procedure TplFontComboBox.FontChanged(Sender: TObject);
begin
 if not FPreview then Invalidate;
end;

procedure TplFontComboBox.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.

