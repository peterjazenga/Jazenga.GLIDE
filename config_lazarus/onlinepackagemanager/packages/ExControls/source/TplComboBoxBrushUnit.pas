{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplComboBoxBrushUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Messages, LMessages,
  Classes, Forms, Controls, Graphics, StdCtrls, plUtils,
  SysUtils, comctrls ,dialogs, TplComboBoxUnit;

type

TplBrushComboBox = class(TplCustomComboBox)
  private
   FDrawLabel: TDrawItemLabelEvent;
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   function  GetSelection: TBrushStyle;
   procedure SetSelection(Value: TBrushStyle);
   procedure EnabledChanged;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Selection: TBrushStyle read GetSelection write SetSelection default bsSolid;
   property OnDrawItemLabel: TDrawItemLabelEvent read FDrawLabel write FDrawLabel;

  end;

implementation   

//===================== TplBrushComboBox ===================================

const BrushStyleArray: array[0..7] of string = ('Solid', 'Clear', 'Horizontal',
             'Vertical', 'Diagonal Right', 'Diagonal Left', 'Cross', 'Diagonal Cross');

constructor TplBrushComboBox.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //ParentColor := false;
 //DoubleBuffered := true;
 Style := csOwnerDrawFixed;
end;

destructor TplBrushComboBox.Destroy;
begin
 inherited Destroy;
end;

function TplBrushComboBox.GetSelection: TBrushStyle;
begin
 Result := TBrushStyle(ItemIndex);
end;

procedure TplBrushComboBox.SetSelection(Value: TBrushStyle);
var
 i: integer;
begin
 for i:=0 to 7 do
  if TBrushStyle(i) = Value then
   begin
    ItemIndex := i;
    Exit;
   end;
end;

procedure TplBrushComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R, IR: TRect;
 tc: TColor;
begin
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
 Inc(R.Left, 1);
 Inc(R.Top, 1);
 Dec(R.Bottom, 1);
 R.Right := R.Left + (R.Bottom - R.Top);
 // draw icon
 Canvas.Brush.Color := clWindowText;
 Canvas.Brush.Style := TBrushStyle(Index);
 tc := Canvas.Font.Color;
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 if Canvas.Font.Color <> tc then
  Canvas.Brush.Color := Canvas.Font.Color;
 Canvas.Rectangle(R);
 // draw text
 Canvas.Brush.Style := bsClear;
 IR := Rect;
 IR.Left := R.Right + 4;
 DrawText(Canvas.Handle, PChar(Items.Strings[index]), Length(Items.Strings[index]), IR, DT_VCENTER or DT_SINGLELINE);
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TplBrushComboBox.CreateWnd;
var
 c: integer;
begin
 inherited CreateWnd;
 Items.Clear;
 for c := 0 to High(BrushStyleArray) do
  Items.Add(BrushStyleArray[c]);
 SetSelection(bsSolid);
end;

procedure TplBrushComboBox.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.

