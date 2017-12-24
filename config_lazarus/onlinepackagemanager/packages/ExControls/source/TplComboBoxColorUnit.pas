{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplComboBoxColorUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Messages, LMessages,
  Classes, Forms, Controls, Graphics, StdCtrls, plUtils,
  SysUtils, comctrls ,dialogs, TplComboBoxUnit;

type

TplColorComboBox = class(TplCustomComboBox)
  private
    FShowNames: Boolean;
    FSelection: TColor;
    FColorBoxWidth: Integer;
    FColorDlg: TColorDialog;
    procedure SetShowNames(Value: Boolean);
    procedure SetSelection(Value: TColor);
    procedure SetColorBoxWidth(Value: Integer);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CreateWnd; override;
    procedure Click; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function AddColor(ColorName: String; Color: TColor): Boolean;
    function DeleteColorByName(ColorName: String): Boolean;
    function DeleteColorByColor(Color: TColor): Boolean;
  published
    property BoxWidth: Integer read FColorBoxWidth write SetColorBoxWidth;
    property ShowNames: Boolean read FShowNames write SetShowNames;
    property Selection: TColor read FSelection write SetSelection;

  end;

implementation   

//====================== TplColorComboBox =============================
const
  StdColors = 16;
  StdColorValues: array [1..StdColors] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

constructor TplColorComboBox.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  TControlCanvas(Canvas).Control := Self;

  FShowNames := True;
  FColorBoxWidth := 12;
  FSelection := clBlack;
  FColorDlg := TColorDialog.Create(Self);

  Style := csOwnerDrawFixed;
end;

destructor TplColorComboBox.Destroy;
begin
  FColorDlg.Free;
  inherited;
end;

procedure TplColorComboBox.CreateWnd;
var
  I: Integer;
  ColorName: string;
begin
  inherited CreateWnd;
  Clear;
  for I := 1 to StdColors do
  begin
    ColorName := Copy(ColorToString(StdColorValues[I]), 3, 40);
    Items.AddObject(ColorName, TObject(StdColorValues[I]));
  end;
  Items.AddObject('Custom',TObject(clBlack));
  ItemIndex := 0;
  Change;
end;


procedure TplColorComboBox.SetShowNames(Value: Boolean);
begin
 if Value <> FShowNames then
  begin
   FShowNames := Value;
   Invalidate;
  end;
end;

procedure TplColorComboBox.SetSelection(Value: TColor);
var
 Item: Integer;
 CurrentColor: TColor;
begin
 if (ItemIndex < 0) or (Value <> FSelection) then
  begin
   for Item := 0 to Pred(Items.Count) do
    begin
     CurrentColor := TColor(Items.Objects[Item]);
      if CurrentColor = Value then
       begin
        FSelection := Value;
         if ItemIndex <> Item then ItemIndex := Item;
        Change;
        Break;
       end;
    end;
  end;
end;

procedure TplColorComboBox.SetColorBoxWidth(Value: Integer);
begin
 if Value <> FColorBoxWidth then
  begin
   FColorBoxWidth := Value;
  end;
 Invalidate;
end;

procedure TplColorComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  ARect := Rect;
  Inc(ARect.Top, 2);
  Dec(ARect.Bottom, 2);

  Inc(ARect.Left, 2);
  Dec(ARect.Right, 2);

  if FShowNames then
  begin
    ARect.Right := ARect.Left + FColorBoxWidth;
  end
  else
  begin
    Dec(ARect.Right, 3);
  end;
  with Canvas do
  begin
    Safer := Brush.Color;
    if (odSelected in State) then
    begin
      Canvas.Brush.Color := FHighlightColor;
      FillRect(Rect);
      Pen.Color := clBlack;
      Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      FillRect(Rect);
      Pen.Color := clBlack;
      Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    end;
    Brush.Color := ColorToRgb(TColor(Items.Objects[Index]));
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect)
    finally
      Brush.Color := Safer;
    end;
    if FShowNames then
    begin
      StrPCopy(Text, Items[Index]);
      Rect.Left := ARect.Right + 5;
      Brush.Style := bsClear;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      Brush.Style := bsSolid;
    end;
  end;
end;

procedure TplColorComboBox.Click;
begin
  if ItemIndex >= 0 then
  begin
    if Items[ItemIndex] = 'Custom' then
    begin
      if not FColorDlg.Execute then    Exit;
      Items.Objects[ItemIndex] := TObject(FColorDlg.Color);
    end;
    Selection := TColor(Items.Objects[ItemIndex]);
  end;
  inherited Click;
end;

function TplColorComboBox.AddColor(ColorName: String; Color: TColor): Boolean;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if UpperCase(ColorName) = UpperCase(Items[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Items.InsertObject(Items.Count - 1, ColorName, TObject(Color));
  Result := True;
end;

function TplColorComboBox.DeleteColorByName(ColorName: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if UpperCase(ColorName) = UpperCase(Items[I]) then
    begin
      Items.Delete(I);
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TplColorComboBox.DeleteColorByColor(Color: TColor): Boolean;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if Color = TColor(Items.Objects[I]) then
    begin
      Items.Delete(I);
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

end.

